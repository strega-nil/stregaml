module Spanned = Cafec_spanned
module Untyped_ast = Cafec_parse.Ast
module Error = Error
module Expr = Expr
open Spanned.Prelude
open Error.Monad_spanned

type 'a monad = 'a Error.Monad_spanned.t

type decl = {name: string; params: (string * Type.t) list; ret_ty: Type.t}

type t = {func_decls: decl spanned list; func_exprs: Expr.t spanned list}

module Types : sig
  val get : t -> Untyped_ast.Type.t spanned -> Type.t monad
end = struct
  let get _ctxt unt_ty =
    let module T = Untyped_ast.Type in
    let unt_ty, sp = unt_ty in
    let%bind (), _ = with_span sp in
    match unt_ty with T.Named name ->
      match name with
      | "unit" -> wrap Type.Unit
      | "bool" -> wrap Type.Bool
      | "int" -> wrap Type.Int
      | _ -> wrap_err (Error.Type_not_found unt_ty)
end

module Functions : sig
  val index_by_name : t -> string -> int option

  val decl_by_index : t -> int -> decl spanned

  val expr_by_index : t -> int -> Expr.t spanned
end = struct
  let index_by_name {func_decls; _} search =
    let rec helper n = function
      | ({name; _}, _) :: _ when name = search -> Some n
      | _ :: names -> helper (n + 1) names
      | [] -> None
    in
    helper 0 func_decls


  let decl_by_index {func_decls; _} idx =
    let rec helper = function
      | 0, decl :: _ -> decl
      | n, _ :: decls -> helper (n - 1, decls)
      | _, [] -> assert false
    in
    if idx < 0 then assert false else helper (idx, func_decls)


  let expr_by_index {func_exprs; _} idx =
    let rec helper = function
      | 0, expr :: _ -> expr
      | n, _ :: exprs -> helper (n - 1, exprs)
      | _, [] -> assert false
    in
    if idx < 0 then assert false else helper (idx, func_exprs)
end

let rec type_of_expr ctxt decl e =
  let e, sp = e in
  let%bind (), _ = with_span sp in
  match e with
  | Expr.Unit_literal -> wrap Type.Unit
  | Expr.Bool_literal _ -> wrap Type.Bool
  | Expr.Integer_literal _ -> wrap Type.Int
  | Expr.If_else (cond, e1, e2) -> (
      match%bind type_of_expr ctxt decl cond with
      | Type.Bool, _ ->
          let%bind t1, _ = type_of_expr ctxt decl e1 in
          let%bind t2, _ = type_of_expr ctxt decl e2 in
          if t1 = t2 then wrap t1
          else wrap_err (Error.If_branches_of_differing_type (t1, t2))
      | ty, _ -> wrap_err (Error.If_on_non_bool ty) )
  | Expr.Call (callee, args) -> (
      let%bind ty_callee, _ = type_of_expr ctxt decl callee in
      let%bind ty_args, _ =
        let rec helper = function
          | [] -> wrap []
          | x :: xs ->
              let%bind ty, _ = type_of_expr ctxt decl x in
              let%bind rest, _ = helper xs in
              wrap (ty :: rest)
        in
        helper args
      in
      match ty_callee with
      | Type.(Function {params; ret_ty}) ->
          if ty_args = params then wrap ret_ty
          else
            wrap_err
              (Error.Invalid_function_arguments
                 {expected= params; found= ty_args})
      | ty -> wrap_err (Error.Call_of_non_function ty) )
  | Expr.Builtin b -> (
    match b with
    | Expr.Builtin_add | Expr.Builtin_sub ->
        wrap Type.(Function {params= [Type.Int; Type.Int]; ret_ty= Type.Int})
    | Expr.Builtin_less_eq ->
        wrap Type.(Function {params= [Type.Int; Type.Int]; ret_ty= Type.Bool})
    )
  | Expr.Global_function f ->
      let decl, _ = Functions.decl_by_index ctxt f in
      let params = List.map (fun (_, ty) -> ty) decl.params in
      let ret_ty = decl.ret_ty in
      wrap Type.(Function {params; ret_ty})
  | Expr.Parameter p ->
      let _, ty = List.nth_exn p decl.params in
      wrap ty


let find_parameter name lst =
  let rec helper name lst idx =
    match lst with
    | [] -> None
    | (name', ty) :: _ when name' = name -> Some (ty, idx)
    | _ :: xs -> helper name xs (idx + 1)
  in
  helper name lst 0


let rec type_expression decl ast unt_expr =
  let module U = Untyped_ast.Expr in
  let module T = Expr in
  match%bind Ok unt_expr with
  | U.Unit_literal, _ -> wrap T.Unit_literal
  | U.Bool_literal b, _ -> wrap (T.Bool_literal b)
  | U.Integer_literal i, _ -> wrap (T.Integer_literal i)
  | U.If_else (cond, thn, els), _ ->
      let%bind cond = type_expression decl ast cond in
      let%bind thn = type_expression decl ast thn in
      let%bind els = type_expression decl ast els in
      wrap (T.If_else (cond, thn, els))
  | U.Call (callee, args), _ ->
      let%bind callee = type_expression decl ast callee in
      let rec helper = function
        | [] -> wrap []
        | x :: xs ->
            let%bind x = type_expression decl ast x in
            let%bind xs, _ = helper xs in
            wrap (x :: xs)
      in
      let%bind args, _ = helper args in
      wrap (T.Call (callee, args))
  | U.Variable name, _ ->
      let {params; _} = decl in
      match find_parameter name params with
      | None -> (
        match Functions.index_by_name ast name with
        | None -> (
          match name with
          | "LESS_EQ" -> wrap (T.Builtin T.Builtin_less_eq)
          | "ADD" -> wrap (T.Builtin T.Builtin_add)
          | "SUB" -> wrap (T.Builtin T.Builtin_sub)
          | _ -> wrap_err (Error.Name_not_found name) )
        | Some idx -> wrap (T.Global_function idx) )
      | Some (_ty, idx) -> wrap (T.Parameter idx)


let add_function_declaration unt_func (ctxt: t) : t monad =
  let module U = Untyped_ast.Item in
  let unt_func, _ = unt_func in
  let {U.fname= name; U.params; U.ret_ty; _} = unt_func in
  let%bind params, parm_sp =
    let rec helper ctxt = function
      | [] -> wrap []
      | (name, x) :: xs ->
          let%bind ty, _ = Types.get ctxt x in
          let%bind tys, _ = helper ctxt xs in
          wrap ((name, ty) :: tys)
    in
    helper ctxt params
  in
  let%bind ret_ty, rty_sp =
    match ret_ty with
    | Some ret_ty -> Types.get ctxt ret_ty
    | None -> wrap Type.Unit
  in
  let decl = ({name; params; ret_ty}, Spanned.union parm_sp rty_sp) in
  wrap {ctxt with func_decls= decl :: ctxt.func_decls}


(*
  note: call this on unt_funcs in the same order as you called
  add_function_declaration
*)
let add_function_definition unt_func (ctxt: t) : t monad =
  let module U = Untyped_ast.Item in
  let unt_func, _ = unt_func in
  let decl =
    let num_funcs = List.length ctxt.func_decls in
    let idx = num_funcs - 1 - List.length ctxt.func_exprs in
    let decl, _ = Functions.decl_by_index ctxt idx in
    assert (decl.name = unt_func.U.fname) ;
    decl
  in
  let%bind expr = type_expression decl ctxt unt_func.U.expr in
  let%bind expr_ty, _ = type_of_expr ctxt decl expr in
  if expr_ty = decl.ret_ty then
    wrap {ctxt with func_exprs= expr :: ctxt.func_exprs}
  else
    wrap_err
      (Error.Return_type_mismatch {expected= decl.ret_ty; found= expr_ty})


let make unt_ast =
  let module U = Untyped_ast in
  let rec add_declarations ast = function
    | unt_func :: funcs ->
        let%bind new_ast, _ = add_function_declaration unt_func ast in
        add_declarations new_ast funcs
    | [] -> wrap ast
  in
  let rec add_definitions ast = function
    | unt_func :: funcs ->
        let%bind new_ast, _ = add_function_definition unt_func ast in
        add_definitions new_ast funcs
    | [] -> wrap ast
  in
  (*
   note(ubsan): this eventually won't be an issue
   it's currently O(n^2), but by the time n gets big enough,
   this should be rewritten
  *)
  let check_for_duplicates _ast = wrap () in
  let ret = {func_decls= []; func_exprs= []} in
  let%bind ret, _ = add_declarations ret unt_ast.U.funcs in
  let%bind ret, _ = add_definitions ret unt_ast.U.funcs in
  let%bind (), _ = check_for_duplicates ret in
  wrap ret
