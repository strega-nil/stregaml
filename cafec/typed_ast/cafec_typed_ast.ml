module Error = Error
module Spanned = Cafec_spanned
module Untyped_ast = Cafec_parse.Ast
open Spanned.Prelude
open Error.Monad_spanned

type 'a monad = 'a Error.Monad_spanned.t

type builtin = Builtin_less_eq | Builtin_add | Builtin_sub

type decl = {params: (string * Type.t) list; ret_ty: Type.t}

(* TODO(ubsan): add spans *)
type expr =
  | Unit_literal
  | Bool_literal of bool
  | Integer_literal of int
  | If_else of (expr spanned * expr spanned * expr spanned)
  | Call of (expr spanned * expr spanned list)
  | Builtin of builtin
  | Global_function of int
  | Parameter of int

type t =
  { func_names: string list
  ; func_decls: decl spanned list
  ; func_exprs: expr spanned list }

module Type : sig
  type _ast_ty = t

  include module type of Type

  val get : _ast_ty -> Untyped_ast.Type.t -> t monad
end = struct
  type _ast_ty = t

  include Type

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

module Function : sig
  val index_by_name : t -> string -> int option

  val decl_by_index : t -> int -> decl spanned

  val name_and_decl_by_index : t -> int -> string * decl spanned

  val expr_by_index : t -> int -> expr spanned
end = struct
  let index_by_name {func_names; _} name =
    let rec helper n = function
      | name' :: _ when name = name' -> Some n
      | _ :: names -> helper (n + 1) names
      | [] -> None
    in
    helper 0 func_names


  let decl_by_index {func_decls; _} idx =
    let rec helper = function
      | 0, decl :: _ -> decl
      | n, _ :: decls -> helper (n - 1, decls)
      | _, [] -> assert false
    in
    if idx < 0 then assert false else helper (idx, func_decls)


  let name_and_decl_by_index {func_decls; func_names; _} idx =
    let rec helper = function
      | 0, name :: _, decl :: _ -> (name, decl)
      | n, _ :: names, _ :: decls -> helper (n - 1, names, decls)
      | _, _, [] | _, [], _ -> assert false
    in
    if idx < 0 then assert false else helper (idx, func_names, func_decls)


  let expr_by_index {func_exprs; _} idx =
    let rec helper = function
      | 0, expr :: _ -> expr
      | n, _ :: exprs -> helper (n - 1, exprs)
      | _, [] -> assert false
    in
    if idx < 0 then assert false else helper (idx, func_exprs)
end

let rec type_of_expr ctxt e =
  let e, sp = e in
  let%bind (), _ = with_span sp in
  match e with
  | Unit_literal -> wrap Type.Unit
  | Bool_literal _ -> wrap Type.Bool
  | Integer_literal _ -> wrap Type.Int
  | If_else (cond, e1, e2) -> (
      match%bind type_of_expr ctxt cond with
      | Type.Bool, _ ->
          let%bind t1, _ = type_of_expr ctxt e1 in
          let%bind t2, _ = type_of_expr ctxt e2 in
          if t1 = t2 then wrap t1
          else wrap_err (Error.If_branches_of_differing_type (t1, t2))
      | ty, _ -> wrap_err (Error.If_on_non_bool ty) )
  | Call (callee, args) -> (
      let%bind ty_callee, _ = type_of_expr ctxt callee in
      let%bind ty_args, _ =
        let rec helper = function
          | [] -> wrap []
          | x :: xs ->
              let%bind ty, _ = type_of_expr ctxt x in
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
  | Builtin b -> (
    match b with
    | Builtin_add | Builtin_sub ->
        wrap Type.(Function {params= [Type.Int; Type.Int]; ret_ty= Type.Int})
    | Builtin_less_eq ->
        wrap Type.(Function {params= [Type.Int; Type.Int]; ret_ty= Type.Bool})
    )
  | Global_function f ->
      let decl, _ = Function.decl_by_index ctxt f in
      let params = List.map (fun (_, ty) -> ty) decl.params in
      let ret_ty = decl.ret_ty in
      wrap Type.(Function {params; ret_ty})
  | Parameter _ -> assert false


let find_parameter name lst =
  let rec helper name lst idx =
    match lst with
    | [] -> None
    | (name', ty) :: _ when name' = name -> Some (ty, idx)
    | _ :: xs -> helper name xs (idx + 1)
  in
  helper name lst 0


let rec type_expression decl ast unt_expr =
  let module E = Untyped_ast.Expr in
  match%bind Ok unt_expr with
  | E.Unit_literal, _ -> wrap Unit_literal
  | E.Bool_literal b, _ -> wrap (Bool_literal b)
  | E.Integer_literal i, _ -> wrap (Integer_literal i)
  | E.If_else (cond, thn, els), _ ->
      let%bind cond = type_expression decl ast cond in
      let%bind thn = type_expression decl ast thn in
      let%bind els = type_expression decl ast els in
      wrap (If_else (cond, thn, els))
  | E.Call (callee, args), _ ->
      let%bind callee = type_expression decl ast callee in
      let rec helper = function
        | [] -> wrap []
        | x :: xs ->
            let%bind x = type_expression decl ast x in
            let%bind xs, _ = helper xs in
            wrap (x :: xs)
      in
      let%bind args, _ = helper args in
      wrap (Call (callee, args))
  | E.Variable name, _ ->
      let {params; _} = decl in
      match find_parameter name params with
      | None -> (
        match Function.index_by_name ast name with
        | None -> (
          match name with
          | "LESS_EQ" -> wrap (Builtin Builtin_less_eq)
          | "ADD" -> wrap (Builtin Builtin_add)
          | "SUB" -> wrap (Builtin Builtin_sub)
          | _ -> wrap_err (Error.Name_not_found name) )
        | Some idx -> wrap (Global_function idx) )
      | Some (_ty, idx) -> wrap (Parameter idx)


let add_function_declaration unt_func (ctxt: t) : t monad =
  let module F = Untyped_ast.Function in
  let unt_func, _ = unt_func in
  let {F.name; F.params; F.ret_ty; _} = unt_func in
  let%bind params, parm_sp =
    let rec helper ctxt = function
      | [] -> wrap []
      | (name, x) :: xs ->
          let%bind ty, _ = Type.get ctxt x in
          let%bind tys, _ = helper ctxt xs in
          wrap ((name, ty) :: tys)
    in
    helper ctxt params
  in
  let%bind ret_ty, rty_sp =
    match ret_ty with
    | Some ret_ty -> Type.get ctxt ret_ty
    | None -> wrap Type.Unit
  in
  let decl = ({params; ret_ty}, Spanned.union parm_sp rty_sp) in
  wrap
    { ctxt with
      func_names= name :: ctxt.func_names; func_decls= decl :: ctxt.func_decls
    }


(*
  note: call this on unt_funcs in the same order as you called
  add_function_declaration
*)
let add_function_definition unt_func (ctxt: t) : t monad =
  let module F = Untyped_ast.Function in
  let unt_func, _ = unt_func in
  let decl =
    let num_funcs = List.length ctxt.func_names in
    let idx = num_funcs - 1 - List.length ctxt.func_exprs in
    let name, (decl, _) = Function.name_and_decl_by_index ctxt idx in
    assert (name = unt_func.F.name) ;
    decl
  in
  let%bind expr = type_expression decl ctxt unt_func.F.expr in
  let%bind expr_ty, _ = type_of_expr ctxt expr in
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
  let ret = {func_names= []; func_decls= []; func_exprs= []} in
  let%bind ret, _ = add_declarations ret unt_ast.U.funcs in
  let%bind ret, _ = add_definitions ret unt_ast.U.funcs in
  let%bind (), _ = check_for_duplicates ret in
  wrap ret


type value =
  | Value_unit
  | Value_bool of bool
  | Value_integer of int
  | Value_function of int
  | Value_builtin of builtin

let run self =
  let rec eval args ctxt = function
    | Unit_literal -> Value_unit
    | Bool_literal b -> Value_bool b
    | Integer_literal n -> Value_integer n
    | If_else ((cond, _), (thn, _), (els, _)) -> (
      match eval args ctxt cond with
      | Value_bool true -> eval args ctxt thn
      | Value_bool false -> eval args ctxt els
      | _ -> assert false )
    | Parameter i -> List.nth_exn i args
    | Call ((e, _), args') -> (
      match eval args ctxt e with
      | Value_function func ->
          let expr, _ = Function.expr_by_index self func in
          let args' = List.map (fun (e, _) -> eval args ctxt e) args' in
          eval args' ctxt expr
      | Value_builtin b ->
          let (lhs, _), (rhs, _) =
            match args' with [lhs; rhs] -> (lhs, rhs) | _ -> assert false
          in
          let lhs =
            match eval args ctxt lhs with
            | Value_integer v -> v
            | _ -> assert false
          in
          let rhs =
            match eval args ctxt rhs with
            | Value_integer v -> v
            | _ -> assert false
          in
          let ret =
            match b with
            | Builtin_add -> Value_integer (lhs + rhs)
            | Builtin_sub -> Value_integer (lhs - rhs)
            | Builtin_less_eq -> Value_bool (lhs <= rhs)
          in
          ret
      | _ -> assert false )
    | Builtin b -> Value_builtin b
    | Global_function i -> Value_function i
  in
  match Function.index_by_name self "main" with
  | None -> print_endline "main not defined"
  | Some idx ->
      let main_expr, _ = Function.expr_by_index self idx in
      match eval [] self main_expr with
      | Value_integer n -> Printf.printf "main returned %d\n" n
      | Value_bool true -> print_endline "main returned true"
      | Value_bool false -> print_endline "main returned false"
      | _ -> assert false
