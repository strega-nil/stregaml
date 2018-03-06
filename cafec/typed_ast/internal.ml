module Spanned = Cafec_spanned
module Untyped_ast = Cafec_parse.Ast
module Error = Error
module Expr = Expr
open Spanned.Prelude
open Error.Monad_spanned

type 'a monad = 'a Error.Monad_spanned.t

type func_decl = {fname: string; params: (string * Type.t) list; ret_ty: Type.t}

type type_def =
  | Type_alias of Type.t
  | Type_struct of (string * Type.t) list

type t =
  { type_decls: string list
  ; type_defs: type_def list
  ; func_decls: func_decl spanned list (* note(ubsan): always normalized *)
  ; func_defs: Expr.t spanned list }

module Types : sig
  val type_untyped : t -> Untyped_ast.Type.t -> Type.t option

  val normalize : t -> Type.t -> Type.t
end = struct
  let type_untyped ctxt unt_ty =
    let Untyped_ast.Type.Named name = unt_ty in
    match name with
    | "unit" -> Some (Type.Builtin Type.Builtin_unit)
    | "bool" -> Some (Type.Builtin Type.Builtin_bool)
    | "int" -> Some (Type.Builtin Type.Builtin_int)
    | name ->
        let rec helper n find = function
          | [] -> None
          | name :: _ when find = name -> Some (Type.User_defined n)
          | _ :: xs -> helper (n + 1) find xs
        in
        helper 0 name ctxt.type_decls


  let rec normalize ctxt = function
    | Type.Builtin b -> Type.Builtin b
    | Type.User_defined i ->
      match List.nth_exn i ctxt.type_defs with
      | Type_alias t -> normalize ctxt t
      | Type_struct _ -> Type.User_defined i
end

module Functions : sig
  val index_by_name : t -> string -> int option

  val decl_by_index : t -> int -> func_decl spanned

  val expr_by_index : t -> int -> Expr.t spanned
end = struct
  let index_by_name {func_decls; _} search =
    let rec helper n = function
      | ({fname; _}, _) :: _ when fname = search -> Some n
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


  let expr_by_index {func_defs; _} idx =
    let rec helper = function
      | 0, expr :: _ -> expr
      | n, _ :: defs -> helper (n - 1, defs)
      | _, [] -> assert false
    in
    if idx < 0 then assert false else helper (idx, func_defs)
end

(* NOTE(ubsan): returns a normalized type *)
let rec type_of_expr ctxt decl e =
  let e, sp = e in
  let%bind (), _ = with_span sp in
  match e with
  | Expr.Unit_literal -> wrap (Type.Builtin Type.Builtin_unit)
  | Expr.Bool_literal _ -> wrap (Type.Builtin Type.Builtin_bool)
  | Expr.Integer_literal _ -> wrap (Type.Builtin Type.Builtin_int)
  | Expr.If_else (cond, e1, e2) -> (
      match%bind type_of_expr ctxt decl cond with
      | Type.Builtin Type.Builtin_bool, _ ->
          let%bind t1, _ = type_of_expr ctxt decl e1 in
          let%bind t2, _ = type_of_expr ctxt decl e2 in
          if t1 = t2 then wrap t1
          else wrap_err (Error.If_branches_of_differing_type (t1, t2))
      | ty, _ -> wrap_err (Error.If_on_non_bool ty) )
  | Expr.Call (callee, args) -> (
      let%bind ty_callee, _ = type_of_expr ctxt decl callee in
      match ty_callee with
      | Type.(Builtin Builtin_function {params; ret_ty}) ->
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
          if ty_args = params then wrap ret_ty
          else
            wrap_err
              (Error.Invalid_function_arguments
                 {expected= params; found= ty_args})
      | ty -> wrap_err (Error.Call_of_non_function ty) )
  | Expr.Builtin b -> (
    match b with
    | Expr.Builtin_add | Expr.Builtin_sub ->
        wrap
          Type.(
            Builtin
              (Builtin_function
                 { params= [Builtin Builtin_int; Builtin Builtin_int]
                 ; ret_ty= Builtin Builtin_int }))
    | Expr.Builtin_less_eq ->
        wrap
          Type.(
            Builtin
              (Builtin_function
                 { params= [Builtin Builtin_int; Builtin Builtin_int]
                 ; ret_ty= Builtin Builtin_bool })) )
  | Expr.Global_function f ->
      let decl, _ = Functions.decl_by_index ctxt f in
      let params = List.map (fun (_, ty) -> ty) decl.params in
      let ret_ty = decl.ret_ty in
      wrap Type.(Builtin (Builtin_function {params; ret_ty}))
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

(*
  NOTE(ubsan): call the declaration functions in the same order as the
  definition functions
*)

let add_type_declaration unt_type ctxt =
  let module T = Untyped_ast.Type in
  let module I = Untyped_ast.Item in
  let rec duplicates search = function
    | [] -> false
    | name :: _ when name = search -> true
    | _ :: xs -> duplicates search xs
  in
  let {I.tname; _}, sp = unt_type in
  let%bind (), _ = with_span sp in
  if duplicates tname ctxt.type_decls then assert false
  else if tname = "int" || tname = "unit" || tname = "bool" then
    assert false
  else wrap {ctxt with type_decls= tname :: ctxt.type_decls}

let add_type_definition unt_type ctxt =
  let module T = Untyped_ast.Type in
  let module I = Untyped_ast.Item in
  let unt_type, _ = unt_type in
  let () =
    let num_types = List.length ctxt.type_decls in
    let idx = num_types - 1 - List.length ctxt.type_defs in
    let name = List.nth_exn idx ctxt.type_decls in
    assert (name = unt_type.I.tname)
  in
  let%bind def, _ =
    match unt_type.I.kind with
    | I.Alias (unt_ty, _) ->
        ( match Types.type_untyped ctxt unt_ty with
        | Some t -> wrap (Type_alias t)
        | None -> wrap_err (Error.Type_not_found unt_ty) )
    | I.Struct members ->
        let rec helper ctxt = function
          | [] -> wrap []
          | (name, (x, sp)) :: xs ->
              let%bind (), _ = with_span sp in
              let%bind x, _ =
                match Types.type_untyped ctxt x with
                | Some x -> wrap x
                | None -> wrap_err (Error.Type_not_found x)
              in
              let%bind xs, _ = helper ctxt xs in
              wrap ((name, x) :: xs)
        in
        let%bind members, _ = helper ctxt members in
        wrap (Type_struct members)
  in
  wrap {ctxt with type_defs= def :: ctxt.type_defs}


let add_function_declaration unt_func (ctxt: t) : t monad =
  let module U = Untyped_ast.Item in
  let unt_func, _ = unt_func in
  let {U.fname; U.params; U.ret_ty; _} = unt_func in
  let%bind params, parm_sp =
    let rec helper ctxt = function
      | [] -> wrap []
      | (name, (x, _)) :: xs ->
          match Types.type_untyped ctxt x with
          | Some ty ->
              let ty = Types.normalize ctxt ty in
              let%bind tys, _ = helper ctxt xs in
              wrap ((name, ty) :: tys)
          | None -> wrap_err (Error.Type_not_found x)
    in
    helper ctxt params
  in
  let%bind ret_ty, rty_sp =
    match ret_ty with
    | Some (ret_ty, _) ->
        ( match Types.type_untyped ctxt ret_ty with
        | Some ty -> wrap (Types.normalize ctxt ty)
        | None -> wrap_err (Error.Type_not_found ret_ty) )
    | None -> wrap (Type.Builtin Type.Builtin_unit)
  in
  (* check for duplicates *)
  let rec check_for_duplicates search = function
    | [] -> None
    | (f, sp) :: _ when f.fname = search -> Some (f, sp)
    | _ :: xs -> check_for_duplicates search xs
  in
  match check_for_duplicates fname ctxt.func_decls with
  | Some (_, sp) ->
      wrap_err
        (Error.Defined_function_multiple_times
           {name= fname; original_declaration= sp})
  | None ->
      let decl = ({fname; params; ret_ty}, Spanned.union parm_sp rty_sp) in
      wrap {ctxt with func_decls= decl :: ctxt.func_decls}


let add_function_definition unt_func (ctxt: t) : t monad =
  let module U = Untyped_ast.Item in
  let unt_func, _ = unt_func in
  let decl =
    let num_funcs = List.length ctxt.func_decls in
    let idx = num_funcs - 1 - List.length ctxt.func_defs in
    let decl, _ = Functions.decl_by_index ctxt idx in
    assert (decl.fname = unt_func.U.fname) ;
    decl
  in
  let%bind expr = type_expression decl ctxt unt_func.U.expr in
  let%bind expr_ty, _ = type_of_expr ctxt decl expr in
  if expr_ty = decl.ret_ty then
    wrap {ctxt with func_defs= expr :: ctxt.func_defs}
  else
    wrap_err
      (Error.Return_type_mismatch {expected= decl.ret_ty; found= expr_ty})


let make unt_ast =
  let module U = Untyped_ast in
  let rec add_type_declarations ast = function
    | unt_type :: types ->
        let%bind new_ast, _ = add_type_declaration unt_type ast in
        add_type_declarations new_ast types
    | [] -> wrap ast
  in
  let rec add_type_definitions ast = function
    | unt_type :: types ->
        let%bind new_ast, _ = add_type_definition unt_type ast in
        add_type_definitions new_ast types
    | [] -> wrap ast
  in
  let rec add_function_declarations ast = function
    | unt_func :: funcs ->
        let%bind new_ast, _ = add_function_declaration unt_func ast in
        add_function_declarations new_ast funcs
    | [] -> wrap ast
  in
  let rec add_function_definitions ast = function
    | unt_func :: funcs ->
        let%bind new_ast, _ = add_function_definition unt_func ast in
        add_function_definitions new_ast funcs
    | [] -> wrap ast
  in
  let ret = {type_decls= []; func_decls= []; func_defs= []; type_defs= []} in
  let%bind ret, _ = add_type_declarations ret unt_ast.U.types in
  let%bind ret, _ = add_type_definitions ret unt_ast.U.types in
  let%bind ret, _ = add_function_declarations ret unt_ast.U.funcs in
  let%bind ret, _ = add_function_definitions ret unt_ast.U.funcs in
  wrap ret
