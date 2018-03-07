module Spanned = Cafec_spanned
module Untyped_ast = Cafec_parse.Ast
module Error = Error
module Expr = Expr
open Spanned.Prelude
open Error.Monad_spanned

type 'a monad = 'a Error.Monad_spanned.t

type func_decl = {fname: string; params: (string * Type.t) list; ret_ty: Type.t}

type t =
  { type_decls: string list
  ; type_defs: Type.definition list
  ; func_decls: func_decl spanned list (* note(ubsan): always normalized *)
  ; func_defs: Expr.t spanned list }

module Types : sig
  val type_untyped : t -> Untyped_ast.Type.t -> Type.t option

  val normalize : t -> Type.t -> Type.t

  val definition : t -> Type.t -> Type.definition option
  (**
    only works for user-defined types;
    definition . normalize will *always* return (Some Struct) or None,
    never (Some Alias)
  *)
end = struct
  let type_untyped ctxt unt_ty =
    let Untyped_ast.Type.Named name = unt_ty in
    let rec helper n find = function
      | [] -> None
      | name :: _ when find = name -> Some (Type.User_defined n)
      | _ :: xs -> helper (n + 1) find xs
    in
    match helper 0 name ctxt.type_decls with
    | Some ty -> Some ty
    | None ->
      match name with
      | "unit" -> Some (Type.Builtin Type.Builtin_unit)
      | "bool" -> Some (Type.Builtin Type.Builtin_bool)
      | "int" -> Some (Type.Builtin Type.Builtin_int)
      | _ -> None


  let rec normalize ctxt = function
    | Type.Builtin b -> Type.Builtin b
    | Type.User_defined i ->
      match List.nth_exn i ctxt.type_defs with
      | Type.Def_alias t -> normalize ctxt t
      | Type.Def_struct _ -> Type.User_defined i


  let definition ctxt = function
    | Type.Builtin _ -> None
    | Type.User_defined i -> Some (List.nth_exn i ctxt.type_defs)
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

(*
  NOTE(ubsan): returns a normalized type
  also, typechecks
*)
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
    | Expr.Builtin_add | Expr.Builtin_sub | Expr.Builtin_mul ->
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
  | Expr.Struct_literal (ty, members) ->
      let rec check_for_extra_members ty ty_members n = function
        | [] -> wrap ()
        | (m, _) :: _ when m = n ->
            let name, _ = List.nth_exn n ty_members in
            wrap_err
              (Error.Struct_literal_member_defined_multiple_times (ty, name))
        | _ :: xs -> check_for_extra_members ty ty_members n xs
      in
      let rec check_for_specific_member ty ty_members n = function
        | [] ->
            print_string "\n\n  number of members: " ;
            print_int (List.length ty_members) ;
            print_string ", n: " ;
            print_int n ;
            print_string "\n\n" ;
            let name, _ = List.nth_exn n ty_members in
            wrap_err (Error.Struct_literal_without_member (ty, name))
        | (m, init) :: xs when n = m ->
            let%bind (), _ = check_for_extra_members ty ty_members n xs in
            let%bind init_ty, _ = type_of_expr ctxt decl init in
            let name, member_ty = List.nth_exn n ty_members in
            if init_ty = member_ty then wrap ()
            else
              wrap_err
                (Error.Struct_literal_incorrect_member_type
                   {ty; member= name; expected= member_ty; found= init_ty})
        | _ :: xs -> check_for_specific_member ty ty_members n xs
      in
      let rec check_for_all_members ty ty_members n members =
        let%bind (), _ = check_for_specific_member ty ty_members n members in
        if n > 0 then check_for_all_members ty ty_members (n - 1) members
        else wrap ()
      in
      let ty_members =
        match Types.definition ctxt ty with
        | Some Type.Def_struct members -> members
        | _ -> assert false
      in
      let number_of_members = List.length ty_members in
      let%bind (), _ =
        if number_of_members > 0 then
          check_for_all_members ty ty_members (number_of_members - 1) members
        else
          (* already did the checking for unknown members in type_expression *)
          wrap ()
      in
      wrap ty
  | Expr.Struct_access (expr, idx) ->
      let%bind ty, _ = type_of_expr ctxt decl expr in
      (* note: we do this twice - should be memoized *)
      match Types.definition ctxt ty with
      | Some Type.Def_struct members ->
          let _, member_ty = List.nth_exn idx members in
          wrap member_ty
      | Some _ | None -> assert false


(* this should've been caught in type_expression *)

let find_parameter name lst =
  let rec helper name lst idx =
    match lst with
    | [] -> None
    | (name', ty) :: _ when name' = name -> Some (ty, idx)
    | _ :: xs -> helper name xs (idx + 1)
  in
  helper name lst 0


(* NOTE(ubsan): this does *not* typecheck *)
let rec type_expression decl ctxt unt_expr =
  let module U = Untyped_ast.Expr in
  let module T = Expr in
  match%bind Ok unt_expr with
  | U.Unit_literal, _ -> wrap T.Unit_literal
  | U.Bool_literal b, _ -> wrap (T.Bool_literal b)
  | U.Integer_literal i, _ -> wrap (T.Integer_literal i)
  | U.If_else (cond, thn, els), _ ->
      let%bind cond = type_expression decl ctxt cond in
      let%bind thn = type_expression decl ctxt thn in
      let%bind els = type_expression decl ctxt els in
      wrap (T.If_else (cond, thn, els))
  | U.Call (callee, args), _ ->
      let%bind callee = type_expression decl ctxt callee in
      let rec helper = function
        | [] -> wrap []
        | x :: xs ->
            let%bind x = type_expression decl ctxt x in
            let%bind xs, _ = helper xs in
            wrap (x :: xs)
      in
      let%bind args, _ = helper args in
      wrap (T.Call (callee, args))
  | U.Variable name, _ -> (
      let {params; _} = decl in
      match find_parameter name params with
      | None -> (
        match Functions.index_by_name ctxt name with
        | None -> (
          match name with
          | "LESS_EQ" -> wrap (T.Builtin T.Builtin_less_eq)
          | "ADD" -> wrap (T.Builtin T.Builtin_add)
          | "SUB" -> wrap (T.Builtin T.Builtin_sub)
          | "MUL" -> wrap (T.Builtin T.Builtin_mul)
          | _ -> wrap_err (Error.Name_not_found name) )
        | Some idx -> wrap (T.Global_function idx) )
      | Some (_ty, idx) -> wrap (T.Parameter idx) )
  | U.Struct_literal (ty, members), _ ->
      let%bind ty, _ =
        match Types.type_untyped ctxt ty with
        | Some ty -> wrap (Types.normalize ctxt ty)
        | None -> wrap_err (Error.Type_not_found ty)
      in
      let%bind type_members, _ =
        match Types.definition ctxt ty with
        | Some Type.Def_struct members -> wrap members
        | Some Type.Def_alias _ -> assert false
        | None -> wrap_err (Error.Struct_literal_of_non_struct_type ty)
      in
      let%bind members, _ =
        let rec find_member find n = function
          | [] -> None
          | (name, _) :: _ when name = find -> Some n
          | _ :: xs -> find_member find (n + 1) xs
        in
        let rec helper decl ctxt = function
          | [] -> wrap []
          | ((name, expr), _) :: xs ->
              let%bind idx, _ =
                match find_member name 0 type_members with
                | Some i -> wrap i
                | None ->
                    wrap_err
                      (Error.Struct_literal_with_unknown_member_name (ty, name))
              in
              let%bind expr = type_expression decl ctxt expr in
              let%bind rest, _ = helper decl ctxt xs in
              wrap ((idx, expr) :: rest)
        in
        helper decl ctxt members
      in
      wrap (T.Struct_literal (ty, members))
  | U.Struct_access (expr, member), _ ->
      let%bind expr = type_expression decl ctxt expr in
      let%bind ty, _ = type_of_expr ctxt decl expr in
      match Types.definition ctxt ty with
      | Some Type.Def_struct members -> (
          let rec find_idx n find = function
            | (name, _) :: _ when name = find -> Some n
            | _ :: xs -> find_idx (n + 1) find xs
            | [] -> None
          in
          match find_idx 0 member members with
          | Some n -> wrap (Expr.Struct_access (expr, n))
          | None -> wrap_err (Error.Struct_access_non_member (ty, member)) )
      | Some Type.Def_alias _ -> assert false
      | None -> wrap_err (Error.Struct_access_on_non_struct_type (ty, member))


(*
  NOTE(ubsan): call the declaration functions in the same order as the
  definition functions

  NOTE(ubsan): type_declaration doesn't work on the AST,
  it just works on a list of strings
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
  if duplicates tname ctxt then
    Error (Error.Defined_type_multiple_times tname, sp)
  else Ok (tname :: ctxt)


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
    | I.Alias (unt_ty, _) -> (
      match Types.type_untyped ctxt unt_ty with
      | Some t -> wrap (Type.Def_alias t)
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
        wrap (Type.Def_struct members)
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
    | Some (ret_ty, _) -> (
      match Types.type_untyped ctxt ret_ty with
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
  let rec type_declarations lst = function
    | unt_type :: types -> (
      match add_type_declaration unt_type lst with
      | Ok lst -> type_declarations lst types
      | Error e -> Error e )
    | [] -> Ok lst
  in
  match type_declarations [] unt_ast.U.types with
  | Error (e, sp) -> Error ((e, []), sp)
  | Ok type_decls ->
      let ret =
        let ret = {type_decls; func_decls= []; func_defs= []; type_defs= []} in
        let%bind ret, _ = add_type_definitions ret unt_ast.U.types in
        let%bind ret, _ = add_function_declarations ret unt_ast.U.funcs in
        let%bind ret, _ = add_function_definitions ret unt_ast.U.funcs in
        wrap ret
      in
      match ret with
      | Ok o -> Ok o
      | Error (e, sp) -> Error ((e, type_decls), sp)
