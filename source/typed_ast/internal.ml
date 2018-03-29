module Span = Spanned.Span
module Untyped_ast = Cafec_parse.Ast
module Error = Error
module Expr = Expr
open Spanned.Result.Monad

type func_decl = {fname: string; params: (string * Type.t) list; ret_ty: Type.t}

type t =
  { type_decls: string list
  ; type_defs: Type.definition list
  ; func_decls: func_decl Spanned.t list (* note(ubsan): always normalized *)
  ; func_defs: Expr.t Spanned.t list }

module Types : sig
  val type_untyped :
    t -> Untyped_ast.Type.t -> (Type.t, Error.t) Spanned.Result.t

  val normalize : t -> Type.t -> Type.t

  val definition : t -> Type.t -> Type.definition option
  (**
    only works for user-defined types;
    [definition . normalize] will *always* return (Some Struct) or None,
    never (Some Alias)
  *)
end = struct
  let rec type_untyped ctxt unt_ty =
    match unt_ty with
    | Untyped_ast.Type.Named name -> (
        let rec helper n find = function
          | [] -> None
          | name :: _ when String.equal find name -> Some (Type.User_defined n)
          | _ :: xs -> helper (n + 1) find xs
        in
        match helper 0 name ctxt.type_decls with
        | Some ty -> return ty
        | None ->
          match name with
          | "unit" -> return (Type.Builtin Type.Builtin_unit)
          | "bool" -> return (Type.Builtin Type.Builtin_bool)
          | "int" -> return (Type.Builtin Type.Builtin_int)
          | _ -> return_err (Error.Type_not_found unt_ty) )
    | Untyped_ast.Type.Function (params, ret_ty) ->
        let rec type_params ctxt = function
          | [] -> return []
          | (x, _) :: xs ->
              let%bind x = type_untyped ctxt x in
              let%bind xs = type_params ctxt xs in
              return (x :: xs)
        in
        let%bind params = type_params ctxt params in
        let%bind ret_ty =
          match ret_ty with
          | Some (ret_ty, _) -> type_untyped ctxt ret_ty
          | None -> return (Type.Builtin Type.Builtin_unit)
        in
        return (Type.Builtin (Type.Builtin_function {params; ret_ty}))


  let rec normalize ctxt = function
    | Type.Builtin b -> Type.Builtin b
    | Type.User_defined i ->
      match List.nth_exn ctxt.type_defs i with
      | Type.Def_alias t -> normalize ctxt t
      | Type.Def_struct _ -> Type.User_defined i


  let definition ctxt = function
    | Type.Builtin _ -> None
    | Type.User_defined i -> Some (List.nth_exn ctxt.type_defs i)
end

module Functions : sig
  val index_by_name : t -> string -> int option

  val decl_by_index : t -> int -> func_decl Spanned.t

  val expr_by_index : t -> int -> Expr.t Spanned.t
end = struct
  let index_by_name {func_decls; _} search =
    let rec helper n = function
      | ({fname; _}, _) :: _ when String.equal fname search -> Some n
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
  let%bind () = with_span sp in
  match e with
  | Expr.Unit_literal -> return (Type.Builtin Type.Builtin_unit)
  | Expr.Bool_literal _ -> return (Type.Builtin Type.Builtin_bool)
  | Expr.Integer_literal _ -> return (Type.Builtin Type.Builtin_int)
  | Expr.If_else (cond, e1, e2) -> (
      match%bind type_of_expr ctxt decl cond with
      | Type.Builtin Type.Builtin_bool ->
          let%bind t1 = type_of_expr ctxt decl e1 in
          let%bind t2 = type_of_expr ctxt decl e2 in
          if Type.equal t1 t2 then return t1
          else return_err (Error.If_branches_of_differing_type (t1, t2))
      | ty -> return_err (Error.If_on_non_bool ty) )
  | Expr.Call (callee, args) -> (
      let%bind ty_callee = type_of_expr ctxt decl callee in
      match ty_callee with
      | Type.(Builtin Builtin_function {params; ret_ty}) ->
          let%bind ty_args =
            let rec helper = function
              | [] -> return []
              | x :: xs ->
                  let%bind ty = type_of_expr ctxt decl x in
                  let%bind rest = helper xs in
                  return (ty :: rest)
            in
            helper args
          in
          if List.equal ty_args params ~equal:Type.equal then return ret_ty
          else
            return_err
              (Error.Invalid_function_arguments
                 {expected= params; found= ty_args})
      | ty -> return_err (Error.Call_of_non_function ty) )
  | Expr.Builtin b -> (
    match b with
    | Expr.Builtin.Add | Expr.Builtin.Sub | Expr.Builtin.Mul ->
        return
          Type.(
            Builtin
              (Builtin_function
                 { params= [Builtin Builtin_int; Builtin Builtin_int]
                 ; ret_ty= Builtin Builtin_int }))
    | Expr.Builtin.Less_eq ->
        return
          Type.(
            Builtin
              (Builtin_function
                 { params= [Builtin Builtin_int; Builtin Builtin_int]
                 ; ret_ty= Builtin Builtin_bool })) )
  | Expr.Global_function f ->
      let decl, _ = Functions.decl_by_index ctxt f in
      let rec get_params = function
        | (_, x) :: xs -> x :: get_params xs
        | [] -> []
      in
      let params = get_params decl.params in
      let ret_ty = decl.ret_ty in
      return Type.(Builtin (Builtin_function {params; ret_ty}))
  | Expr.Parameter p ->
      let _, ty = List.nth_exn decl.params p in
      return ty
  | Expr.Struct_literal (ty, members) ->
      let rec check_for_extra_members ty ty_members n = function
        | [] -> return ()
        | (m, _) :: _ when m = n ->
            let name, _ = List.nth_exn ty_members n in
            return_err
              (Error.Struct_literal_member_defined_multiple_times (ty, name))
        | _ :: xs -> check_for_extra_members ty ty_members n xs
      in
      let rec check_for_specific_member ty ty_members n = function
        | [] ->
            let name, _ = List.nth_exn ty_members n in
            return_err (Error.Struct_literal_without_member (ty, name))
        | (m, init) :: xs when n = m ->
            let%bind () = check_for_extra_members ty ty_members n xs in
            let%bind init_ty = type_of_expr ctxt decl init in
            let name, member_ty = List.nth_exn ty_members n in
            if Type.equal init_ty member_ty then return ()
            else
              return_err
                (Error.Struct_literal_incorrect_member_type
                   {ty; member= name; expected= member_ty; found= init_ty})
        | _ :: xs -> check_for_specific_member ty ty_members n xs
      in
      let rec check_for_all_members ty ty_members n members =
        let%bind () = check_for_specific_member ty ty_members n members in
        if n > 0 then check_for_all_members ty ty_members (n - 1) members
        else return ()
      in
      let ty_members =
        match Types.definition ctxt ty with
        | Some Type.Def_struct members -> members
        | _ -> assert false
      in
      let number_of_members = List.length ty_members in
      let%bind () =
        if number_of_members > 0 then
          check_for_all_members ty ty_members (number_of_members - 1) members
        else
          (* already did the checking for unknown members in type_expression *)
          return ()
      in
      return ty
  | Expr.Struct_access (expr, idx) ->
      let%bind ty = type_of_expr ctxt decl expr in
      (* note: we do this twice - should be memoized *)
      match Types.definition ctxt ty with
      | Some Type.Def_struct members ->
          let _, member_ty = List.nth_exn members idx in
          return member_ty
      (* this should've been caught in type_expression *)
      | Some _ | None -> assert false


let find_parameter name lst =
  let rec helper name lst idx =
    match lst with
    | [] -> None
    | (name', ty) :: _ when String.equal name' name -> Some (ty, idx)
    | _ :: xs -> helper name xs (idx + 1)
  in
  helper name lst 0


(* NOTE(ubsan): this does *not* typecheck *)
let rec type_expression decl ctxt unt_expr =
  let module U = Untyped_ast.Expr in
  let module T = Expr in
  let unt_expr, sp = unt_expr in
  let%bind () = with_span sp in
  match unt_expr with
  | U.Unit_literal -> return T.Unit_literal
  | U.Bool_literal b -> return (T.Bool_literal b)
  | U.Integer_literal i -> return (T.Integer_literal i)
  | U.If_else (cond, thn, els) ->
      let%bind cond = spanned_bind (type_expression decl ctxt cond) in
      let%bind thn = spanned_bind (type_expression decl ctxt thn) in
      let%bind els = spanned_bind (type_expression decl ctxt els) in
      return (T.If_else (cond, thn, els))
  | U.Call (callee, args) ->
      let%bind callee = spanned_bind (type_expression decl ctxt callee) in
      let rec helper = function
        | [] -> return []
        | x :: xs ->
            let%bind x = spanned_bind (type_expression decl ctxt x) in
            let%bind xs = helper xs in
            return (x :: xs)
      in
      let%bind args = helper args in
      return (T.Call (callee, args))
  | U.Variable name -> (
      let {params; _} = decl in
      match find_parameter name params with
      | None -> (
        match Functions.index_by_name ctxt name with
        | None -> (
          match name with
          | "LESS_EQ" -> return (T.Builtin T.Builtin.Less_eq)
          | "ADD" -> return (T.Builtin T.Builtin.Add)
          | "SUB" -> return (T.Builtin T.Builtin.Sub)
          | "MUL" -> return (T.Builtin T.Builtin.Mul)
          | _ -> return_err (Error.Name_not_found name) )
        | Some idx -> return (T.Global_function idx) )
      | Some (_ty, idx) -> return (T.Parameter idx) )
  | U.Struct_literal (ty, members) ->
      let%bind ty =
        let%bind ty = Types.type_untyped ctxt ty in
        return (Types.normalize ctxt ty)
      in
      let%bind type_members =
        match Types.definition ctxt ty with
        | Some Type.Def_struct members -> return members
        | Some Type.Def_alias _ -> assert false
        | None -> return_err (Error.Struct_literal_of_non_struct_type ty)
      in
      let%bind members =
        let rec find_member find n = function
          | [] -> None
          | (name, _) :: _ when String.equal name find -> Some n
          | _ :: xs -> find_member find (n + 1) xs
        in
        let rec helper decl ctxt = function
          | [] -> return []
          | ((name, expr), _) :: xs ->
              let%bind idx =
                match find_member name 0 type_members with
                | Some i -> return i
                | None ->
                    return_err
                      (Error.Struct_literal_with_unknown_member_name (ty, name))
              in
              let%bind expr = spanned_bind (type_expression decl ctxt expr) in
              let%bind rest = helper decl ctxt xs in
              return ((idx, expr) :: rest)
        in
        helper decl ctxt members
      in
      return (T.Struct_literal (ty, members))
  | U.Struct_access (expr, member) ->
      let%bind expr = spanned_bind (type_expression decl ctxt expr) in
      let%bind ty = type_of_expr ctxt decl expr in
      match Types.definition ctxt ty with
      | Some Type.Def_struct members -> (
          let rec find_idx n find = function
            | (name, _) :: _ when String.equal name find -> Some n
            | _ :: xs -> find_idx (n + 1) find xs
            | [] -> None
          in
          match find_idx 0 member members with
          | Some n -> return (Expr.Struct_access (expr, n))
          | None -> return_err (Error.Struct_access_non_member (ty, member)) )
      | Some Type.Def_alias _ -> assert false
      | None ->
          return_err (Error.Struct_access_on_non_struct_type (ty, member))


(*
  NOTE(ubsan): call the declaration functions in the same order as the
  definition functions

  NOTE(ubsan): type_declaration doesn't work on the AST,
  it just works on a list of strings
*)

let type_declaration unt_type ctxt =
  let module T = Untyped_ast.Type in
  let module I = Untyped_ast.Item in
  let rec duplicates search = function
    | [] -> false
    | name :: _ when String.equal name search -> true
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
    let name = List.nth_exn ctxt.type_decls idx in
    assert (String.equal name unt_type.I.tname)
  in
  let%bind def =
    match unt_type.I.kind with
    | I.Alias (unt_ty, _) ->
        let%bind t = Types.type_untyped ctxt unt_ty in
        return (Type.Def_alias t)
    | I.Struct members ->
        let rec helper ctxt = function
          | [] -> return []
          | (name, (x, sp)) :: xs ->
              let%bind () = with_span sp in
              let%bind x = Types.type_untyped ctxt x in
              let%bind xs = helper ctxt xs in
              return ((name, x) :: xs)
        in
        let%bind members = helper ctxt members in
        return (Type.Def_struct members)
  in
  return {ctxt with type_defs= def :: ctxt.type_defs}


let add_function_declaration unt_func ctxt =
  let module U = Untyped_ast.Item in
  let unt_func, _ = unt_func in
  let {U.fname; U.params; U.ret_ty; _} = unt_func in
  let%bind params, parm_sp =
    let rec helper ctxt = function
      | [] -> return []
      | (name, (x, _)) :: xs ->
          let%bind ty =
            let%bind tmp = Types.type_untyped ctxt x in
            return (Types.normalize ctxt tmp)
          in
          let%bind tys = helper ctxt xs in
          return ((name, ty) :: tys)
    in
    spanned_bind (helper ctxt params)
  in
  let%bind ret_ty, rty_sp =
    match ret_ty with
    | Some (ret_ty, _) ->
        let%bind ty, sp = spanned_bind (Types.type_untyped ctxt ret_ty) in
        return (Types.normalize ctxt ty, sp)
    | None -> return (Type.Builtin Type.Builtin_unit, Span.made_up)
  in
  (* check for duplicates *)
  let rec check_for_duplicates search = function
    | [] -> None
    | (f, sp) :: _ when String.equal f.fname search -> Some (f, sp)
    | _ :: xs -> check_for_duplicates search xs
  in
  match check_for_duplicates fname ctxt.func_decls with
  | Some (_, sp) ->
      return_err
        (Error.Defined_function_multiple_times
           {name= fname; original_declaration= sp})
  | None ->
      let decl = ({fname; params; ret_ty}, Span.union parm_sp rty_sp) in
      return {ctxt with func_decls= decl :: ctxt.func_decls}


let add_function_definition unt_func ctxt =
  let module U = Untyped_ast.Item in
  let unt_func, _ = unt_func in
  let decl =
    let num_funcs = List.length ctxt.func_decls in
    let idx = num_funcs - 1 - List.length ctxt.func_defs in
    let decl, _ = Functions.decl_by_index ctxt idx in
    assert (String.equal decl.fname unt_func.U.fname) ;
    decl
  in
  let%bind expr = spanned_bind (type_expression decl ctxt unt_func.U.expr) in
  let%bind expr_ty = type_of_expr ctxt decl expr in
  if Type.equal expr_ty decl.ret_ty then
    return {ctxt with func_defs= expr :: ctxt.func_defs}
  else
    return_err
      (Error.Return_type_mismatch {expected= decl.ret_ty; found= expr_ty})


let make unt_ast =
  let module U = Untyped_ast in
  let rec add_type_definitions ast = function
    | unt_type :: types ->
        let%bind new_ast = add_type_definition unt_type ast in
        add_type_definitions new_ast types
    | [] -> return ast
  in
  let rec add_function_declarations ast = function
    | unt_func :: funcs ->
        let%bind new_ast = add_function_declaration unt_func ast in
        add_function_declarations new_ast funcs
    | [] -> return ast
  in
  let rec add_function_definitions ast = function
    | unt_func :: funcs ->
        let%bind new_ast = add_function_definition unt_func ast in
        add_function_definitions new_ast funcs
    | [] -> return ast
  in
  let rec type_declarations lst = function
    | unt_type :: types -> (
      match type_declaration unt_type lst with
      | Ok lst -> type_declarations lst types
      | Error e -> Error e )
    | [] -> Ok lst
  in
  match type_declarations [] unt_ast.U.types with
  | Error (e, sp) -> (Error (e, []), sp)
  | Ok type_decls ->
      let ret =
        let ret = {type_decls; func_decls= []; func_defs= []; type_defs= []} in
        let%bind ret = add_type_definitions ret unt_ast.U.types in
        let%bind ret = add_function_declarations ret unt_ast.U.funcs in
        let%bind ret = add_function_definitions ret unt_ast.U.funcs in
        return ret
      in
      match ret with
      | Ok o, sp -> (Ok o, sp)
      | Error e, sp -> (Error (e, type_decls), sp)
