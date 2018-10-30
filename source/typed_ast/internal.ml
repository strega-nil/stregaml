module Span = Spanned.Span
module Untyped_ast = Cafec_parse.Ast
module Expr = Ast.Expr
open Spanned.Result.Monad

module Function_declaration = struct
  type t = {name: string; params: (string * Type.t) list; ret_ty: Type.t}
end

type t =
  { type_context: Type.Context.t
  ; function_context: Function_declaration.t Spanned.t list
  ; function_definitions: Expr.block Spanned.t list }

type 'a result = ('a, Error.t) Spanned.Result.t

module Functions : sig
  val index_by_name :
    Function_declaration.t Spanned.t list -> string -> int option

  val decl_by_index :
    Function_declaration.t Spanned.t list -> int
    -> Function_declaration.t Spanned.t

  val expr_by_index : Expr.block Spanned.t list -> int -> Expr.block Spanned.t
end = struct
  let index_by_name ctxt search =
    let rec helper n = function
      | ({Function_declaration.name; _}, _) :: _ when String.equal name search ->
          Some n
      | _ :: names -> helper (n + 1) names
      | [] -> None
    in
    helper 0 ctxt


  let decl_by_index ctxt idx =
    let rec helper = function
      | 0, decl :: _ -> decl
      | n, _ :: decls -> helper (n - 1, decls)
      | _, [] -> assert false
    in
    if idx < 0 then assert false else helper (idx, ctxt)


  let expr_by_index func_defs idx =
    let rec helper = function
      | 0, expr :: _ -> expr
      | n, _ :: defs -> helper (n - 1, defs)
      | _, [] -> assert false
    in
    if idx < 0 then assert false else helper (idx, func_defs)
end

(* also typechecks *)
let rec type_of_block (ctxt: t) (decl: Function_declaration.t)
    (blk: Expr.block Spanned.t) : Type.t result =
  let blk, _ = blk in
  match blk.Expr.expr with
  | None -> return (Type.Builtin Type.Unit)
  | Some e -> type_of_expr ctxt decl e


and type_of_expr (ctxt: t) (decl: Function_declaration.t) (e: Expr.t Spanned.t)
    : Type.t result =
  let e, sp = e in
  let%bind () = with_span sp in
  match e with
  | Expr.Unit_literal -> return (Type.Builtin Type.Unit)
  | Expr.Bool_literal _ -> return (Type.Builtin Type.Bool)
  | Expr.Integer_literal _ -> return (Type.Builtin Type.Int)
  | Expr.If_else (cond, e1, e2) -> (
      match%bind type_of_expr ctxt decl cond with
      | Type.Builtin Type.Bool ->
          let%bind t1 = type_of_block ctxt decl e1 in
          let%bind t2 = type_of_block ctxt decl e2 in
          if Type.equal t1 t2 then return t1
          else return_err (Error.If_branches_of_differing_type (t1, t2))
      | ty -> return_err (Error.If_non_bool ty) )
  | Expr.Call (callee, args) -> (
      let%bind ty_callee = type_of_expr ctxt decl callee in
      match ty_callee with
      | Type.Builtin Type.Function {params; ret_ty} ->
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
      let open Type in
      match b with
      | Expr.Builtin.Add | Expr.Builtin.Sub | Expr.Builtin.Mul ->
          let func =
            Function {params= [Builtin Int; Builtin Int]; ret_ty= Builtin Int}
          in
          return (Builtin func)
      | Expr.Builtin.Less_eq ->
          let func =
            Function {params= [Builtin Int; Builtin Int]; ret_ty= Builtin Bool}
          in
          return (Builtin func) )
  | Expr.Block blk -> type_of_block ctxt decl blk
  | Expr.Global_function f ->
      let decl, _ = Functions.decl_by_index ctxt.function_context f in
      let rec get_params = function
        | (_, x) :: xs -> x :: get_params xs
        | [] -> []
      in
      let params = get_params decl.Function_declaration.params in
      let ret_ty = decl.Function_declaration.ret_ty in
      return (Type.Builtin (Type.Function {params; ret_ty}))
  | Expr.Parameter p ->
      let _, ty = List.nth_exn decl.Function_declaration.params p in
      return ty
  | Expr.Record_literal {ty; members} -> (
      let compare ((name1, _), _) ((name2, _), _) =
        String.compare name1 name2
      in
      match List.find_a_dup ~compare members with
      | Some ((name, _), _) ->
          return_err (Error.Record_literal_duplicate_members name)
      | None ->
          let rec map = function
            | [] -> return []
            | ((name, e), _) :: xs ->
                let%bind ty = type_of_expr ctxt decl e in
                let%bind rest = map xs in
                return ((name, ty) :: rest)
          in
          let%bind members = map members in
          let ty = Type.User_defined ty in
          let structural = Type.structural ty ~ctxt:ctxt.type_context in
          (* TODO: fix this *)
          if Poly.equal (Type.Structural.Record members) structural then
            return ty
          else failwith "not yet implemented" )
  | Expr.Record_access (expr, name) ->
      let%bind ty = type_of_expr ctxt decl expr in
      match Type.structural ty ~ctxt:ctxt.type_context with
      | Type.Structural.Record members -> (
          let f (n, _) = String.equal n name in
          match List.find ~f members with
          | Some (_, ty) -> return ty
          | None -> return_err (Error.Record_access_non_member (ty, name)) )
      | _ -> return_err (Error.Record_access_non_record_type (ty, name))


let find_parameter name lst =
  let rec helper name lst idx =
    match lst with
    | [] -> None
    | (name', ty) :: _ when String.equal name' name -> Some (ty, idx)
    | _ :: xs -> helper name xs (idx + 1)
  in
  helper name lst 0


(* NOTE(ubsan): this does *not* do typechecking *)
let rec type_block decl (ctxt: t) unt_blk =
  let module U = Untyped_ast in
  let module T = Ast in
  let rec type_stmts = function
    | [] -> return []
    | (s, sp) :: xs ->
      match s with
      | U.Stmt.Expression e ->
        let%bind e = type_expression decl ctxt e in
        let%bind xs = type_stmts xs in
        return ((T.Stmt.Expression e, sp) :: xs)
      | _ -> assert false
  in
  let U.Expr.({stmts; expr}), sp = unt_blk in
  let%bind stmts = type_stmts stmts in
  let%bind expr =
    match expr with
    | Some e ->
        let%bind e = spanned_bind (type_expression decl ctxt e) in
        return (Some e)
    | None -> return None
  in
  (Ok T.Expr.{stmts; expr}, sp)


and type_expression decl (ctxt: t) unt_expr =
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
      let%bind thn = spanned_bind (type_block decl ctxt thn) in
      let%bind els = spanned_bind (type_block decl ctxt els) in
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
  | U.Variable {path= _; name} -> (
      let {Function_declaration.params; _} = decl in
      match find_parameter name params with
      | None -> (
        match Functions.index_by_name ctxt.function_context name with
        | None -> (
          match name with
          | "LESS_EQ" -> return (T.Builtin T.Builtin.Less_eq)
          | "ADD" -> return (T.Builtin T.Builtin.Add)
          | "SUB" -> return (T.Builtin T.Builtin.Sub)
          | "MUL" -> return (T.Builtin T.Builtin.Mul)
          | _ -> return_err (Error.Name_not_found name) )
        | Some idx -> return (T.Global_function idx) )
      | Some (_ty, idx) -> return (T.Parameter idx) )
  | U.Block blk ->
      let%bind blk = spanned_bind (type_block decl ctxt blk) in
      return (Expr.Block blk)
  | U.Record_literal {ty; members} ->
      let%bind ty =
        match Type.type_untyped ty ~ctxt:ctxt.type_context with
        | Result.Ok Type.User_defined idx -> return idx
        | _ -> failwith "blaaagh"
      in
      let%bind members =
        let rec map (xs: (string * U.t Spanned.t) Spanned.t list) =
          match xs with
          | [] -> return []
          | ((name, expr), sp) :: xs ->
              let%bind expr = spanned_bind (type_expression decl ctxt expr) in
              let%bind xs = map xs in
              return (((name, expr), sp) :: xs)
        in
        map members
      in
      return (T.Record_literal {ty; members})
  | U.Record_access (expr, member) ->
      let%bind expr = spanned_bind (type_expression decl ctxt expr) in
      return (T.Record_access (expr, member))


let add_function_declaration (ctxt: t) (unt_func: Untyped_ast.Func.t Spanned.t)
    : t result =
  let module F = Untyped_ast.Func in
  let unt_func, _ = unt_func in
  let {F.name; F.params; F.ret_ty; _} = unt_func in
  let%bind params, parm_sp =
    let rec helper ctxt = function
      | [] -> return []
      | ((name, ty), _) :: xs ->
          let%bind ty =
            match Type.type_untyped ~ctxt ty with
            | Result.Ok ty -> return ty
            | Result.Error Type.Type_not_found name ->
                return_err (Error.Type_not_found name)
          in
          let%bind tys = helper ctxt xs in
          return ((name, ty) :: tys)
    in
    spanned_bind (helper ctxt.type_context params)
  in
  let%bind ret_ty =
    match ret_ty with
    | Some (ret_ty, _) -> (
      match Type.type_untyped ~ctxt:ctxt.type_context ret_ty with
      | Result.Ok ty -> return ty
      | Result.Error Type.Type_not_found name ->
          return_err (Error.Type_not_found name) )
    | None -> return (Type.Builtin Type.Unit)
  in
  (* check for duplicates *)
  let rec check_for_duplicates search = function
    | [] -> None
    | (f, sp) :: _ when String.equal f.Function_declaration.name search ->
        Some (f, sp)
    | _ :: xs -> check_for_duplicates search xs
  in
  match check_for_duplicates name ctxt.function_context with
  | Some (_, sp) ->
      return_err
        (Error.Defined_function_multiple_times {name; original_declaration= sp})
  | None ->
      let decl = (Function_declaration.{name; params; ret_ty}, parm_sp) in
      return {ctxt with function_context= decl :: ctxt.function_context}


let add_function_definition (ctxt: t) (unt_func: Untyped_ast.Func.t Spanned.t)
    : t result =
  let module F = Untyped_ast.Func in
  let unt_func, _ = unt_func in
  let decl =
    let num_funcs = List.length ctxt.function_context in
    let idx = num_funcs - 1 - List.length ctxt.function_definitions in
    let decl, _ = Functions.decl_by_index ctxt.function_context idx in
    assert (String.equal decl.Function_declaration.name unt_func.F.name) ;
    decl
  in
  let%bind body = spanned_bind (type_block decl ctxt unt_func.F.body) in
  let%bind body_ty = type_of_block ctxt decl body in
  if Type.equal body_ty decl.Function_declaration.ret_ty then
    return {ctxt with function_definitions= body :: ctxt.function_definitions}
  else
    return_err
      (Error.Return_type_mismatch
         {expected= decl.Function_declaration.ret_ty; found= body_ty})


let make unt_ast : (t, Error.t * Type.Context.t) Spanned.Result.t =
  let module U = Untyped_ast in
  let rec add_function_declarations ast = function
    | unt_func :: funcs ->
        let%bind new_ast = add_function_declaration ast unt_func in
        add_function_declarations new_ast funcs
    | [] -> return ast
  in
  let rec add_function_definitions ast = function
    | unt_func :: funcs ->
        let%bind new_ast = add_function_definition ast unt_func in
        add_function_definitions new_ast funcs
    | [] -> return ast
  in
  let%bind type_context =
    match Type.Context.make unt_ast.U.types with
    | Result.Ok tc -> return tc
    | Result.Error Type.Context.Duplicate_definitions name ->
        return_err (Error.Defined_type_multiple_times name, Type.Context.empty)
  in
  let ret =
    let ret = {type_context; function_context= []; function_definitions= []} in
    let%bind ret = add_function_declarations ret unt_ast.U.funcs in
    add_function_definitions ret unt_ast.U.funcs
  in
  match ret with
  | Result.Ok o, sp -> (Result.Ok o, sp)
  | Result.Error e, sp -> (Result.Error (e, type_context), sp)
