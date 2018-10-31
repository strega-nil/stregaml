open! Types.Pervasives
module Span = Spanned.Span
module Untyped_ast = Cafec_parse.Ast
module Expr = Ast.Expr

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
       Function_declaration.t Spanned.t list
    -> int
    -> Function_declaration.t Spanned.t

  val expr_by_index : Expr.block Spanned.t list -> int -> Expr.block Spanned.t
end = struct
  let index_by_name ctxt search =
    let rec helper n = function
      | ({Function_declaration.name; _}, _) :: _ when String.equal name search
        ->
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

let find_parameter name lst =
  let rec helper name lst idx =
    match lst with
    | [] -> None
    | (name', ty) :: _ when String.equal name' name -> Some (ty, idx)
    | _ :: xs -> helper name xs (idx + 1)
  in
  helper name lst 0

let rec typeck_block (locals : (string * Type.t) list) (ctxt : t) unt_blk =
  let module U = Untyped_ast in
  let module T = Ast in
  let rec typeck_stmts = function
    | [] -> return []
    | (s, sp) :: xs -> (
      match s with
      | U.Stmt.Expression e ->
          let%bind e = typeck_expression locals ctxt e in
          let%bind xs = typeck_stmts xs in
          return ((T.Stmt.Expression e, sp) :: xs)
      | U.Stmt.Let _ ->
          assert false
          (* let%bind expr = typeck_expression locals ctxt expr in
          let ty =
            match ty with
            | None -> return None
            | Some ty ->
              let%bind ty = Type.of_untyped ty ~ctxt:ctxt.type_context in
              return (Some ty)
          in
          let%bind xs = typeck_stmts xs in
          return ((T.Stmt.Let {name; ty; expr}, sp) :: xs) *)
      )
  in
  let U.Expr.({stmts; expr}), sp = unt_blk in
  let%bind stmts = typeck_stmts stmts in
  let%bind expr =
    match expr with
    | Some e ->
        let%bind e = spanned_bind (typeck_expression locals ctxt e) in
        return (Some e)
    | None -> return None
  in
  (Ok T.Expr.{stmts; expr}, sp)

and typeck_expression (locals : (string * Type.t) list) (ctxt : t) unt_expr =
  let module U = Untyped_ast.Expr in
  let module T = Expr in
  let unt_expr, sp = unt_expr in
  let%bind () = with_span sp in
  match unt_expr with
  | U.Unit_literal ->
      return T.{variant= Unit_literal; ty= Type.Builtin Type.Unit}
  | U.Bool_literal b ->
      return T.{variant= Bool_literal b; ty= Type.Builtin Type.Bool}
  | U.Integer_literal i ->
      return T.{variant= Integer_literal i; ty= Type.Builtin Type.Int}
  | U.If_else {cond; thn; els} -> (
      let%bind cond = spanned_bind (typeck_expression locals ctxt cond) in
      match cond with
      | T.({ty= Type.Builtin Type.Bool; _}), _ ->
          let%bind thn = spanned_bind (typeck_block locals ctxt thn) in
          let%bind els = spanned_bind (typeck_block locals ctxt els) in
          let thn_ty =
            match thn with
            | T.({expr= Some ({ty; _}, _); _}), _ -> ty
            | _ -> Type.Builtin Type.Unit
          in
          let els_ty =
            match els with
            | T.({expr= Some ({ty; _}, _); _}), _ -> ty
            | _ -> Type.Builtin Type.Unit
          in
          if Type.equal thn_ty els_ty then
            return T.{variant= If_else {cond; thn; els}; ty= thn_ty}
          else
            return_err (Error.If_branches_of_differing_type (thn_ty, els_ty))
      | T.({ty; _}), _ -> return_err (Error.If_non_bool ty) )
  | U.Call (callee, args) ->
      let%bind callee = spanned_bind (typeck_expression locals ctxt callee) in
      let rec helper = function
        | [] -> return []
        | x :: xs ->
            let%bind x = spanned_bind (typeck_expression locals ctxt x) in
            let%bind xs = helper xs in
            return (x :: xs)
      in
      let%bind args = helper args in
      let%bind callee_ty =
        match callee with
        | T.({ty= Type.Builtin (Type.Function {params; ret_ty}); _}), _ ->
            let get_arg_type (T.({ty; _}), _) = ty in
            let rec correct_types args params =
              match (args, params) with
              | [], [] -> true
              | arg :: args, parm :: parms ->
                  if not (Type.equal (get_arg_type arg) parm) then false
                  else correct_types args parms
              | _ -> false
            in
            if correct_types args params then return ret_ty
            else
              return_err
                (Error.Invalid_function_arguments
                   {expected= params; found= List.map ~f:get_arg_type args})
        | T.({ty; _}), _ -> return_err (Error.Call_of_non_function ty)
      in
      return T.{variant= Call (callee, args); ty= callee_ty}
  | U.Variable {path= _; name} -> (
    match find_parameter name locals with
    | Some (ty, idx) -> return T.{variant= Local idx; ty}
    | None -> (
      match Functions.index_by_name ctxt.function_context name with
      | None -> (
          let int_ty = Type.(Builtin Int) in
          let less_eq_ty =
            Type.(
              Builtin
                (Function {params= [int_ty; int_ty]; ret_ty= Builtin Bool}))
          in
          let op_ty =
            Type.(
              Builtin (Function {params= [int_ty; int_ty]; ret_ty= int_ty}))
          in
          match name with
          | "LESS_EQ" ->
              return T.{variant= Builtin T.Builtin.Less_eq; ty= less_eq_ty}
          | "ADD" -> return T.{variant= Builtin T.Builtin.Add; ty= op_ty}
          | "SUB" -> return T.{variant= Builtin T.Builtin.Sub; ty= op_ty}
          | "MUL" -> return T.{variant= Builtin T.Builtin.Mul; ty= op_ty}
          | _ -> return_err (Error.Name_not_found name) )
      | Some idx ->
          let ty =
            let decl, _ = Functions.decl_by_index ctxt.function_context idx in
            let rec get_params = function
              | (_, x) :: xs -> x :: get_params xs
              | [] -> []
            in
            let params = get_params decl.Function_declaration.params in
            let ret_ty = decl.Function_declaration.ret_ty in
            Type.Builtin (Type.Function {params; ret_ty})
          in
          return T.{variant= Global_function idx; ty} ) )
  | U.Block blk ->
      let%bind blk = spanned_bind (typeck_block locals ctxt blk) in
      let ty =
        match blk with
        | T.({expr= Some ({ty; _}, _); _}), _ -> ty
        | _ -> Type.Builtin Type.Unit
      in
      return T.{variant= Block blk; ty}
  | U.Record_literal {ty; members} ->
      let%bind ty, ty_sp =
        spanned_bind (Type.of_untyped ty ~ctxt:ctxt.type_context)
      in
      let%bind type_members =
        match Type.structural ty ~ctxt:ctxt.type_context with
        | Type.Structural.Record members -> return members
        | _ -> return_err (Error.Record_literal_non_record_type ty)
      in
      let rec find_in_type_members s = function
        | (name, ty) :: _ when String.equal name s -> Some ty
        | _ :: xs -> find_in_type_members s xs
        | [] -> None
      in
      let rec has_dup el = function
        | [] -> None
        | ((name, _), _) :: _ when String.equal el name -> Some el
        | _ :: xs -> has_dup el xs
      in
      let%bind members =
        let rec map (xs : (string * U.t Spanned.t) Spanned.t list) =
          match xs with
          | [] -> return []
          | ((name, expr), sp) :: xs ->
              let%bind expr =
                spanned_bind (typeck_expression locals ctxt expr)
              in
              let T.({ty; _}), _ = expr in
              let%bind () =
                match find_in_type_members name type_members with
                | Some mty ->
                    if Type.equal mty ty then return ()
                    else
                      return_err
                        (Error.Record_literal_incorrect_type
                           {field= name; field_ty= ty; member_ty= mty})
                | None ->
                    return_err (Error.Record_literal_extra_field (ty, name))
              in
              let%bind () =
                match has_dup name xs with
                | Some name ->
                    return_err (Error.Record_literal_duplicate_members name)
                | None -> return ()
              in
              let%bind xs = map xs in
              return (((name, expr), sp) :: xs)
        in
        map members
      in
      let%bind () =
        let rec check_for_existence type_members members =
          (*
            note: we only need to check for existence, not type correctness
            if the type wasn't correct, we'd find it in map
          *)
          let rec check_for_single name = function
            | ((x, _), _) :: _ when String.equal name x -> true
            | _ :: xs -> check_for_single name xs
            | [] -> false
          in
          match type_members with
          | [] -> return ()
          | (name, ty) :: xs ->
              if check_for_single name members then
                check_for_existence xs members
              else return_err (Error.Record_literal_missing_field (ty, name))
        in
        check_for_existence type_members members
      in
      return T.{variant= Record_literal {ty= (ty, ty_sp); members}; ty}
  | U.Record_access (expr, member) ->
      let%bind expr = spanned_bind (typeck_expression locals ctxt expr) in
      let T.({ty= ety; _}), _ = expr in
      let%bind ty =
        match Type.structural ety ~ctxt:ctxt.type_context with
        | Type.Structural.Record members -> (
            let f (n, _) = String.equal n member in
            match List.find ~f members with
            | Some (_, ty) -> return ty
            | None -> return_err (Error.Record_access_non_member (ety, member))
            )
        | _ -> return_err (Error.Record_access_non_record_type (ety, member))
      in
      return T.{variant= Record_access (expr, member); ty}

let add_function_declaration (ctxt : t)
    (unt_func : Untyped_ast.Func.t Spanned.t) : t result =
  let module F = Untyped_ast.Func in
  let unt_func, _ = unt_func in
  let {F.name; F.params; F.ret_ty; _} = unt_func in
  let%bind params, parm_sp =
    let rec helper ctxt = function
      | [] -> return []
      | (((name, _), ty), _) :: xs ->
          let%bind ty = Type.of_untyped ~ctxt ty in
          let%bind tys = helper ctxt xs in
          return ((name, ty) :: tys)
    in
    spanned_bind (helper ctxt.type_context params)
  in
  let%bind ret_ty =
    match ret_ty with
    | Some ret_ty -> Type.of_untyped ~ctxt:ctxt.type_context ret_ty
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

let add_function_definition (ctxt : t)
    (unt_func : Untyped_ast.Func.t Spanned.t) : t result =
  let module F = Untyped_ast.Func in
  let unt_func, _ = unt_func in
  let decl =
    let num_funcs = List.length ctxt.function_context in
    let idx = num_funcs - 1 - List.length ctxt.function_definitions in
    let decl, _ = Functions.decl_by_index ctxt.function_context idx in
    assert (String.equal decl.Function_declaration.name unt_func.F.name) ;
    decl
  in
  let%bind body, body_sp =
    spanned_bind
      (typeck_block decl.Function_declaration.params ctxt unt_func.F.body)
  in
  let body_ty =
    match body.Ast.Expr.expr with
    | Some e ->
        let Ast.Expr.({ty; _}), _ = e in
        ty
    | None -> Type.Builtin Type.Unit
  in
  if Type.equal body_ty decl.Function_declaration.ret_ty then
    return
      { ctxt with
        function_definitions= (body, body_sp) :: ctxt.function_definitions }
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
    | Result.Ok o, sp -> (Result.Ok o, sp)
    | Result.Error e, sp -> (Result.Error (e, Type.Context.empty), sp)
  in
  let ret =
    let ret = {type_context; function_context= []; function_definitions= []} in
    let%bind ret = add_function_declarations ret unt_ast.U.funcs in
    add_function_definitions ret unt_ast.U.funcs
  in
  match ret with
  | Result.Ok o, sp -> (Result.Ok o, sp)
  | Result.Error e, sp -> (Result.Error (e, type_context), sp)
