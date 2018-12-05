open! Types.Pervasives
module Span = Spanned.Span
module Untyped_ast = Cafec_Parse.Ast
module Expr = Ast.Expr
module Local = Ast.Expr.Local
module Binding = Ast.Binding

module Function_declaration = struct
  type t = {name: Name.t; params: Binding.t list; ret_ty: Type.t}
end

module Infix_group = struct
  type associativity = Untyped_ast.Infix_group.associativity =
    | Assoc_start
    | Assoc_none

  type precedence = Less of int

  type t = {associativity: associativity; precedence: precedence list}
end

type t =
  { type_context: Type.Context.t
  ; infix_group_names: Nfc_string.t list
  ; infix_groups: Infix_group.t list
  ; infix_decls: (Nfc_string.t * int) list
  ; function_context: Function_declaration.t Spanned.t list
  ; function_definitions: Expr.block Spanned.t list }

type 'a result = ('a, Error.t) Spanned.Result.t

module Functions : sig
  val index_by_name :
    Function_declaration.t Spanned.t list -> Name.t -> int option

  val decl_by_index :
       Function_declaration.t Spanned.t list
    -> int
    -> Function_declaration.t Spanned.t

  val expr_by_index : Expr.block Spanned.t list -> int -> Expr.block Spanned.t
end = struct
  let index_by_name ctxt search =
    let rec helper n = function
      | ({Function_declaration.name; _}, _) :: _ when Name.equal name search ->
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

module Bind_order = struct
  type t = Start | End | Unordered

  let negate = function Start -> End | End -> Start | Unordered -> Unordered

  let order ~ctxt op1 op2 =
    let module U = Untyped_ast.Expr in
    let module T = Infix_group in
    let get_infix_group ctxt op =
      let f (name, _) = Nfc_string.equal op name in
      match List.find ~f ctxt.infix_decls with
      | Some (_, idx) -> Some (idx, List.nth_exn ctxt.infix_groups idx)
      | None -> None
    in
    let order_named ctxt op1 op2 =
      let rec order_infix_groups idx info idx2 =
        if idx = idx2 then
          match info.T.associativity with
          | T.Assoc_start -> Some Start
          | T.Assoc_none -> Some Unordered
        else
          let rec check_all_sub_precedences = function
            | [] -> None
            | T.Less idx :: xs -> (
                let info = List.nth_exn ctxt.infix_groups idx in
                (*
              note: since ig < ig2, this means that no matter what,
              ig binds looser than ig2
            *)
                match order_infix_groups idx info idx2 with
                | Some _ -> Some End
                | None -> check_all_sub_precedences xs )
          in
          check_all_sub_precedences info.T.precedence
      in
      let order_infix_groups_comm (idx1, info1) (idx2, info2) =
        match order_infix_groups idx1 info1 idx2 with
        | Some order -> order
        | None -> (
          match order_infix_groups idx2 info2 idx1 with
          | Some order -> negate order
          | None -> Unordered )
      in
      match (get_infix_group ctxt op1, get_infix_group ctxt op2) with
      | Some ig1, Some ig2 -> order_infix_groups_comm ig1 ig2
      | _ -> Unordered
    in
    match (op1, op2) with
    | U.Infix_assign, U.Infix_assign -> Unordered
    | U.Infix_assign, _ -> End
    | _, U.Infix_assign -> Start
    | U.Infix_name op1, U.Infix_name op2 -> order_named ctxt op1 op2
end

let find_local name (lst : Binding.t list) : Local.t option =
  let name, _ = name in
  let f index binding =
    let name', _ = binding.Binding.name in
    if Name.equal name' name then Some Local.{binding; index} else None
  in
  List.find_mapi ~f lst

let value_type ty = Ast.Expr.Type.{category= Value; ty}

let rec typeck_block (locals : Binding.t list) (ctxt : t) unt_blk =
  let module U = Untyped_ast in
  let module T = Ast in
  let rec typeck_stmts locals = function
    | [] -> return ([], locals)
    | (s, sp) :: xs -> (
      match s with
      | U.Stmt.Expression e ->
          let%bind e = typeck_expression locals ctxt e in
          let%bind xs, expr_locals = typeck_stmts locals xs in
          return ((T.Stmt.Expression e, sp) :: xs, expr_locals)
      | U.Stmt.Let {name; is_mut; ty; expr} ->
          let%bind expr = spanned_bind (typeck_expression locals ctxt expr) in
          let T.Expr.({ty= expr_ty; _}), _ = expr in
          let T.Expr.Type.({ty= expr_ty; _}) = expr_ty in
          let%bind ty =
            match ty with
            | None -> return (expr_ty, Spanned.Span.made_up)
            | Some ty ->
                let%bind ty, ty_sp =
                  spanned_bind (Type.of_untyped ty ~ctxt:ctxt.type_context)
                in
                if Type.equal expr_ty ty then return (ty, ty_sp)
                else
                  let name, _ = name in
                  return_err
                    (Error.Incorrect_let_type {name; let_ty= ty; expr_ty})
          in
          let mutability = if is_mut then Type.Mutable else Type.Immutable in
          let binding = Binding.{name; mutability; ty} in
          let locals = binding :: locals in
          let%bind xs, expr_locals = typeck_stmts locals xs in
          return ((T.Stmt.Let {binding; expr}, sp) :: xs, expr_locals) )
  in
  let U.Expr.({stmts; expr}), sp = unt_blk in
  let%bind stmts, locals = typeck_stmts locals stmts in
  let%bind expr =
    match expr with
    | Some e ->
        let%bind e = spanned_bind (typeck_expression locals ctxt e) in
        return (Some e)
    | None -> return None
  in
  (Ok T.Expr.{stmts; expr}, sp)

and typeck_call callee args =
  let module T = Ast.Expr in
  let callee_ty = T.base_type_sp callee in
  let%bind ret_ty =
    match callee_ty with
    | Type.Builtin (Type.Function {params; ret_ty}) ->
        let correct_types args params =
          let f a p = Type.equal (T.base_type_sp a) p in
          if List.length args <> List.length params then false
          else List.for_all2_exn ~f args params
        in
        if correct_types args params then return ret_ty
        else
          return_err
            (Error.Invalid_function_arguments
               {expected= params; found= List.map ~f:T.base_type_sp args})
    | ty -> return_err (Error.Call_of_non_function ty)
  in
  return T.{variant= Call (callee, args); ty= value_type ret_ty}

and typeck_infix_list (locals : Binding.t list) (ctxt : t) e0 rest =
  let module U = Untyped_ast.Expr in
  let module T = Ast.Expr in
  let make_op_expr op e0 e1 =
    match op with
    | U.Infix_assign, _ -> (
        let source, dest = (e1, e0) in
        let T.Type.({category= dest_cat; ty= dest_ty}) = T.full_type_sp dest in
        let source_ty = T.base_type_sp source in
        if not (Type.equal dest_ty source_ty) then
          return_err
            (Error.Assignment_to_incompatible_type
               {dest= dest_ty; source= source_ty})
        else
          match dest_cat with
          | T.Type.Value -> return_err Error.Assignment_to_value
          | T.Type.Place {mutability= Type.Immutable} ->
              return_err Error.Assignment_to_immutable_place
          | T.Type.Place {mutability= Type.Mutable} ->
              let ty = value_type (Type.Builtin Type.Unit) in
              return T.{variant= Assign {dest; source}; ty} )
    | U.Infix_name name, sp ->
        let name = (Name.{string= name; kind= Infix}, sp) in
        let name = (U.Name U.{path= []; name}, sp) in
        let%bind callee = spanned_bind (typeck_expression locals ctxt name) in
        typeck_call callee [e0; e1]
  in
  match rest with
  | [] -> spanned_lift e0
  | ((op1, sp1), e1) :: rest -> (
      let%bind e1 = spanned_bind (typeck_expression locals ctxt e1) in
      match rest with
      | [] -> make_op_expr (op1, sp1) e0 e1
      | ((op2, sp2), _) :: _ -> (
        match Bind_order.order ~ctxt op1 op2 with
        | Bind_order.Start ->
            let%bind lhs = spanned_bind (make_op_expr (op1, sp1) e0 e1) in
            typeck_infix_list locals ctxt lhs rest
        | Bind_order.End ->
            let%bind rhs =
              spanned_bind (typeck_infix_list locals ctxt e1 rest)
            in
            make_op_expr (op1, sp1) e0 rhs
        | Bind_order.Unordered ->
            return_err
              (Error.Unordered_operators {op1= (op1, sp1); op2= (op2, sp2)}) )
      )

and typeck_expression (locals : Binding.t list) (ctxt : t) unt_expr =
  let module U = Untyped_ast.Expr in
  let module T = Expr in
  let%bind unt_expr = spanned_lift unt_expr in
  match unt_expr with
  | U.Unit_literal ->
      let ty = value_type (Type.Builtin Type.Unit) in
      return T.{variant= Unit_literal; ty}
  | U.Bool_literal b ->
      let ty = value_type (Type.Builtin Type.Bool) in
      return T.{variant= Bool_literal b; ty}
  | U.Integer_literal i ->
      let ty = value_type (Type.Builtin Type.Int32) in
      return T.{variant= Integer_literal i; ty}
  | U.Match {cond; arms} ->
      let%bind cond = spanned_bind (typeck_expression locals ctxt cond) in
      let cond_ty = T.base_type_sp cond in
      let%bind variants =
        match Type.structural cond_ty ~ctxt:ctxt.type_context with
        | Type.Structural.Variant variants -> return variants
        | _ -> return_err (Error.Match_non_variant_type cond_ty)
      in
      let variants_len = List.length variants in
      let branch_some = Array.create ~len:variants_len false in
      let branches = Array.create ~len:variants_len T.{stmts= []; expr= None} in
      let f ((pat, _), (blk, _)) = assert false in
      assert false
  | U.If_else {cond; thn; els} -> (
      let%bind cond = spanned_bind (typeck_expression locals ctxt cond) in
      match T.base_type_sp cond with
      | Type.Builtin Type.Bool ->
          let%bind thn = spanned_bind (typeck_block locals ctxt thn) in
          let%bind els = spanned_bind (typeck_block locals ctxt els) in
          let typeof (e, _) =
            Option.value_map ~f:T.base_type_sp
              ~default:(Type.Builtin Type.Unit) e.T.expr
          in
          let thn_ty = typeof thn in
          let els_ty = typeof els in
          if Type.equal thn_ty els_ty then
            return T.{variant= If_else {cond; thn; els}; ty= value_type thn_ty}
          else
            return_err (Error.If_branches_of_differing_type (thn_ty, els_ty))
      | ty -> return_err (Error.If_non_bool ty) )
  | U.Builtin ((name, _), args) -> (
      let%bind arg1, arg2 =
        match args with
        | [a1; a2] -> return (a1, a2)
        | args ->
            return_err
              (Error.Builtin_mismatched_arity
                 {name; expected= 2; found= List.length args})
      in
      let%bind arg1 = spanned_bind (typeck_expression locals ctxt arg1) in
      let%bind arg2 = spanned_bind (typeck_expression locals ctxt arg2) in
      let a1_ty, a2_ty = (T.base_type_sp arg1, T.base_type_sp arg2) in
      let%bind () =
        match (a1_ty, a2_ty) with
        | Type.Builtin Type.Int32, Type.Builtin Type.Int32 -> return ()
        | _ ->
            return_err
              (Error.Builtin_invalid_arguments {name; found= [a1_ty; a2_ty]})
      in
      match (name :> string) with
      | "less_eq" ->
          return
            { T.variant= T.Builtin (T.Builtin.Less_eq (arg1, arg2))
            ; T.ty= value_type (Type.Builtin Type.Bool) }
      | "add" ->
          return
            { T.variant= T.Builtin (T.Builtin.Add (arg1, arg2))
            ; T.ty= value_type (Type.Builtin Type.Int32) }
      | "sub" ->
          return
            { T.variant= T.Builtin (T.Builtin.Sub (arg1, arg2))
            ; T.ty= value_type (Type.Builtin Type.Int32) }
      | "mul" ->
          return
            { T.variant= T.Builtin (T.Builtin.Mul (arg1, arg2))
            ; T.ty= value_type (Type.Builtin Type.Int32) }
      | _ -> return_err (Error.Unknown_builtin name) )
  | U.Call (callee, args) ->
      let%bind callee = spanned_bind (typeck_expression locals ctxt callee) in
      let f x = spanned_bind (typeck_expression locals ctxt x) in
      let%bind args = return_map ~f args in
      typeck_call callee args
  | U.Prefix_operator ((op, sp), expr) ->
      let name = (Name.{string= op; kind= Prefix}, sp) in
      let name = (U.Name U.{path= []; name}, sp) in
      let%bind callee = spanned_bind (typeck_expression locals ctxt name) in
      let%bind arg = spanned_bind (typeck_expression locals ctxt expr) in
      typeck_call callee [arg]
  | U.Infix_list (first, rest) ->
      let%bind first = spanned_bind (typeck_expression locals ctxt first) in
      typeck_infix_list locals ctxt first rest
  | U.Name U.({path= []; name}) -> (
    match find_local name locals with
    | Some loc ->
        let Binding.({ty= ty, _; mutability; _}) = loc.Local.binding in
        let ty = T.Type.{ty; category= Place {mutability}} in
        return T.{variant= Local loc; ty}
    | None -> (
        let name, _ = name in
        match Functions.index_by_name ctxt.function_context name with
        | None -> return_err (Error.Name_not_found name)
        | Some idx ->
            let ty =
              let decl, _ =
                Functions.decl_by_index ctxt.function_context idx
              in
              let params =
                let f Binding.({ty= ty, _; _}) = ty in
                List.map ~f decl.Function_declaration.params
              in
              let ret_ty = decl.Function_declaration.ret_ty in
              value_type (Type.Builtin (Type.Function {params; ret_ty}))
            in
            return T.{variant= Global_function idx; ty} ) )
  | U.Name U.({path= [ty_name]; name= name, _}) ->
      let%bind ty =
        let ty_name, sp = ty_name in
        let ty = (Untyped_ast.Type.Named ty_name, sp) in
        Type.of_untyped ty ~ctxt:ctxt.type_context
      in
      let%bind type_members =
        match Type.structural ty ~ctxt:ctxt.type_context with
        | Type.Structural.Variant members -> return members
        | _ -> return_err (Error.Name_not_found_in_type (ty, name))
      in
      let%bind nfc_name =
        match name with
        | Name.({string; kind= Identifier}) -> return string
        | _ -> return_err (Error.Name_not_found_in_type (ty, name))
      in
      let%bind idx, ty_member =
        let rec helper idx = function
          | [] -> return_err (Error.Name_not_found_in_type (ty, name))
          | (name, ty) :: _ when Nfc_string.equal nfc_name name ->
              return (idx, ty)
          | _ :: xs -> helper (idx + 1) xs
        in
        helper 0 type_members
      in
      let ty = Type.(Builtin (Function {params= [ty_member]; ret_ty= ty})) in
      return T.{variant= Constructor (ty, idx); ty= value_type ty}
  | U.Name _ -> failwith "paths with size > 1 not supported"
  | U.Block blk ->
      let%bind blk = spanned_bind (typeck_block locals ctxt blk) in
      let ty =
        match blk with
        | T.({expr= Some ({ty; _}, _); _}), _ -> ty
        | _ -> value_type (Type.Builtin Type.Unit)
      in
      return T.{variant= Block blk; ty}
  | U.Reference {is_mut; place} ->
      let%bind place = spanned_bind (typeck_expression locals ctxt place) in
      let mutability = if is_mut then Type.Mutable else Type.Immutable in
      let pointee_ty = T.full_type_sp place in
      let%bind ty =
        match pointee_ty with
        | T.Type.({category= Value; _}) ->
            return_err (Error.Reference_taken_to_value mutability)
        | T.Type.({category= Place {mutability= pmut}; ty= pointee}) -> (
          match (mutability, pmut) with
          | Type.Immutable, _ | Type.Mutable, Type.Mutable ->
              return
                (value_type
                   (Type.Builtin (Type.Reference {mutability; pointee})))
          | Type.Mutable, Type.Immutable ->
              return_err Error.Mutable_reference_taken_to_immutable_place )
      in
      return T.{variant= Reference {mutability; place}; ty}
  | U.Dereference value -> (
      let%bind value = spanned_bind (typeck_expression locals ctxt value) in
      match T.base_type_sp value with
      | Type.Builtin (Type.Reference {mutability; pointee}) ->
          let ty = T.Type.{category= Place {mutability}; ty= pointee} in
          return T.{variant= Dereference value; ty}
      | ty -> return_err (Error.Dereference_of_non_reference ty) )
  | U.Record_literal {ty; members} ->
      let%bind ty, ty_sp =
        spanned_bind (Type.of_untyped ty ~ctxt:ctxt.type_context)
      in
      let%bind type_members =
        match Type.structural ty ~ctxt:ctxt.type_context with
        | Type.Structural.Record members -> return members
        | _ -> return_err (Error.Record_literal_non_record_type ty)
      in
      let find_in_type_members s members =
        let rec helper idx = function
          | (name, ty) :: _ when Nfc_string.equal name s -> Some (idx, ty)
          | _ :: xs -> helper (idx + 1) xs
          | [] -> None
        in
        helper 0 members
      in
      let members_len = List.length type_members in
      let members_init : bool array = Array.create ~len:members_len false in
      let members_typed : T.t array =
        Array.create ~len:members_len T.unit_value
      in
      let%bind () =
        let rec helper = function
          | [] -> return ()
          | ((name, expr), _) :: xs ->
              let%bind expr = typeck_expression locals ctxt expr in
              let ty = T.base_type expr in
              let%bind idx =
                match find_in_type_members name type_members with
                | Some (idx, mty) ->
                    if Type.equal mty ty then return idx
                    else
                      return_err
                        (Error.Record_literal_incorrect_type
                           {field= name; field_ty= ty; member_ty= mty})
                | None ->
                    return_err (Error.Record_literal_extra_field (ty, name))
              in
              let%bind () =
                if members_init.(idx) then
                  return_err (Error.Record_literal_duplicate_members name)
                else (
                  members_typed.(idx) <- expr ;
                  members_init.(idx) <- true ;
                  return () )
              in
              helper xs
        in
        helper members
      in
      let%bind () =
        let f _ el = not el in
        match Array.findi ~f members_init with
        | None -> return ()
        | Some (idx, _) ->
            let name, ty = List.nth_exn type_members idx in
            return_err (Error.Record_literal_missing_field (ty, name))
      in
      return
        T.
          { variant= Record_literal {ty= (ty, ty_sp); members= members_typed}
          ; ty= value_type ty }
  | U.Record_access (expr, member) ->
      let%bind expr = spanned_bind (typeck_expression locals ctxt expr) in
      let ety = T.full_type_sp expr in
      let%bind idx, ty =
        let T.Type.({category= ecat; ty= ebty}) = ety in
        match Type.structural ebty ~ctxt:ctxt.type_context with
        | Type.Structural.Record members -> (
            let f _ (n, _) = Nfc_string.equal n member in
            match List.findi ~f members with
            | Some (idx, (_, ty)) -> return (idx, T.Type.{category= ecat; ty})
            | None ->
                return_err (Error.Record_access_non_member (ebty, member)) )
        | _ -> return_err (Error.Record_access_non_record_type (ebty, member))
      in
      return T.{variant= Record_access (expr, idx); ty}

let find_infix_group_name ctxt id =
  let f _ name = Nfc_string.equal id name in
  match List.findi ~f ctxt.infix_group_names with
  | Some (i, _) -> Some i
  | None -> None

let add_infix_group_name (ctxt : t)
    (group : Untyped_ast.Infix_group.t Spanned.t) : t result =
  let module U = Untyped_ast.Infix_group in
  let {U.name= name, _; _}, _ = group in
  match find_infix_group_name ctxt name with
  | Some _ -> return_err (Error.Infix_group_defined_multiple_times name)
  | None -> return {ctxt with infix_group_names= name :: ctxt.infix_group_names}

let add_infix_group (ctxt : t) (group : Untyped_ast.Infix_group.t Spanned.t) :
    t result =
  let module U = Untyped_ast.Infix_group in
  let {U.associativity; U.precedence; _}, _ = group in
  let f (U.Less (id, _)) =
    match find_infix_group_name ctxt id with
    | Some idx -> return (Infix_group.Less idx)
    | None -> return_err (Error.Infix_group_not_found id)
  in
  let%bind precedence = return_map ~f precedence in
  let group = Infix_group.{associativity; precedence} in
  return {ctxt with infix_groups= group :: ctxt.infix_groups}

let add_infix_decl (ctxt : t)
    (unt_infix_decl : Untyped_ast.Infix_declaration.t Spanned.t) : t result =
  let module U = Untyped_ast.Infix_declaration in
  let%bind {U.name= name, _; U.group= group, _} =
    spanned_lift unt_infix_decl
  in
  match find_infix_group_name ctxt group with
  | None -> return_err (Error.Infix_group_not_found group)
  | Some idx ->
      let decl = (name, idx) in
      return {ctxt with infix_decls= decl :: ctxt.infix_decls}

let add_function_declaration (ctxt : t)
    (unt_func : Untyped_ast.Func.t Spanned.t) : t result =
  let module F = Untyped_ast.Func in
  let unt_func, _ = unt_func in
  let {F.name; F.params; F.ret_ty; _} = unt_func in
  let%bind params, parm_sp =
    let f ((name, ty), _) =
      let%bind ty =
        spanned_bind (Type.of_untyped ~ctxt:ctxt.type_context ty)
      in
      return Binding.{name; mutability= Type.Immutable; ty}
    in
    spanned_bind (return_map ~f params)
  in
  let%bind ret_ty =
    match ret_ty with
    | Some ret_ty -> Type.of_untyped ~ctxt:ctxt.type_context ret_ty
    | None -> return (Type.Builtin Type.Unit)
  in
  (* check for duplicates *)
  let rec check_for_duplicates search = function
    | [] -> None
    | (f, sp) :: _ when Name.equal f.Function_declaration.name search ->
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
    assert (Name.equal decl.Function_declaration.name unt_func.F.name) ;
    decl
  in
  let%bind body, body_sp =
    spanned_bind
      (typeck_block decl.Function_declaration.params ctxt unt_func.F.body)
  in
  let body_ty =
    match body.Expr.expr with
    | Some e -> Expr.base_type_sp e
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
  let%bind type_context =
    match Type.Context.make unt_ast.U.types with
    | Result.Ok o, sp -> (Result.Ok o, sp)
    | Result.Error e, sp -> (Result.Error (e, Type.Context.empty), sp)
  in
  let ret =
    let init =
      { type_context
      ; infix_group_names= []
      ; infix_groups= []
      ; infix_decls= []
      ; function_context= []
      ; function_definitions= [] }
    in
    let%bind init =
      return_fold unt_ast.U.infix_groups ~init ~f:add_infix_group_name
    in
    let%bind init =
      return_fold unt_ast.U.infix_groups ~init ~f:add_infix_group
    in
    let%bind init =
      return_fold unt_ast.U.infix_decls ~init ~f:add_infix_decl
    in
    let%bind init =
      return_fold unt_ast.U.funcs ~init ~f:add_function_declaration
    in
    return_fold unt_ast.U.funcs ~init ~f:add_function_definition
  in
  match ret with
  | Result.Ok o, sp -> (Result.Ok o, sp)
  | Result.Error e, sp -> (Result.Error (e, type_context), sp)
