open! Types.Pervasives
module Span = Spanned.Span
module Untyped_ast = Cafec_Parse.Ast
module Untyped_expr = Untyped_ast.Expr
module Untyped_type = Cafec_Parse.Type
module Attribute = Untyped_ast.Attribute
module Expr = Ast.Expr
module Local = Ast.Expr.Local
module Binding = Ast.Binding

module Function_declaration = struct
  type t =
    | Declaration :
        { name : Name.anyfix Name.t
        ; params : Binding.t Array.t
        ; ret_ty : Type.Category.any Type.t
        ; attributes : Attribute.t Spanned.t list }
        -> t

  let name (Declaration {name; _}) = name

  let params (Declaration {params; _}) = params

  let ret_ty (Declaration {ret_ty; _}) = ret_ty

  let attributes (Declaration r) = r.attributes
end

module Infix_group = struct
  type associativity = Untyped_ast.Infix_group.associativity =
    | Assoc_start : associativity
    | Assoc_end : associativity
    | Assoc_none : associativity

  type precedence = Less : int -> precedence

  type t =
    | Infix_group :
        { associativity : associativity
        ; precedence : precedence Array.t }
        -> t

  let associativity (Infix_group {associativity; _}) = associativity

  let precedence (Infix_group {precedence; _}) = precedence
end

module Context = struct
  type t =
    | Context :
        { type_context : Type.Context.t
        ; infix_group_names : Nfc_string.t Array.t
        ; infix_groups : Infix_group.t Array.t
        ; infix_decls : (Name.infix Name.t * int) Array.t
        ; entrypoint : int option
        ; function_context : Function_declaration.t Spanned.t Array.t
        ; function_definitions : Expr.Block.t Spanned.t Array.t }
        -> t

  let type_context (Context r) = r.type_context

  let infix_group_names (Context r) = r.infix_group_names

  let infix_groups (Context r) = r.infix_groups

  let infix_decls (Context r) = r.infix_decls

  let function_context (Context r) = r.function_context

  let function_definitions (Context r) = r.function_definitions

  let entrypoint (Context r) = r.entrypoint

  let with_type_context (Context r) type_context =
    Context {r with type_context}

  let with_infix_group_names (Context r) infix_group_names =
    Context {r with infix_group_names}

  let with_infix_groups (Context r) infix_groups =
    Context {r with infix_groups}

  let with_infix_decls (Context r) infix_decls =
    Context {r with infix_decls}

  let with_function_context (Context r) function_context =
    Context {r with function_context}

  let with_function_definitions (Context r) function_definitions =
    Context {r with function_definitions}

  let with_entrypoint (Context r) entrypoint =
    Context {r with entrypoint}
end

include Context

type 'a result = ('a, Error.t) Spanned.Result.t

let name_not_found_in : type f a.
    Type.Category.value Type.t -> f Name.t Spanned.t -> a result =
 fun ty (name, sp) ->
  let%bind () = with_span sp in
  return_err
    (Error.Name_not_found_in_type {ty; name = Name.erase name})

module Compound_type = struct
  type t =
    | Variant
    | Record
end

let find_field :
       _ Name.t
    -> fields:Type.Representation.field Spanned.t Array.t
    -> (int * Type.Category.value Type.t) option =
 fun name ~fields ->
  match Name.nonfix name with
  | Some (Name.Name {string; kind; _}) -> (
    match kind with
    | Name.Identifier -> (
        let nfc_name = string in
        let f _ (((name, _), _), _) = Nfc_string.equal nfc_name name in
        match Array.findi fields ~f with
        | Some (idx, ((_, (field_ty, _)), _)) -> Some (idx, field_ty)
        | None -> None )
    | Name.Operator -> None )
  | None -> None

let find_variant :
       _ Name.t
    -> variants:Type.Representation.variant Spanned.t Array.t
    -> (int * Type.Category.value Type.t option) option =
 fun name ~variants ->
  match Name.nonfix name with
  | Some (Name.Name {string; kind; _}) -> (
    match kind with
    | Name.Identifier -> (
        let nfc_name = string in
        let f _ (((name, _), _), _) = Nfc_string.equal nfc_name name in
        match Array.findi variants ~f with
        | Some (idx, ((_, field_ty), _)) -> (
          match field_ty with
          | Some (fty, _) -> Some (idx, Some fty)
          | None -> Some (idx, None) )
        | None -> None )
    | Name.Operator -> None )
  | None -> None

module Functions : sig
  val index_by_name :
    Function_declaration.t Spanned.t Array.t -> _ Name.t -> int option
end = struct
  let index_by_name ctxt search =
    let module D = Function_declaration in
    let f _ (D.Declaration {name; _}, _) = Name.equal name search in
    match Array.findi ~f ctxt with
    | Some (idx, _) -> Some idx
    | None -> None
end

module Bind_order = struct
  type t =
    | Start
    | End
    | Unordered

  let negate = function
    | Start -> End
    | End -> Start
    | Unordered -> Unordered

  let order ~ctxt op1 op2 =
    let module U = Untyped_expr in
    let module T = Infix_group in
    let get_infix_group ctxt op =
      let f (name, _) = Name.equal op name in
      match Array.find ~f (infix_decls ctxt) with
      | Some (_, idx) -> Some (idx, (infix_groups ctxt).(idx))
      | None -> None
    in
    let order_named ctxt op1 op2 =
      let rec order_infix_groups idx info idx2 =
        if idx = idx2
        then
          match T.associativity info with
          | T.Assoc_start -> Some Start
          | T.Assoc_end -> Some End
          | T.Assoc_none -> Some Unordered
        else
          let precedence info =
            (*
              note: since ig < ig2, this means that no matter what,
              ig binds looser than ig2
            *)
            match order_infix_groups idx info idx2 with
            | Some _ -> Some End
            | None -> None
          in
          Array.find_map ~f:precedence (infix_groups ctxt)
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
    let name', _ = Binding.name binding in
    if Name.equal name' name
    then Some (Local.Local {binding; index})
    else None
  in
  List.find_mapi ~f lst

let rec typeck_block (locals : Binding.t list) (ctxt : t) unt_blk =
  let module U = Untyped_ast in
  let module T = Ast in
  (*
    TODO: fix this
    probably want to do a fold on locals?
  *)
  let rec typeck_stmts locals = function
    | [] -> return ([], locals)
    | (s, sp) :: xs -> (
      match s with
      | U.Stmt.Expression e ->
          let%bind e = typeck_expression locals ctxt e in
          let%bind xs, expr_locals = typeck_stmts locals xs in
          return ((T.Stmt.Expression e, sp) :: xs, expr_locals)
      | U.Stmt.Let {name; is_mut; ty; expr} ->
          let%bind expr =
            spanned_bind (typeck_expression locals ctxt expr)
          in
          let expr_ty = T.Expr.full_type_sp expr in
          let%bind ty =
            match ty with
            | None -> return expr_ty
            | Some ty ->
                let%bind ty =
                  Type.of_untyped ty ~ctxt:(type_context ctxt)
                in
                if Type.compatible expr_ty ty
                then return ty
                else
                  let name, _ = name in
                  return_err
                    (Error.Incorrect_let_type
                       {name; let_ty = ty; expr_ty})
          in
          let binding = Binding.Binding {name; is_mut; ty} in
          let locals = binding :: locals in
          let%bind xs, expr_locals = typeck_stmts locals xs in
          return ((T.Stmt.Let {binding; expr}, sp) :: xs, expr_locals)
      )
  in
  let U.Expr.Block.Block {stmts; expr}, sp = unt_blk in
  let%bind stmts, locals = typeck_stmts locals stmts in
  let stmts = Array.of_list stmts in
  let%bind expr =
    match expr with
    | Some e ->
        let%bind e = spanned_bind (typeck_expression locals ctxt e) in
        return (Some e)
    | None -> return None
  in
  (Ok (T.Expr.Block.Block {stmts; expr}), sp)

and typeck_call callee args =
  let module T = Ast.Expr in
  let callee_ty = T.base_type_sp callee in
  let%bind ret_ty =
    match callee_ty with
    | Type.Structural (Type.Structural.Function {params; ret_ty}) ->
        let correct_types args params =
          let f (a, p) = Type.compatible (T.full_type_sp a) p in
          if Array.length args <> Array.length params
          then false
          else
            Sequence.for_all ~f
              (Sequence.zip
                 (Array.to_sequence args)
                 (Array.to_sequence params))
        in
        if correct_types args params
        then return ret_ty
        else
          return_err
            (Error.Invalid_function_arguments
               { expected = params
               ; found = Array.map ~f:T.full_type_sp args })
    | ty -> return_err (Error.Call_of_non_function ty)
  in
  return (T.Expr {variant = T.Call (callee, args); ty = ret_ty})

and typeck_infix_list (locals : Binding.t list) (ctxt : t) e0 rest =
  let module U = Untyped_expr in
  let module T = Ast.Expr in
  let make_op_expr op e0 e1 =
    match op with
    | U.Infix_assign, _ -> (
        let source, dest = (e1, e0) in
        let dest_ty, dest_cat =
          let full = T.full_type_sp dest in
          (Type.value_type full, Type.category full)
        in
        let source_ty = T.base_type_sp source in
        if not (Type.equal dest_ty source_ty)
        then
          return_err
            (Error.Assignment_to_incompatible_type
               {dest = dest_ty; source = source_ty})
        else
          let module C = Type.Category in
          match dest_cat with
          | C.Any C.Value -> return_err Error.Assignment_to_value
          | C.Any (C.Place C.Immutable) ->
              return_err Error.Assignment_to_immutable_place
          | C.Any (C.Place C.Mutable) ->
              let ty = Type.erase Type.unit in
              return (T.Expr {variant = T.Assign {dest; source}; ty})
          | C.Any (C.Any _) ->
              failwith
                "un-normalized Any type returned from Type.category" )
    | U.Infix_name name, sp ->
        let name = (name, sp) in
        let name = (U.Name (U.Qualified {path = []; name}), sp) in
        let%bind callee =
          spanned_bind (typeck_expression locals ctxt name)
        in
        typeck_call callee (Array.binary e0 e1)
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
            let%bind lhs =
              spanned_bind (make_op_expr (op1, sp1) e0 e1)
            in
            typeck_infix_list locals ctxt lhs rest
        | Bind_order.End ->
            let%bind rhs =
              spanned_bind (typeck_infix_list locals ctxt e1 rest)
            in
            make_op_expr (op1, sp1) e0 rhs
        | Bind_order.Unordered ->
            return_err
              (Error.Unordered_operators
                 {op1 = (op1, sp1); op2 = (op2, sp2)}) ) )

and typeck_match :
       t
    -> Binding.t list
    -> Untyped_expr.t Spanned.t
    -> Untyped_expr.match_arm list
    -> Expr.t result =
 fun ctxt locals cond parse_arms ->
  let module U = Untyped_expr in
  let%bind cond = spanned_bind (typeck_expression locals ctxt cond) in
  let cond_ty = Expr.base_type_sp cond in
  let%bind variants =
    match Type.representation cond_ty ~ctxt:(type_context ctxt) with
    | Type.Representation.Variant {variants} -> return variants
    | _ -> return_err (Error.Match_non_variant_type cond_ty)
  in
  let arms_ty = ref None in
  let%bind arms =
    let f (Untyped_expr.Match_arm {pattern; block}) =
      let U.Pattern {constructor; binding}, _ = pattern in
      let constructor, _ = constructor in
      let%bind cty, arm_name =
        match U.qualified_path constructor with
        | [(ty_name, sp)] ->
            let ty = (Untyped_type.Named ty_name, sp) in
            let%bind ty =
              Type.of_untyped ty ~ctxt:(type_context ctxt)
            in
            return (ty, U.qualified_name constructor)
        | _ -> failwith "paths with size <> 1 not supported"
      in
      let%bind () =
        if not (Type.equal cty cond_ty)
        then
          return_err
            (Error.Pattern_of_wrong_type
               {expected = cond_ty; found = cty})
        else return ()
      in
      let%bind index, bind_ty =
        match find_variant ~variants (fst arm_name) with
        | Some (idx, Some ty) -> return (idx, Some (Type.erase ty))
        | Some (idx, None) -> return (idx, None)
        | None -> name_not_found_in cond_ty arm_name
      in
      let%bind locals =
        match (bind_ty, binding) with
        | None, None -> return locals
        | Some ty, Some name ->
            let binding =
              Ast.Binding.Binding {name; is_mut = false; ty}
            in
            return (binding :: locals)
        | Some _, None -> return_err Error.Match_not_binding_data
        | None, Some _ -> return_err Error.Match_binding_without_data
      in
      let%bind block = spanned_bind (typeck_block locals ctxt block) in
      let%bind () =
        match !arms_ty with
        | None ->
            arms_ty := Some (Expr.Block.base_type_sp block) ;
            return ()
        | Some ty ->
            let arm_ty = Expr.Block.base_type_sp block in
            if Type.equal arm_ty ty
            then return ()
            else
              return_err
                (Error.Match_branches_of_different_type
                   {expected = ty; found = arm_ty})
      in
      return (index, (bind_ty, block))
    in
    let tmp = Return.Array.of_list_map_unordered ~f parse_arms in
    match tmp with
    | Result.Ok o -> o
    | Result.Error (Array.Empty_cell idx) ->
        let ((name, _), _), _ = variants.(idx) in
        return_err (Error.Match_missing_branch name)
    | Result.Error (Array.Duplicate idx) ->
        let ((name, _), _), _ = variants.(idx) in
        return_err (Error.Match_repeated_branches name)
  in
  let variant = Expr.Match {cond; arms} in
  let ty =
    match !arms_ty with
    | Some ty -> Type.erase ty
    | None ->
        (* technically should be bottom, but _shrug_ *)
        Type.erase Type.unit
  in
  return (Expr.Expr {variant; ty})

and typeck_name : type a.
       t
    -> Binding.t list
    -> Nfc_string.t Spanned.t list
    -> a Name.t Spanned.t
    -> Expr.t result =
 fun ctxt locals path name ->
  match path with
  | [] -> (
    match find_local name locals with
    | Some loc ->
        let (Binding.Binding {ty; is_mut; _}) = Local.binding loc in
        let ty = Type.erase (Type.local_type ~is_mut ty) in
        return (Expr.Expr {variant = Expr.Local loc; ty})
    | None -> (
        let name, _ = name in
        match Functions.index_by_name (function_context ctxt) name with
        | None -> return_err (Error.Name_not_found (Name.erase name))
        | Some idx ->
            let ty =
              let decl, _ = (function_context ctxt).(idx) in
              let params =
                let f (Binding.Binding {ty; _}) = ty in
                Array.map ~f (Function_declaration.params decl)
              in
              let ret_ty = Function_declaration.ret_ty decl in
              Type.erase
                (Type.Structural
                   (Type.Structural.Function {params; ret_ty}))
            in
            return (Expr.Expr {variant = Expr.Global_function idx; ty})
        ) )
  | [ty_name] -> (
      let%bind variant_ty =
        let ty_name, sp = ty_name in
        let ty = (Untyped_type.Named ty_name, sp) in
        Type.of_untyped ty ~ctxt:(type_context ctxt)
      in
      let%bind variants =
        match
          Type.representation ~ctxt:(type_context ctxt) variant_ty
        with
        | Type.Representation.Variant {variants} -> return variants
        | _ -> name_not_found_in variant_ty name
      in
      let%bind idx, ty_member =
        match find_variant ~variants (fst name) with
        | Some x -> return x
        | None -> name_not_found_in variant_ty name
      in
      match ty_member with
      | Some ty ->
          let module S = Type.Structural in
          let params = Array.unary (Type.erase ty) in
          let ret_ty = Type.erase variant_ty in
          let ty = Type.Structural (S.Function {params; ret_ty}) in
          let ty = Type.erase ty in
          let variant = Expr.Constructor (variant_ty, idx) in
          return (Expr.Expr {variant; ty})
      | None ->
          let ty = Type.erase variant_ty in
          let variant = Expr.Nilary_variant (variant_ty, idx) in
          return (Expr.Expr {variant; ty}) )
  | _ -> failwith "paths with size > 1 not supported"

and typeck_record_literal :
       t
    -> Binding.t list
    -> Type.Category.value Untyped_type.t Spanned.t
    -> (Name.nonfix Name.t * Untyped_expr.t Spanned.t) Spanned.t list
    -> Expr.t result =
 fun ctxt locals ty fields ->
  let%bind ty, ty_sp =
    spanned_bind (Type.of_untyped ty ~ctxt:(type_context ctxt))
  in
  let%bind type_fields =
    match Type.representation ty ~ctxt:(type_context ctxt) with
    | Type.Representation.Record {fields} -> return fields
    | _ -> return_err (Error.Record_literal_non_record_type ty)
  in
  let find_field = find_field ~fields:type_fields in
  let%bind fields_typed =
    let fields_len = Array.length type_fields in
    let f ((name, expr), _) =
      let%bind expr = typeck_expression locals ctxt expr in
      let ety = Expr.base_type expr in
      let%bind idx =
        match find_field name with
        | Some (idx, mty) ->
            if Type.equal mty ety
            then return idx
            else
              return_err
                (Error.Record_literal_incorrect_type
                   {field = name; field_ty = ety; member_ty = mty})
        | None ->
            return_err (Error.Record_literal_extra_field (ty, name))
      in
      return (idx, expr)
    in
    let tmp =
      Return.Array.of_sequence_unordered ~len:fields_len
        (Sequence.map ~f (Sequence.of_list fields))
    in
    match tmp with
    | Result.Ok o -> o
    | Result.Error (Array.Empty_cell idx) ->
        let ((name, _), (ty, _)), _ = type_fields.(idx) in
        return_err (Error.Record_literal_missing_field (ty, name))
    | Result.Error (Array.Duplicate idx) ->
        let ((name, _), _), _ = type_fields.(idx) in
        return_err (Error.Record_literal_duplicate_fields name)
  in
  let variant =
    Expr.Record_literal {ty = (ty, ty_sp); fields = fields_typed}
  in
  let ty = Type.erase ty in
  return (Expr.Expr {variant; ty})

and typeck_tuple_literal :
       t
    -> Binding.t list
    -> Untyped_expr.t Spanned.t list
    -> Expr.t result =
 fun _ctxt _locals fields ->
  match fields with
  | [] ->
      let ty = Type.erase Type.unit in
      let variant = Expr.Tuple_literal Array.empty in
      return (Expr.Expr {variant; ty})
  | _ -> raise Unimplemented

and typeck_integer_literal :
       t
    -> Type.Category.value Untyped_type.t Spanned.t
    -> int Spanned.t
    -> Expr.t result =
 fun ctxt pty value ->
  let%bind ty = Type.of_untyped pty ~ctxt:(type_context ctxt) in
  match Type.representation ty ~ctxt:(type_context ctxt) with
  | Type.Representation.Integer _ ->
      let variant = Expr.Integer_literal (fst value) in
      let ty = Type.erase ty in
      return (Expr.Expr {variant; ty})
  | _ ->
      let sp = Span.union (snd pty) (snd value) in
      let%bind () = with_span sp in
      return_err (Error.Integer_literal_non_integer_type ty)

and typeck_builtin :
       t
    -> Cafec_Parse.Token.Builtin_name.t Spanned.t
    -> Type.Category.value Untyped_type.t Spanned.t list
    -> Expr.t result =
 fun ctxt name type_arguments ->
  let module BName = Cafec_Parse.Token.Builtin_name in
  let builtin =
    match fst name with
    | BName.Less_eq -> Expr.Builtin.Less_eq
    | BName.Add -> Expr.Builtin.Add
    | BName.Sub -> Expr.Builtin.Sub
    | BName.Mul -> Expr.Builtin.Mul
  in
  let variant = Expr.Builtin builtin in
  let%bind ty =
    match fst name with
    | BName.Less_eq -> (
      match type_arguments with
      | [param; ret_ty] ->
          let%bind param =
            Type.of_untyped param ~ctxt:(type_context ctxt)
          in
          let%bind ret_ty =
            Type.of_untyped ret_ty ~ctxt:(type_context ctxt)
          in
          let%bind () =
            match
              Type.representation param ~ctxt:(type_context ctxt)
            with
            | Type.Representation.Integer _ -> return ()
            | _ ->
                return_err
                  (Error.Builtin_invalid_type {builtin; ty = param})
          in
          let%bind () =
            match
              Type.representation ret_ty ~ctxt:(type_context ctxt)
            with
            | Type.Representation.Variant {variants} ->
                (* check for triviality of the variant type *)
                let f ((_, v), _) = Option.is_none v in
                if Array.length variants = 2
                   && Array.for_all ~f variants
                then return ()
                else
                  return_err
                    (Error.Builtin_invalid_type {builtin; ty = ret_ty})
            | _ ->
                return_err
                  (Error.Builtin_invalid_type {builtin; ty = ret_ty})
          in
          let param = Type.erase param in
          let ret_ty = Type.erase ret_ty in
          let ty =
            Type.Structural.Function
              {params = Array.binary param param; ret_ty}
          in
          return ty
      | els ->
          let%bind () = with_span (snd name) in
          let err =
            Error.Builtin_mismatched_arity
              {builtin; expected = 2; found = List.length els}
          in
          return_err err )
    | BName.Add | BName.Sub | BName.Mul -> (
      match type_arguments with
      | [ty] ->
          let%bind ty = Type.of_untyped ty ~ctxt:(type_context ctxt) in
          let%bind () =
            match Type.representation ty ~ctxt:(type_context ctxt) with
            | Type.Representation.Integer _ -> return ()
            | _ ->
                return_err (Error.Builtin_invalid_type {builtin; ty})
          in
          let ty = Type.erase ty in
          let ty =
            Type.Structural.Function
              {params = Array.binary ty ty; ret_ty = ty}
          in
          return ty
      | els ->
          let%bind () = with_span (snd name) in
          let err =
            Error.Builtin_mismatched_arity
              {builtin; expected = 1; found = List.length els}
          in
          return_err err )
  in
  let ty = Type.erase (Type.Structural ty) in
  return (Expr.Expr {variant; ty})

and typeck_expression :
    Binding.t list -> t -> Untyped_expr.t Spanned.t -> Expr.t result =
 fun locals ctxt unt_expr ->
  let module U = Untyped_expr in
  let module T = Expr in
  let%bind unt_expr = spanned_lift unt_expr in
  match unt_expr with
  | U.Integer_literal {ty; value} ->
      typeck_integer_literal ctxt ty value
  | U.Tuple_literal xs -> typeck_tuple_literal ctxt locals xs
  | U.Match {cond; arms} -> typeck_match ctxt locals cond arms
  | U.Builtin {name; type_arguments} ->
      typeck_builtin ctxt name type_arguments
  | U.Call (callee, args) ->
      let%bind callee =
        spanned_bind (typeck_expression locals ctxt callee)
      in
      let f x = spanned_bind (typeck_expression locals ctxt x) in
      let%bind args =
        Return.Array.of_sequence ~len:(List.length args)
          (Sequence.map ~f (Sequence.of_list args))
      in
      typeck_call callee args
  | U.Prefix_operator ((name, sp), expr) ->
      let name = (name, sp) in
      let name = (U.Name (U.Qualified {path = []; name}), sp) in
      let%bind callee =
        spanned_bind (typeck_expression locals ctxt name)
      in
      let%bind arg =
        spanned_bind (typeck_expression locals ctxt expr)
      in
      typeck_call callee (Array.unary arg)
  | U.Infix_list (first, rest) ->
      let%bind first =
        spanned_bind (typeck_expression locals ctxt first)
      in
      typeck_infix_list locals ctxt first rest
  | U.Name (U.Qualified {path; name}) ->
      typeck_name ctxt locals path name
  | U.Block blk ->
      let%bind blk = spanned_bind (typeck_block locals ctxt blk) in
      let ty = T.Block.full_type_sp blk in
      return (T.Expr {variant = T.Block blk; ty})
  | U.Reference place ->
      let%bind place =
        spanned_bind (typeck_expression locals ctxt place)
      in
      let err ty = return_err (Error.Reference_taken_to_value ty) in
      let%bind ty =
        match T.full_type_sp place with
        | Type.Any (Type.Place _ as ty) ->
            return
              (Type.erase
                 (Type.Structural (Type.Structural.Reference ty)))
        | Type.Any (Type.Any _) -> failwith "un-normalized Any type"
        | Type.Any (Type.Structural _ as ty) -> err ty
        | Type.Any (Type.User_defined _ as ty) -> err ty
      in
      return (T.Expr {variant = T.Reference place; ty})
  | U.Dereference value -> (
      let%bind value =
        spanned_bind (typeck_expression locals ctxt value)
      in
      match T.base_type_sp value with
      | Type.Structural (Type.Structural.Reference pointee) ->
          let ty = Type.erase pointee in
          return (T.Expr {variant = T.Dereference value; ty})
      | ty -> return_err (Error.Dereference_of_non_reference ty) )
  | U.Record_literal {ty; fields} ->
      typeck_record_literal ctxt locals ty fields
  | U.Record_access (expr, name) ->
      let%bind expr =
        spanned_bind (typeck_expression locals ctxt expr)
      in
      let ty, cat = Type.to_type_and_category (T.full_type_sp expr) in
      let%bind idx, ty =
        match Type.representation ty ~ctxt:(type_context ctxt) with
        | Type.Representation.Record {fields} -> (
          match find_field ~fields name with
          | Some (idx, ty) ->
              let ty = Type.of_type_and_category (ty, cat) in
              return (idx, ty)
          | None ->
              return_err (Error.Record_access_non_field (ty, name)) )
        | _ ->
            return_err (Error.Record_access_non_record_type (ty, name))
      in
      return (T.Expr {variant = T.Record_access (expr, idx); ty})
  | _ -> failwith "typeck_expression"

let find_infix_group_name ctxt id =
  let f _ name = Nfc_string.equal id name in
  match Array.findi ~f (infix_group_names ctxt) with
  | Some (i, _) -> Some i
  | None -> None

let type_infix_group_name (_ : t)
    (group : Untyped_ast.Infix_group.t Spanned.t) : Nfc_string.t result
    =
  let module U = Untyped_ast.Infix_group in
  let%bind (U.Infix_group {name = name, _; _}) = spanned_lift group in
  return name

let type_infix_group (ctxt : t)
    (group : Untyped_ast.Infix_group.t Spanned.t) :
    Infix_group.t result =
  let module U = Untyped_ast.Infix_group in
  let%bind (U.Infix_group {associativity; precedence; attributes; _}) =
    spanned_lift group
  in
  assert (List.is_empty attributes) ;
  let f (U.Less (id, _)) =
    match find_infix_group_name ctxt id with
    | Some idx -> return (Infix_group.Less idx)
    | None -> return_err (Error.Infix_group_not_found id)
  in
  let%bind precedence = Return.Array.of_list_map ~f precedence in
  return (Infix_group.Infix_group {associativity; precedence})

let type_infix_decl (ctxt : t)
    (unt_infix_decl : Untyped_ast.Infix_declaration.t Spanned.t) :
    (Name.infix Name.t * int) result =
  let module U = Untyped_ast.Infix_declaration in
  let%bind (U.Infix_declaration
             {name = name, _; group = group, _; attributes}) =
    spanned_lift unt_infix_decl
  in
  assert (List.is_empty attributes) ;
  match find_infix_group_name ctxt group with
  | None -> return_err (Error.Infix_group_not_found group)
  | Some idx -> return (name, idx)

let type_function_declaration (ctxt : t)
    (unt_func : Untyped_ast.Func.t Spanned.t) :
    Function_declaration.t Spanned.t result =
  let module F = Untyped_ast.Func in
  let module D = Function_declaration in
  let unt_func, _ = unt_func in
  let (F.Func {name; params; ret_ty; attributes; _}) = unt_func in
  let name = Name.erase name in
  let%bind params, parm_sp =
    let f ((name, ty), _) =
      let%bind ty = Type.of_untyped ~ctxt:(type_context ctxt) ty in
      return (Binding.Binding {name; is_mut = false; ty})
    in
    spanned_bind
      (Return.Array.of_sequence ~len:(List.length params)
         (Sequence.map ~f (Sequence.of_list params)))
  in
  let%bind ret_ty =
    match ret_ty with
    | Some ret_ty -> Type.of_untyped ~ctxt:(type_context ctxt) ret_ty
    | None -> return (Type.Any Type.unit)
  in
  return (D.Declaration {name; params; ret_ty; attributes}, parm_sp)

let type_function_definition (ctxt : t) (idx : int)
    (unt_func : Untyped_ast.Func.t Spanned.t) :
    Expr.Block.t Spanned.t result =
  let module F = Untyped_ast.Func in
  let module D = Function_declaration in
  let unt_func, _ = unt_func in
  let decl =
    let decl, _ = (function_context ctxt).(idx) in
    assert (Name.equal (D.name decl) (F.name unt_func)) ;
    decl
  in
  let%bind body, body_sp =
    spanned_bind
      (typeck_block
         (Array.to_list (D.params decl))
         ctxt (F.body unt_func))
  in
  let body_ty =
    match Expr.Block.expr body with
    | Some e -> Expr.full_type_sp e
    | None -> Type.erase Type.unit
  in
  if Type.compatible (D.ret_ty decl) body_ty
  then return (body, body_sp)
  else
    return_err
      (Error.Return_type_mismatch
         {expected = D.ret_ty decl; found = body_ty})

let precedence_less infix_groups lhs rhs =
  (*
    tries to find an idx = rhs in lhs's tree of less-than precedences
    depth-first
  *)
  let rec f (Infix_group.Less idx) =
    if idx = rhs
    then true
    else
      let prec = Infix_group.precedence infix_groups.(idx) in
      Array.exists ~f prec
  in
  let prec = Infix_group.precedence infix_groups.(lhs) in
  Array.exists ~f prec

let make unt_ast : (t, Error.t * Type.Context.t) Spanned.Result.t =
  let module U = Untyped_ast in
  let%bind type_context =
    match Type.Context.make (U.types unt_ast) with
    | Result.Ok o, sp -> (Result.Ok o, sp)
    | Result.Error e, sp -> (Result.Error (e, Type.Context.empty), sp)
  in
  let check_for_errors arr ~equal ~err =
    match Array.findi_nonconsecutive_duplicates arr ~equal with
    | Some (idx_el1, idx_el2) -> return_err (err idx_el1 idx_el2)
    | None -> return ()
  in
  let ret =
    let ctxt =
      Context
        { type_context
        ; infix_group_names = Array.empty
        ; infix_groups = Array.empty
        ; infix_decls = Array.empty
        ; entrypoint = None
        ; function_context = Array.empty
        ; function_definitions = Array.empty }
    in
    let%bind ctxt =
      let%bind infix_group_names =
        Return.Array.of_list_map
          ~f:(type_infix_group_name ctxt)
          (U.infix_groups unt_ast)
      in
      let equal (_, name1) (_, name2) = Nfc_string.equal name1 name2 in
      let err (_, name) _ =
        Error.Infix_group_defined_multiple_times name
      in
      let%bind () = check_for_errors infix_group_names ~equal ~err in
      return (with_infix_group_names ctxt infix_group_names)
    in
    let%bind ctxt =
      let%bind infix_groups =
        Return.Array.of_list_map ~f:(type_infix_group ctxt)
          (U.infix_groups unt_ast)
      in
      let equal (idx1, _) (idx2, _) =
        precedence_less infix_groups idx1 idx2
        && precedence_less infix_groups idx2 idx1
      in
      let err (idx1, _) (idx2, _) =
        let names = infix_group_names ctxt in
        Error.Infix_group_recursive_precedence
          (names.(idx1), names.(idx2))
      in
      let%bind () = check_for_errors infix_groups ~equal ~err in
      return (with_infix_groups ctxt infix_groups)
    in
    let%bind ctxt =
      let%bind infix_decls =
        Return.Array.of_list_map ~f:(type_infix_decl ctxt)
          (U.infix_decls unt_ast)
      in
      let equal (_, (name1, _)) (_, (name2, _)) =
        Name.equal name1 name2
      in
      let err (_, (name, _)) (_, _) =
        Error.Defined_infix_declaration_multiple_times name
      in
      let%bind () = check_for_errors infix_decls ~equal ~err in
      return (with_infix_decls ctxt infix_decls)
    in
    let%bind ctxt =
      let%bind function_context =
        Return.Array.of_list_map
          ~f:(type_function_declaration ctxt)
          (U.funcs unt_ast)
      in
      let module D = Function_declaration in
      let equal (_, (decl1, _)) (_, (decl2, _)) =
        Name.equal (D.name decl1) (D.name decl2)
      in
      let err (_, (decl, _)) (_, _) =
        Error.Defined_function_multiple_times (D.name decl)
      in
      let%bind () = check_for_errors function_context ~equal ~err in
      return (with_function_context ctxt function_context)
    in
    let%bind ctxt =
      let module D = Function_declaration in
      let rec helper ?found_at arr i =
        if i < Array.length arr
        then
          let func, _ = arr.(i) in
          let atts = D.attributes func in
          match atts with
          | [] -> helper ?found_at arr (i + 1)
          | [(Attribute.Entrypoint, _)] -> (
            match found_at with
            | Some first ->
                let first =
                  let f, _ = arr.(first) in
                  D.name f
                in
                let second = D.name func in
                return_err (Error.Multiple_entrypoints {first; second})
            | None -> helper ~found_at:i arr (i + 1) )
          | _ -> failwith "unexpected attributes"
        else return found_at
      in
      let%bind entrypoint = helper (function_context ctxt) 0 in
      return (with_entrypoint ctxt entrypoint)
    in
    let%bind function_definitions =
      Sequence.of_list (U.funcs unt_ast)
      |> Sequence.mapi ~f:(type_function_definition ctxt)
      |> Return.Array.of_sequence ~len:(List.length (U.funcs unt_ast))
    in
    return (with_function_definitions ctxt function_definitions)
  in
  match ret with
  | Result.Ok o, sp -> (Result.Ok o, sp)
  | Result.Error e, sp -> (Result.Error (e, type_context), sp)
