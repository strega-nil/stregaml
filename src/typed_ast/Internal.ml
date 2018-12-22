open! Types.Pervasives
module Span = Spanned.Span
module Untyped_ast = Cafec_Parse.Ast
module Expr = Ast.Expr
module Local = Ast.Expr.Local
module Binding = Ast.Binding

module Function_declaration = struct
  type t =
    | Declaration : {name: Name.t; params: Binding.t list; ret_ty: Type.t} -> t

  let name (Declaration {name; _}) = name

  let params (Declaration {params; _}) = params

  let ret_ty (Declaration {ret_ty; _}) = ret_ty
end

module Infix_group = struct
  type associativity = Untyped_ast.Infix_group.associativity =
    | Assoc_start : associativity
    | Assoc_end : associativity
    | Assoc_none : associativity

  type precedence = Less : int -> precedence

  type t =
    | Infix_group :
        { associativity: associativity
        ; precedence: precedence list }
        -> t

  let associativity (Infix_group {associativity; _}) = associativity

  let precedence (Infix_group {precedence; _}) = precedence
end

(* these should really all be arrays, probably *)
type t =
  | Context :
      { type_context: Type.Context.t
      ; infix_group_names: Nfc_string.t list
      ; infix_groups: Infix_group.t list
      ; infix_decls: (Name.t * int) list
      ; function_context: Function_declaration.t Spanned.t list
      ; function_definitions: Expr.Block.t Spanned.t list }
      -> t

let type_context (Context r) = r.type_context

let infix_group_names (Context r) = r.infix_group_names

let infix_groups (Context r) = r.infix_groups

let infix_decls (Context r) = r.infix_decls

let function_context (Context r) = r.function_context

let function_definitions (Context r) = r.function_definitions

type 'a result = ('a, Error.t) Spanned.Result.t

module Functions : sig
  val index_by_name :
    Function_declaration.t Spanned.t list -> Name.t -> int option

  val decl_by_index :
       Function_declaration.t Spanned.t list
    -> int
    -> Function_declaration.t Spanned.t

  val expr_by_index :
    Expr.Block.t Spanned.t list -> int -> Expr.Block.t Spanned.t
end = struct
  let index_by_name ctxt search =
    let module D = Function_declaration in
    let rec helper n = function
      | (D.Declaration {name; _}, _) :: _ when Name.equal name search -> Some n
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
      let f (name, _) = Name.equal op name in
      match List.find ~f (infix_decls ctxt) with
      | Some (_, idx) -> Some (idx, List.nth_exn (infix_groups ctxt) idx)
      | None -> None
    in
    let order_named ctxt op1 op2 =
      let rec order_infix_groups idx info idx2 =
        if idx = idx2 then
          match T.associativity info with
          | T.Assoc_start -> Some Start
          | T.Assoc_end -> Some End
          | T.Assoc_none -> Some Unordered
        else
          let rec check_all_sub_precedences = function
            | [] -> None
            | T.Less idx :: xs -> (
                let info = List.nth_exn (infix_groups ctxt) idx in
                (*
                  note: since ig < ig2, this means that no matter what,
                  ig binds looser than ig2
                *)
                match order_infix_groups idx info idx2 with
                | Some _ -> Some End
                | None -> check_all_sub_precedences xs )
          in
          check_all_sub_precedences (T.precedence info)
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
    if Name.equal name' name then Some (Local.Local {binding; index}) else None
  in
  List.find_mapi ~f lst

let value_type ty =
  let module T = Ast.Expr.Type in
  T.Type {category= T.Value; ty}

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
          let expr_ty = T.Expr.base_type_sp expr in
          let%bind ty =
            match ty with
            | None -> return expr_ty
            | Some ty ->
                let%bind ty = Type.of_untyped ty ~ctxt:(type_context ctxt) in
                if Type.equal expr_ty ty then return ty
                else
                  let name, _ = name in
                  return_err
                    (Error.Incorrect_let_type {name; let_ty= ty; expr_ty})
          in
          let mutability = if is_mut then Type.Mutable else Type.Immutable in
          let binding = Binding.Binding {name; mutability; ty} in
          let locals = binding :: locals in
          let%bind xs, expr_locals = typeck_stmts locals xs in
          return ((T.Stmt.Let {binding; expr}, sp) :: xs, expr_locals) )
  in
  let U.Expr.Block.Block {stmts; expr}, sp = unt_blk in
  let%bind stmts, locals = typeck_stmts locals stmts in
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
  return (T.Expr {variant= T.Call (callee, args); ty= value_type ret_ty})

and typeck_infix_list (locals : Binding.t list) (ctxt : t) e0 rest =
  let module U = Untyped_ast.Expr in
  let module T = Ast.Expr in
  let make_op_expr op e0 e1 =
    match op with
    | U.Infix_assign, _ -> (
        let source, dest = (e1, e0) in
        let (T.Type.Type {category= dest_cat; ty= dest_ty}) =
          T.full_type_sp dest
        in
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
              return (T.Expr {variant= T.Assign {dest; source}; ty}) )
    | U.Infix_name name, sp ->
        let name = (name, sp) in
        let name = (U.Name (U.Qualified {path= []; name}), sp) in
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
      return (T.Expr {variant= T.Unit_literal; ty})
  | U.Bool_literal b ->
      let ty = value_type (Type.Builtin Type.Bool) in
      return (T.Expr {variant= T.Bool_literal b; ty})
  | U.Integer_literal i ->
      let ty = value_type (Type.Builtin Type.Int32) in
      return (T.Expr {variant= T.Integer_literal i; ty})
  | U.Match {cond; arms= parse_arms} ->
      let%bind cond = spanned_bind (typeck_expression locals ctxt cond) in
      let cond_ty = T.base_type_sp cond in
      let%bind variants =
        match Type.structural cond_ty ~ctxt:(type_context ctxt) with
        | Type.Structural.Variant variants -> return variants
        | _ -> return_err (Error.Match_non_variant_type cond_ty)
      in
      let variants_len = List.length variants in
      let arms_some = Array.create ~len:variants_len false in
      let arms =
        let ty = Type.Builtin Type.Unit in
        let blk = T.Block.Block {stmts= []; expr= None} in
        Array.create ~len:variants_len (ty, (blk, Spanned.Span.made_up))
      in
      let arms_ty = ref None in
      let%bind () =
        let insert_arm pat blk =
          let (U.Pattern {constructor= constructor, _; binding}) = pat in
          let%bind cty, (cname_name, _) =
            match U.qualified_path constructor with
            | [(ty_name, sp)] ->
                let ty = (Untyped_ast.Type.Named ty_name, sp) in
                let%bind ty = Type.of_untyped ty ~ctxt:(type_context ctxt) in
                return (ty, U.qualified_name constructor)
            | _ -> failwith "paths with size <> 1 not supported"
          in
          let%bind cname =
            match cname_name with
            | Name.Name {string; kind= Name.Identifier; _} -> return string
            | _ -> return_err (Error.Name_not_found_in_type (cty, cname_name))
          in
          let%bind () =
            if not (Type.equal cty cond_ty) then
              return_err
                (Error.Pattern_of_wrong_type {expected= cond_ty; found= cty})
            else return ()
          in
          let%bind index, bind_ty =
            let f _ (name, _) = Nfc_string.equal name cname in
            match List.findi variants ~f with
            | None ->
                return_err (Error.Name_not_found_in_type (cty, cname_name))
            | Some (idx, (_, bind_ty)) -> return (idx, bind_ty)
          in
          let%bind () =
            if arms_some.(index) then
              return_err (Error.Match_repeated_branches cname)
            else return ()
          in
          let binding =
            Ast.Binding.Binding
              {name= binding; mutability= Type.Immutable; ty= bind_ty}
          in
          let locals = binding :: locals in
          let%bind blk = spanned_bind (typeck_block locals ctxt blk) in
          let%bind () =
            match !arms_ty with
            | None -> return (arms_ty := Some (T.Block.base_type_sp blk))
            | Some ty ->
                let arm_ty = T.Block.base_type_sp blk in
                if Type.equal arm_ty ty then return ()
                else
                  return_err
                    (Error.Match_branches_of_different_type
                       {expected= ty; found= arm_ty})
          in
          arms.(index) <- (bind_ty, blk) ;
          arms_some.(index) <- true ;
          return ()
        in
        let rec helper = function
          | [] -> return ()
          | ((pat, _), blk) :: xs ->
              let%bind () = insert_arm pat blk in
              helper xs
        in
        helper parse_arms
      in
      let variant = T.Match {cond; arms} in
      let ty =
        match !arms_ty with
        | Some ty -> value_type ty
        | None ->
            (* technically should be bottom, but _shrug_ *)
            value_type (Type.Builtin Type.Unit)
      in
      return (T.Expr {variant; ty})
  | U.If_else {cond; thn; els} -> (
      let%bind cond = spanned_bind (typeck_expression locals ctxt cond) in
      match T.base_type_sp cond with
      | Type.Builtin Type.Bool ->
          let%bind thn = spanned_bind (typeck_block locals ctxt thn) in
          let%bind els = spanned_bind (typeck_block locals ctxt els) in
          let thn_ty = T.Block.base_type_sp thn in
          let els_ty = T.Block.base_type_sp els in
          if Type.equal thn_ty els_ty then
            return
              (T.Expr
                 {variant= T.If_else {cond; thn; els}; ty= value_type thn_ty})
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
            (T.Expr
               { variant= T.Builtin (T.Builtin.Less_eq (arg1, arg2))
               ; ty= value_type (Type.Builtin Type.Bool) })
      | "add" ->
          return
            (T.Expr
               { variant= T.Builtin (T.Builtin.Add (arg1, arg2))
               ; ty= value_type (Type.Builtin Type.Int32) })
      | "sub" ->
          return
            (T.Expr
               { variant= T.Builtin (T.Builtin.Sub (arg1, arg2))
               ; ty= value_type (Type.Builtin Type.Int32) })
      | "mul" ->
          return
            (T.Expr
               { variant= T.Builtin (T.Builtin.Mul (arg1, arg2))
               ; ty= value_type (Type.Builtin Type.Int32) })
      | _ -> return_err (Error.Unknown_builtin name) )
  | U.Call (callee, args) ->
      let%bind callee = spanned_bind (typeck_expression locals ctxt callee) in
      let f x = spanned_bind (typeck_expression locals ctxt x) in
      let%bind args = return_map ~f args in
      typeck_call callee args
  | U.Prefix_operator ((name, sp), expr) ->
      let name = (name, sp) in
      let name = (U.Name (U.Qualified {path= []; name}), sp) in
      let%bind callee = spanned_bind (typeck_expression locals ctxt name) in
      let%bind arg = spanned_bind (typeck_expression locals ctxt expr) in
      typeck_call callee [arg]
  | U.Infix_list (first, rest) ->
      let%bind first = spanned_bind (typeck_expression locals ctxt first) in
      typeck_infix_list locals ctxt first rest
  | U.Name (U.Qualified {path= []; name}) -> (
    match find_local name locals with
    | Some loc ->
        let (Binding.Binding {ty; mutability; _}) = Local.binding loc in
        let ty = T.Type.Type {ty; category= T.Type.Place {mutability}} in
        return (T.Expr {variant= T.Local loc; ty})
    | None -> (
        let name, _ = name in
        match Functions.index_by_name (function_context ctxt) name with
        | None -> return_err (Error.Name_not_found name)
        | Some idx ->
            let ty =
              let decl, _ =
                Functions.decl_by_index (function_context ctxt) idx
              in
              let params =
                let f (Binding.Binding {ty; _}) = ty in
                List.map ~f (Function_declaration.params decl)
              in
              let ret_ty = Function_declaration.ret_ty decl in
              value_type (Type.Builtin (Type.Function {params; ret_ty}))
            in
            return (T.Expr {variant= T.Global_function idx; ty}) ) )
  | U.Name (U.Qualified {path= [ty_name]; name= name, _}) ->
      let%bind ty =
        let ty_name, sp = ty_name in
        let ty = (Untyped_ast.Type.Named ty_name, sp) in
        Type.of_untyped ty ~ctxt:(type_context ctxt)
      in
      let%bind type_members =
        match Type.structural ty ~ctxt:(type_context ctxt) with
        | Type.Structural.Variant members -> return members
        | _ -> return_err (Error.Name_not_found_in_type (ty, name))
      in
      let%bind nfc_name =
        match name with
        | Name.Name {string; kind= Name.Identifier; _} -> return string
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
      return (T.Expr {variant= T.Constructor (ty, idx); ty= value_type ty})
  | U.Name _ -> failwith "paths with size > 1 not supported"
  | U.Block blk ->
      let%bind blk = spanned_bind (typeck_block locals ctxt blk) in
      let ty = T.Block.full_type_sp blk in
      return (T.Expr {variant= T.Block blk; ty})
  | U.Reference {is_mut; place} ->
      let%bind place = spanned_bind (typeck_expression locals ctxt place) in
      let mutability = if is_mut then Type.Mutable else Type.Immutable in
      let pointee_ty = T.full_type_sp place in
      let%bind ty =
        match T.Type.category pointee_ty with
        | T.Type.Value ->
            return_err (Error.Reference_taken_to_value mutability)
        | T.Type.Place {mutability= pmut} -> (
            let pointee = T.Type.ty pointee_ty in
            match (mutability, pmut) with
            | Type.Immutable, _ | Type.Mutable, Type.Mutable ->
                return
                  (value_type
                     (Type.Builtin (Type.Reference {mutability; pointee})))
            | Type.Mutable, Type.Immutable ->
                return_err Error.Mutable_reference_taken_to_immutable_place )
      in
      return (T.Expr {variant= T.Reference {mutability; place}; ty})
  | U.Dereference value -> (
      let%bind value = spanned_bind (typeck_expression locals ctxt value) in
      match T.base_type_sp value with
      | Type.Builtin (Type.Reference {mutability; pointee}) ->
          let ty =
            T.Type.Type {category= T.Type.Place {mutability}; ty= pointee}
          in
          return (T.Expr {variant= T.Dereference value; ty})
      | ty -> return_err (Error.Dereference_of_non_reference ty) )
  | U.Record_literal {ty; members} ->
      let%bind ty, ty_sp =
        spanned_bind (Type.of_untyped ty ~ctxt:(type_context ctxt))
      in
      let%bind type_members =
        match Type.structural ty ~ctxt:(type_context ctxt) with
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
              let ety = T.base_type expr in
              let%bind idx =
                match find_in_type_members name type_members with
                | Some (idx, mty) ->
                    if Type.equal mty ety then return idx
                    else
                      return_err
                        (Error.Record_literal_incorrect_type
                           {field= name; field_ty= ety; member_ty= mty})
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
        (T.Expr
           { variant= T.Record_literal {ty= (ty, ty_sp); members= members_typed}
           ; ty= value_type ty })
  | U.Record_access (expr, member) ->
      let%bind expr = spanned_bind (typeck_expression locals ctxt expr) in
      let ety = T.full_type_sp expr in
      let%bind idx, ty =
        let (T.Type.Type {category= ecat; ty= ebty}) = ety in
        match Type.structural ebty ~ctxt:(type_context ctxt) with
        | Type.Structural.Record members -> (
            let f _ (n, _) = Nfc_string.equal n member in
            match List.findi ~f members with
            | Some (idx, (_, ty)) ->
                return (idx, T.Type.Type {category= ecat; ty})
            | None ->
                return_err (Error.Record_access_non_member (ebty, member)) )
        | _ -> return_err (Error.Record_access_non_record_type (ebty, member))
      in
      return (T.Expr {variant= T.Record_access (expr, idx); ty})

let find_infix_group_name ctxt id =
  let f _ name = Nfc_string.equal id name in
  match List.findi ~f (infix_group_names ctxt) with
  | Some (i, _) -> Some i
  | None -> None

let add_infix_group_name (ctxt : t)
    (group : Untyped_ast.Infix_group.t Spanned.t) : t result =
  let module U = Untyped_ast.Infix_group in
  let U.Infix_group {name= name, _; _}, _ = group in
  let (Context r) = ctxt in
  match find_infix_group_name ctxt name with
  | Some _ -> return_err (Error.Infix_group_defined_multiple_times name)
  | None ->
      return (Context {r with infix_group_names= name :: r.infix_group_names})

let add_infix_group (ctxt : t) (group : Untyped_ast.Infix_group.t Spanned.t) :
    t result =
  let module U = Untyped_ast.Infix_group in
  let names_len = List.length (infix_group_names ctxt) in
  let ig_len = List.length (infix_groups ctxt) in
  let rec precedence_less ig idx =
    let rec helper = function
      | [] -> false
      | Infix_group.Less idx' :: _ when idx = idx' -> true
      | Infix_group.Less idx' :: xs ->
          let ig_idx = idx' - names_len + ig_len in
          let prec_less =
            if ig_idx < 0 then false
            else
              let ig = List.nth_exn (infix_groups ctxt) ig_idx in
              precedence_less ig idx
          in
          prec_less || helper xs
    in
    helper (Infix_group.precedence ig)
  in
  let U.Infix_group {associativity; precedence; name= name, _}, _ = group in
  let f (U.Less (id, _)) =
    match find_infix_group_name ctxt id with
    | Some idx ->
        (* get the index of idx in the _current_ ctxt.infix_groups *)
        let ig_idx = idx - names_len + ig_len in
        let my_idx = names_len - ig_len - 1 in
        let%bind () =
          if ig_idx < 0 then return ()
          else
            let ig = List.nth_exn (infix_groups ctxt) ig_idx in
            if precedence_less ig my_idx then
              let my_name = name in
              let name = List.nth_exn (infix_group_names ctxt) idx in
              return_err
                (Error.Infix_group_recursive_precedence (name, my_name))
            else return ()
        in
        return (Infix_group.Less idx)
    | None -> return_err (Error.Infix_group_not_found id)
  in
  let%bind precedence = return_map ~f precedence in
  let group = Infix_group.Infix_group {associativity; precedence} in
  let (Context r) = ctxt in
  return (Context {r with infix_groups= group :: r.infix_groups})

let add_infix_decl (ctxt : t)
    (unt_infix_decl : Untyped_ast.Infix_declaration.t Spanned.t) : t result =
  let module U = Untyped_ast.Infix_declaration in
  let%bind (U.Infix_declaration {name= name, _; group= group, _}) =
    spanned_lift unt_infix_decl
  in
  match find_infix_group_name ctxt group with
  | None -> return_err (Error.Infix_group_not_found group)
  | Some idx ->
      let decl = (name, idx) in
      let (Context r) = ctxt in
      return (Context {r with infix_decls= decl :: r.infix_decls})

let add_function_declaration (ctxt : t)
    (unt_func : Untyped_ast.Func.t Spanned.t) : t result =
  let module F = Untyped_ast.Func in
  let module D = Function_declaration in
  let unt_func, _ = unt_func in
  let (F.Func {name; params; ret_ty; _}) = unt_func in
  let%bind params, parm_sp =
    let f ((name, ty), _) =
      let%bind ty = Type.of_untyped ~ctxt:(type_context ctxt) ty in
      return (Binding.Binding {name; mutability= Type.Immutable; ty})
    in
    spanned_bind (return_map ~f params)
  in
  let%bind ret_ty =
    match ret_ty with
    | Some ret_ty -> Type.of_untyped ~ctxt:(type_context ctxt) ret_ty
    | None -> return (Type.Builtin Type.Unit)
  in
  (* check for duplicates *)
  let rec check_for_duplicates search = function
    | [] -> None
    | (f, sp) :: _ when Name.equal (D.name f) search -> Some (f, sp)
    | _ :: xs -> check_for_duplicates search xs
  in
  match check_for_duplicates name (function_context ctxt) with
  | Some (_, sp) ->
      return_err
        (Error.Defined_function_multiple_times {name; original_declaration= sp})
  | None ->
      let module D = Function_declaration in
      let decl = (D.Declaration {name; params; ret_ty}, parm_sp) in
      let (Context r) = ctxt in
      return (Context {r with function_context= decl :: r.function_context})

let add_function_definition (ctxt : t)
    (unt_func : Untyped_ast.Func.t Spanned.t) : t result =
  let module F = Untyped_ast.Func in
  let module D = Function_declaration in
  let unt_func, _ = unt_func in
  let decl =
    let num_funcs = List.length (function_context ctxt) in
    let idx = num_funcs - 1 - List.length (function_definitions ctxt) in
    let decl, _ = Functions.decl_by_index (function_context ctxt) idx in
    assert (Name.equal (D.name decl) (F.name unt_func)) ;
    decl
  in
  let%bind body, body_sp =
    spanned_bind (typeck_block (D.params decl) ctxt (F.body unt_func))
  in
  let body_ty =
    match Expr.Block.expr body with
    | Some e -> Expr.base_type_sp e
    | None -> Type.Builtin Type.Unit
  in
  if Type.equal body_ty (D.ret_ty decl) then
    let (Context r) = ctxt in
    let function_definitions = (body, body_sp) :: r.function_definitions in
    return (Context {r with function_definitions})
  else
    return_err
      (Error.Return_type_mismatch {expected= D.ret_ty decl; found= body_ty})

let make unt_ast : (t, Error.t * Type.Context.t) Spanned.Result.t =
  let module U = Untyped_ast in
  let%bind type_context =
    match Type.Context.make (U.types unt_ast) with
    | Result.Ok o, sp -> (Result.Ok o, sp)
    | Result.Error e, sp -> (Result.Error (e, Type.Context.empty), sp)
  in
  let ret =
    let init =
      Context
        { type_context
        ; infix_group_names= []
        ; infix_groups= []
        ; infix_decls= []
        ; function_context= []
        ; function_definitions= [] }
    in
    let%bind init =
      return_fold (U.infix_groups unt_ast) ~init ~f:add_infix_group_name
    in
    let%bind init =
      return_fold (U.infix_groups unt_ast) ~init ~f:add_infix_group
    in
    let%bind init =
      return_fold (U.infix_decls unt_ast) ~init ~f:add_infix_decl
    in
    let%bind init =
      return_fold (U.funcs unt_ast) ~init ~f:add_function_declaration
    in
    return_fold (U.funcs unt_ast) ~init ~f:add_function_definition
  in
  match ret with
  | Result.Ok o, sp -> (Result.Ok o, sp)
  | Result.Error e, sp -> (Result.Error (e, type_context), sp)
