open! Types.Pervasives
module Parse = Cafec_parse
include Types.Type
module Structural = Types.Type_Structural

module Context = struct
  type user_defined_type = {data: Structural.t}

  (* note: const after construction *)
  type nonrec t =
    { user_defined: user_defined_type array
    ; names: (Nfc_string.t Spanned.t * t) array }

  let make lst =
    let module PType = Parse.Ast.Type in
    let defs, defs_len, aliases, aliases_len =
      let module Def = PType.Definition in
      let rec helper defs defs_len aliases aliases_len = function
        | ({Def.name; Def.kind= Def.Alias ty}, _) :: rest ->
            helper defs defs_len ((name, ty) :: aliases) (aliases_len + 1) rest
        | ({Def.name; Def.kind= Def.User_defined {data}}, _) :: rest ->
            helper ((name, data) :: defs) (defs_len + 1) aliases aliases_len
              rest
        | [] -> (defs, defs_len, aliases, aliases_len)
      in
      helper [] 0 [] 0 lst
    in
    let rec get_ast_type pty =
      (* returns -1 if not found *)
      let rec find_index name index = function
        | [] -> -1
        | ((name', _), _) :: _ when Nfc_string.equal name name' -> index
        | _ :: rest -> find_index name (index + 1) rest
      in
      let rec find_alias name = function
        | [] -> return None
        | ((name', _), alias) :: _ when Nfc_string.equal name name' ->
            let%bind ty = get_ast_type alias in
            return (Some ty)
        | _ :: rest -> find_alias name rest
      in
      match pty with
      | PType.Named name -> (
        match find_index name 0 defs with
        | -1 -> (
            let%bind ty = find_alias name aliases in
            match ty with
            | Some ty -> return ty
            | None -> (
              match (name :> string) with
              | "Unit" -> return (Builtin Unit)
              | "Bool" -> return (Builtin Bool)
              | "Int32" -> return (Builtin Int32)
              | _ -> return_err (Error.Type_not_found name) ) )
        | n -> return (User_defined n) )
      | PType.Reference {is_mut; pointee= p, _} ->
          let mutability = if is_mut then Mutable else Immutable in
          let%bind pointee = get_ast_type p in
          return (Builtin (Reference {mutability; pointee}))
      | PType.Function {params; ret_ty} ->
          let f (x, _) = get_ast_type x in
          let%bind params = return_map ~f params in
          let%bind ret_ty =
            match ret_ty with
            | Some (ty, _) -> get_ast_type ty
            | None -> return (Builtin Unit)
          in
          return (Builtin (Function {params; ret_ty}))
    in
    let names_len = defs_len + aliases_len in
    let user_defined =
      let default = {data= Structural.Record []} in
      Array.create ~len:defs_len default
    in
    let names =
      let default = ((Nfc_string.empty, Spanned.Span.made_up), User_defined (-1)) in
      Array.create ~len:names_len default
    in
    let rec exists_duplicate idx end_idx name =
      if idx = end_idx then false
      else
        let (name_idx, _), _ = names.(idx) in
        if Nfc_string.equal name_idx name then true
        else exists_duplicate (idx + 1) end_idx name
    in
    let fill_defs index ((name, name_sp), def) =
      let module Data = PType.Data in
      if exists_duplicate 0 index name then
        return_err (Error.Type_defined_multiple_times name)
      else
        let%bind data =
          match def with Data.Record lst ->
            let f ((name, ty), _) =
              let%bind ty = get_ast_type ty in
              return (name, ty)
            in
            let%bind members = return_map ~f lst in
            return (Structural.Record members)
        in
        user_defined.(index) <- {data} ;
        names.(index) <- ((name, name_sp), User_defined index) ;
        return ()
    in
    let%bind () = return_iteri ~f:fill_defs defs in
    let fill_aliases index (name, ty) =
      let%bind ty = get_ast_type ty in
      names.(index + defs_len) <- (name, ty) ;
      return ()
    in
    let%bind () = return_iteri ~f:fill_aliases aliases in
    return {user_defined; names}

  let empty = {user_defined= [||]; names= [||]}
end

let rec of_untyped (unt_ty : Parse.Ast.Type.t Spanned.t) ~(ctxt : Context.t) :
    t result =
  let module U = Parse.Ast.Type in
  let module D = U.Definition in
  let%bind unt_ty = spanned_lift unt_ty in
  match unt_ty with
  | U.Named name -> (
      let f ((name', _), _) = Nfc_string.equal name' name in
      match Array.find ~f ctxt.Context.names with
      | Some (_, ty) -> return ty
      | None -> (
        match (name :> string) with
        | "Unit" -> return (Builtin Unit)
        | "Bool" -> return (Builtin Bool)
        | "Int32" -> return (Builtin Int32)
        | _ -> return_err (Error.Type_not_found name) ) )
  | U.Reference {is_mut; pointee} ->
      let mutability = if is_mut then Mutable else Immutable in
      let%bind pointee = of_untyped pointee ~ctxt in
      return (Builtin (Reference {mutability; pointee}))
  | U.Function {params; ret_ty} ->
      let f ty = of_untyped ty ~ctxt in
      let default = return (Builtin Unit) in
      let%bind params = return_map ~f params in
      let%bind ret_ty = Option.value_map ~f ~default ret_ty in
      return (Builtin (Function {params; ret_ty}))

let rec equal l r =
  match (l, r) with
  | Builtin Unit, Builtin Unit -> true
  | Builtin Bool, Builtin Bool -> true
  | Builtin Int32, Builtin Int32 -> true
  | Builtin (Reference l), Builtin (Reference r) -> (
    match (l.mutability, r.mutability) with
    | Mutable, Mutable | Immutable, Immutable -> equal l.pointee r.pointee
    | _ -> false )
  | Builtin (Function f1), Builtin (Function f2) ->
      equal f1.ret_ty f2.ret_ty && List.equal f1.params f2.params ~equal
  | User_defined u1, User_defined u2 -> u1 = u2
  | _ -> false

let structural ty ~(ctxt : Context.t) =
  match ty with
  | Builtin b -> Structural.Builtin b
  | User_defined idx -> Context.(ctxt.user_defined.(idx).data)

let rec to_string ty ~(ctxt : Context.t) =
  match ty with
  | Builtin Unit -> "Unit"
  | Builtin Bool -> "Bool"
  | Builtin Int32 -> "Int32"
  | Builtin (Reference {mutability; pointee}) ->
      let pointer =
        match mutability with Mutable -> "&mut " | Immutable -> "&"
      in
      let pointee = to_string pointee ~ctxt in
      pointer ^ pointee
  | Builtin (Function {params; ret_ty}) ->
      let params =
        let f ty = to_string ty ~ctxt in
        String.concat ~sep:", " (List.map ~f params)
      in
      String.concat ["func("; params; ") -> "; to_string ret_ty ~ctxt]
  | User_defined idx ->
      let (name, _), _ = ctxt.Context.names.(idx) in
      (name :> string)
