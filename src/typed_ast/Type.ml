open! Types.Pervasives
module Parse = Cafec_Parse
include Types.Type

module Structural = struct
  include Types.Type_Structural
  module Kind = Types.Type_Structural_Kind
end

module Context = struct
  type user_type = User_type : {data: Structural.t} -> user_type

  let user_type_data (User_type r) = r.data

  type t =
    | Context :
        { user_types: user_type Array.t
        ; names: (Nfc_string.t Spanned.t * Types.Type.t) Array.t }
        -> t

  let make lst =
    let module PType = Parse.Ast.Type in
    let defs, defs_len, aliases, aliases_len =
      let module Def = PType.Definition in
      let rec helper defs defs_len aliases aliases_len = function
        | (Def.Definition {name; kind= Def.Alias ty}, _) :: rest ->
            helper defs defs_len ((name, ty) :: aliases) (aliases_len + 1) rest
        | (Def.Definition {name; kind= Def.User_defined {data}}, _) :: rest ->
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
          let%bind params =
            Return.Array.of_sequence ~len:(List.length params)
              (Sequence.map ~f (Sequence.of_list params))
          in
          let%bind ret_ty =
            match ret_ty with
            | Some (ty, _) -> get_ast_type ty
            | None -> return (Builtin Unit)
          in
          return (Builtin (Function {params; ret_ty}))
    in
    let%bind names =
      let names_len = defs_len + aliases_len in
      let user_defined index ((name, sp), _) =
        return ((name, sp), User_defined index)
      in
      let alias (name, ty) =
        let%bind ty = get_ast_type ty in
        return (name, ty)
      in
      Return.Array.of_sequence ~len:names_len
        (Sequence.append
           (Sequence.mapi ~f:user_defined (Sequence.of_list defs))
           (Sequence.map ~f:alias (Sequence.of_list aliases)))
    in
    let%bind () =
      (* check for duplicates *)
      let rec mem_from name idx arr =
        if idx = Array.length arr then None
        else
          let (el, _), _ = arr.(idx) in
          if Nfc_string.equal name el then Some idx
          else mem_from name (idx + 1) arr
      in
      let rec check_duplicates idx =
        if idx = Array.length names then return ()
        else
          let (name, _), _ = names.(idx) in
          match mem_from name (idx + 1) names with
          | Some _ -> return_err (Error.Type_defined_multiple_times name)
          | None -> check_duplicates (idx + 1)
      in
      check_duplicates 0
    in
    let%bind user_types =
      let f (_, def) =
        let module Data = PType.Data in
        let%bind data =
          let typed_members lst =
            let f ((name, ty), _) =
              let%bind ty = get_ast_type ty in
              return (name, ty)
            in
            let%bind lst = Return.List.map ~f lst in
            return (Array.of_list lst)
          in
          let (Data.Data {kind; members}) = def in
          let%bind members = typed_members members in
          match kind with
          | Data.Record -> return (Structural.Record members)
          | Data.Variant -> return (Structural.Variant members)
        in
        return (User_type {data})
      in
      Return.Array.of_sequence ~len:defs_len
        (Sequence.map ~f (Sequence.of_list defs))
    in
    return (Context {user_types; names})

  let empty = Context {user_types= Array.empty (); names= Array.empty ()}

  let user_types (Context r) = r.user_types

  let names (Context r) = r.names
end

let rec of_untyped (unt_ty : Parse.Ast.Type.t Spanned.t) ~(ctxt : Context.t) :
    t result =
  let module U = Parse.Ast.Type in
  let module D = U.Definition in
  let%bind unt_ty = spanned_lift unt_ty in
  match unt_ty with
  | U.Named name -> (
      let f ((name', _), _) = Nfc_string.equal name' name in
      match Array.find ~f (Context.names ctxt) with
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
      let%bind params =
        Return.Array.of_sequence ~len:(List.length params)
          (Sequence.map ~f (Sequence.of_list params))
      in
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
      equal f1.ret_ty f2.ret_ty && Array.equal f1.params f2.params ~equal
  | User_defined u1, User_defined u2 -> u1 = u2
  | _ -> false

let structural ty ~(ctxt : Context.t) =
  match ty with
  | Builtin b -> Structural.Builtin b
  | User_defined idx -> Context.user_type_data (Context.user_types ctxt).(idx)

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
        String.concat_sequence ~sep:", "
          (Sequence.map ~f (Array.to_sequence params))
      in
      String.concat ["func("; params; ") -> "; to_string ret_ty ~ctxt]
  | User_defined idx ->
      let (name, _), _ = (Context.names ctxt).(idx) in
      (name :> string)
