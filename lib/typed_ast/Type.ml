open! Types.Pervasives
module Parse = Cafec_Parse
include Types.Type

module Category = struct
  include Types.Type_Category

  let mutability_to_string = function
    | Mutable -> "mut"
    | Immutable -> "ref"

  let mutability_equal = Parse.Type.mutability_equal

  let mutability_compatible m1 m2 =
    match (m1, m2) with
    | Mutable, Mutable -> true
    | Immutable, Immutable -> true
    | Mutable, Immutable -> true
    | Immutable, Mutable -> false

  let rec equal : type a b. a t -> b t -> bool =
   fun c1 c2 ->
    match (c1, c2) with
    | Value, Value -> true
    | Place m1, Place m2 -> mutability_equal m1 m2
    | Any a1, Any a2 -> equal a1 a2
    | Any a1, a2 -> equal a1 a2
    | a1, Any a2 -> equal a1 a2
    | _ -> false

  let erase : type a. a t -> any t = function
    | Any a -> Any a
    | cat -> Any cat

  let rec compatible : type a b. a t -> b t -> bool =
   fun c1 c2 ->
    match (c1, c2) with
    | Value, Value -> true
    | Place _, Value -> true
    | Place m1, Place m2 -> mutability_compatible m1 m2
    | Any a1, Any a2 -> compatible a1 a2
    | Any a1, a2 -> compatible a1 a2
    | a1, Any a2 -> compatible a1 a2
    | Value, Place _ -> false
end

let erase (type cat) (ty : cat t) : Category.any t =
  match ty with Any _ -> ty | x -> Any x

module Structural = struct
  include Types.Type_Structural
  module Kind = Types.Type_Structural_Kind
end

module Context = struct
  type user_type = User_type : {data : Structural.t} -> user_type

  let user_type_data (User_type r) = r.data

  type t =
    | Context :
        { user_types : user_type Array.t
        ; names :
            (Nfc_string.t Spanned.t * Category.value Types.Type.t)
            Array.t }
        -> t

  let make lst =
    let module PType = Parse.Type in
    let defs, defs_len, aliases, aliases_len =
      let module Def = PType.Definition in
      let rec helper defs defs_len aliases aliases_len = function
        | [] -> (defs, defs_len, aliases, aliases_len)
        | (Def.Definition {name; kind; attributes}, _) :: rest -> (
            assert (List.is_empty attributes) ;
            match kind with
            | Def.Alias ty ->
                helper defs defs_len ((name, ty) :: aliases)
                  (aliases_len + 1) rest
            | Def.User_defined {data} ->
                helper ((name, data) :: defs) (defs_len + 1) aliases
                  aliases_len rest )
      in
      helper [] 0 [] 0 lst
    in
    let rec get_ast_type : type cat.
        cat PType.t -> cat Types.Type.t result =
     fun pty ->
      (* returns -1 if not found *)
      let rec find_index name index = function
        | [] -> -1
        | ((name', _), _) :: _ when Nfc_string.equal name name' ->
            index
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
      | PType.Reference (p, _) ->
          let%bind pointee = get_ast_type p in
          return (Builtin (Reference pointee))
      | PType.Function {params; ret_ty} ->
          let f (x, _) = get_ast_type x in
          let%bind params = Return.Array.of_list_map ~f params in
          let%bind ret_ty =
            match ret_ty with
            | Some (ty, _) -> Return.map ~f:erase (get_ast_type ty)
            | None -> return (Any (Builtin Unit))
          in
          return (Builtin (Function {params; ret_ty}))
      | _ -> failwith "get_ast_type"
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
      let equal ((name, _), _) ((name', _), _) =
        Nfc_string.equal name name'
      in
      match Array.find_nonconsecutive_duplicates names ~equal with
      | Some (((name, _), _), _) ->
          return_err (Error.Type_defined_multiple_times name)
      | None -> return ()
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
            Return.Array.of_list_map ~f lst
          in
          let (Data.Data {kind; members}) = def in
          let%bind members = typed_members members in
          match kind with
          | Data.Record -> return (Structural.Record members)
          | Data.Variant -> return (Structural.Variant members)
        in
        return (User_type {data})
      in
      Return.Array.of_list_map ~f defs
    in
    return (Context {user_types; names})

  let empty =
    Context {user_types = Array.empty (); names = Array.empty ()}

  let user_types (Context r) = r.user_types

  let names (Context r) = r.names
end

let rec of_untyped : type cat.
    cat Parse.Type.t Spanned.t -> ctxt:Context.t -> cat t result =
 fun (unt_ty, unt_sp) ~ctxt ->
  let module U = Parse.Type in
  let module D = U.Definition in
  let%bind () = with_span unt_sp in
  match unt_ty with
  | U.Any ty ->
      let ret =
        Return.map ~f:(fun x -> Any x) (of_untyped (ty, unt_sp) ~ctxt)
      in
      (* otherwise, ocaml doesn't unify cat and U.any *)
      (ret : cat t result)
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
  | U.Reference pointee ->
      let%bind pointee = of_untyped pointee ~ctxt in
      return (Builtin (Reference pointee))
  | U.Function {params; ret_ty} ->
      let f ty = of_untyped ty ~ctxt in
      let default = return (erase (Builtin Unit)) in
      let%bind params = Return.Array.of_list_map ~f params in
      let%bind ret_ty = Option.value_map ~f ~default ret_ty in
      return (Builtin (Function {params; ret_ty}))
  | U.Place {mutability; ty} ->
      let%bind ty = of_untyped ty ~ctxt in
      let mutability, _ = mutability in
      return (Place {mutability; ty})

let rec equal : type a b. a t -> b t -> bool =
 fun l r ->
  match (l, r) with
  | Builtin Unit, Builtin Unit -> true
  | Builtin Bool, Builtin Bool -> true
  | Builtin Int32, Builtin Int32 -> true
  | Builtin (Reference l), Builtin (Reference r) -> equal l r
  | Builtin (Function f1), Builtin (Function f2) ->
      equal f1.ret_ty f2.ret_ty
      && Array.equal f1.params f2.params ~equal
  | User_defined u1, User_defined u2 -> u1 = u2
  | Place {mutability = m1; ty = ty1}, Place {mutability = m2; ty = ty2}
    ->
      Category.mutability_equal m1 m2 && equal ty1 ty2
  | Any a1, Any a2 -> equal a1 a2
  | Any a1, a2 -> equal a1 a2
  | a1, Any a2 -> equal a1 a2
  | _ -> false

let rec value_type : type cat. cat t -> Category.value t = function
  | Any ty -> value_type ty
  | Place {ty; _} -> ty
  | Builtin _ as ty -> ty
  | User_defined _ as ty -> ty

let rec category : type cat. cat t -> cat Category.t = function
  | Any ty -> Category.Any (category ty)
  | Place {mutability; _} -> Category.Place mutability
  | Builtin _ -> Category.Value
  | User_defined _ -> Category.Value

let compatible : type a b. a t -> b t -> bool =
 fun ty_from ty_to ->
  Category.compatible (category ty_from) (category ty_to)
  && equal (value_type ty_from) (value_type ty_to)

let structural ty ~(ctxt : Context.t) =
  match ty with
  | Builtin b -> Structural.Builtin b
  | User_defined idx ->
      Context.user_type_data (Context.user_types ctxt).(idx)

let rec local_type : type cat. cat t -> is_mut:bool -> Category.place t
    =
 fun ty ~is_mut ->
  let local_value_type : Category.value t -> Category.place t =
   fun ty ->
    let mutability =
      if is_mut then Category.Mutable else Category.Immutable
    in
    Place {mutability; ty}
  in
  match ty with
  | Any ty -> local_type ty ~is_mut
  | Place _ as ty -> ty
  | Builtin _ as ty -> local_value_type ty
  | User_defined _ as ty -> local_value_type ty

let rec to_type_and_category : type a.
    a t -> Category.value t * a Category.t = function
  | Builtin _ as ty -> (ty, Category.Value)
  | User_defined _ as ty -> (ty, Category.Value)
  | Place {mutability; ty} -> (ty, Category.Place mutability)
  | Any ty ->
      let ty, cat = to_type_and_category ty in
      (ty, (Category.erase cat : a Category.t))

let rec of_type_and_category : type a.
    Category.value t * a Category.t -> a t =
 fun (ty, cat) ->
  match cat with
  | Category.Value -> ty
  | Category.Place mutability -> Place {mutability; ty}
  | Category.Any cat -> Any (of_type_and_category (ty, cat))

let rec to_string : type cat. cat t -> ctxt:Context.t -> string =
 fun ty ~ctxt ->
  match ty with
  | Builtin Unit -> "Unit"
  | Builtin Bool -> "Bool"
  | Builtin Int32 -> "Int32"
  | Builtin (Reference pointee) -> "&" ^ to_string pointee ~ctxt
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
  | Place {mutability; ty} ->
      String.concat
        [ Category.mutability_to_string mutability
        ; " "
        ; to_string ty ~ctxt ]
  | Any ty -> to_string ty ~ctxt
