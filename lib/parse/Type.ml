include Types.Type

let mutability_equal lhs rhs =
  match (lhs, rhs) with
  | Immutable, Immutable -> true
  | Mutable, Mutable -> true
  | _ -> false

let mutability_to_string = function
  | Immutable -> "ref"
  | Mutable -> "mut"

let rec to_string : type cat. cat t -> string = function
  | Named id -> (id :> string)
  | Reference (pointee, _) -> "&" ^ to_string pointee
  | Function {params; ret_ty} ->
      let f (x, _) = to_string x in
      let ret_ty =
        match ret_ty with Some x -> ") -> " ^ f x | None -> ")"
      in
      let params = String.concat ~sep:", " (List.map params ~f) in
      String.concat ["func("; params; ret_ty]
  | Place {mutability = mut, _; ty = ty, _} ->
      String.concat [mutability_to_string mut; " "; to_string ty]
  | Any ty -> to_string ty

module Data = struct
  include Types.Type_Data

  let to_string ?name data ~lang =
    let members_to_string members =
      let f ((name, ty), _) =
        ignore (name : Nfc_string.t) ;
        String.concat
          ["\n    "; (name :> string); ": "; to_string ty; ";"]
      in
      String.concat (List.map members ~f)
    in
    let (Data {kind; members}) = data in
    let kind =
      match kind with
      | Record -> Token.Keyword.Record
      | Variant -> Token.Keyword.Variant
    in
    let name = match name with Some n -> " " ^ n | None -> "" in
    let members = members_to_string members in
    String.concat
      [Lang.keyword_to_string ~lang kind; name; " {"; members; "\n  }"]
end

module Definition = Types.Type_Definition
