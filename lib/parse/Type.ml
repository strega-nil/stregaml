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
  | Tuple xs ->
      let f (x, _) = to_string x in
      let xs = String.concat ~sep:", " (List.map xs ~f) in
      String.concat ["("; xs; ")"]
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
    let kind, data =
      match data with
      | Record {fields} ->
          let f ((name, ty), _) =
            let (name, _) : Nfc_string.t Spanned.t = name in
            let (ty, _) = ty in
            String.concat
              ["\n    "; (name :> string); ": "; to_string ty; ";"]
          in
          let fields = String.concat (List.map fields ~f) in
          (Token.Keyword.Record, fields)
      | Variant {variants} ->
          let f ((name, ty), _) =
            let (name, _) : Nfc_string.t Spanned.t = name in
            match ty with
            | Some (ty, _) ->
                String.concat
                  ["\n    "; (name :> string); ": "; to_string ty; ";"]
            | None -> String.concat ["\n    "; (name :> string); ";"]
          in
          let variants = String.concat (List.map variants ~f) in
          (Token.Keyword.Variant, variants)
      | Integer {bits} ->
          let data =
            String.concat ["\n    bits = "; Int.to_string bits; ";"]
          in
          (Token.Keyword.Integer, data)
    in
    let name = match name with Some n -> " " ^ n | None -> "" in
    String.concat
      [Lang.keyword_to_string ~lang kind; name; " {"; data; "\n  }"]
end

module Definition = Types.Type_Definition
