type t =
  | Unit
  | Bool
  | Int
  | Record of (string * t) list
  | Function of {params: t list; ret_ty: t}

let rec equal l r =
  match (l, r) with
  | Unit, Unit -> true
  | Bool, Bool -> true
  | Int, Int -> true
  | Function fl, Function fr ->
      equal fl.ret_ty fr.ret_ty && List.equal fl.params fr.params ~equal
  | _ -> false


let rec to_string ty =
  match ty with
  | Unit -> "unit"
  | Bool -> "bool"
  | Int -> "int"
  | Record _members -> "record-type"
  | Function {params; ret_ty} ->
      let params =
        let f ty = to_string ty in
        String.concat ~sep:", " (List.map ~f params)
      in
      String.concat ["func"; params; " -> "; to_string ret_ty]
