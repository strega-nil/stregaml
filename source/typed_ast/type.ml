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
  | Record m1, Record m2 ->
      let equal (name1, ty1) (name2, ty2) =
        String.equal name1 name2 && equal ty1 ty2
      in
      List.equal m1 m2 ~equal
  | Function f1, Function f2 ->
      equal f1.ret_ty f2.ret_ty && List.equal f1.params f2.params ~equal
  | _ -> false


let rec to_string ty =
  match ty with
  | Unit -> "unit"
  | Bool -> "bool"
  | Int -> "int"
  | Record members ->
      let members =
        let f (name, ty) = String.concat [name; ": "; to_string ty] in
        String.concat ~sep:"; " (List.map ~f members)
      in
      String.concat ["< "; members; " >"]
  | Function {params; ret_ty} ->
      let params =
        let f ty = to_string ty in
        String.concat ~sep:", " (List.map ~f params)
      in
      String.concat ["func"; params; " -> "; to_string ret_ty]
