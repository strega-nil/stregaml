type t =
  | User_defined of int
  | Unit
  | Bool
  | Int
  | Function of {params: t list; ret_ty: t}

module Definition = struct
  type typ = t

  type t = Alias of typ | Struct of (string * typ) list
end

type context = string list

let rec equal l r =
  match (l, r) with
  | Unit, Unit -> true
  | Bool, Bool -> true
  | Int, Int -> true
  | Function fl, Function fr ->
      equal fl.ret_ty fr.ret_ty && List.equal fl.params fr.params ~equal
  | User_defined l, User_defined r -> l = r
  | _ -> false


let rec to_string ty ~ctxt =
  match ty with
  | Unit -> "unit"
  | Bool -> "bool"
  | Int -> "int"
  | Function {params; ret_ty} ->
      let params =
        let f ty = to_string ty ~ctxt in
        String.concat ~sep:", " (List.map ~f params)
      in
      String.concat ["func"; params; " -> "; to_string ret_ty ~ctxt]
  | User_defined i -> List.nth_exn ctxt i
