type t = Builtin of builtin | User_defined of int

and builtin =
  | Builtin_unit
  | Builtin_bool
  | Builtin_int
  | Builtin_function of {params: t list; ret_ty: t}

type definition = Def_alias of t | Def_struct of (string * t) list

type context = string list

let rec equal l r =
  match (l, r) with
  | Builtin bl, Builtin br -> (
    match (bl, br) with
    | Builtin_unit, Builtin_unit -> true
    | Builtin_bool, Builtin_bool -> true
    | Builtin_int, Builtin_int -> true
    | Builtin_function fl, Builtin_function fr ->
        equal fl.ret_ty fr.ret_ty && List.equal fl.params fr.params ~equal
    | _ -> false )
  | User_defined l, User_defined r -> l = r
  | _ -> false


let rec to_string ty ~ctxt =
  match ty with
  | Builtin Builtin_unit -> "unit"
  | Builtin Builtin_bool -> "bool"
  | Builtin Builtin_int -> "int"
  | Builtin Builtin_function {params; ret_ty} ->
      let params =
        let f ty = to_string ty ~ctxt in
        String.concat ~sep:", " (List.map ~f params)
      in
      String.concat ["func"; params; " -> "; to_string ret_ty ~ctxt]
  | User_defined i -> List.nth_exn ctxt i
