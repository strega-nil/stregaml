type t =
  | Unit
  | Bool
  | Int
  | Record of (string * t) list
  | Function of {params: t list; ret_ty: t}

val equal : t -> t -> bool

val to_string : t -> string
