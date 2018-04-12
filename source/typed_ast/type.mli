type t =
  | User_defined of int
  | Unit
  | Bool
  | Int
  | Function of {params: t list; ret_ty: t}

module Definition :
  sig
    type typ

    type t = Alias of typ | Struct of (string * typ) list
  end
  with type typ := t

type context = string list

val equal : t -> t -> bool

val to_string : t -> ctxt:context -> string
