type t = Unit | Bool | Int | Function of {params: t list; ret_ty: t}

val print_list : t list -> unit

val print : t -> unit
