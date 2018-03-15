type t = Builtin of builtin | User_defined of int

and builtin =
  | Builtin_unit
  | Builtin_bool
  | Builtin_int
  | Builtin_function of {params: t list; ret_ty: t}

type definition = Def_alias of t | Def_struct of (string * t) list

type context = string list

val output : Stdio.Out_channel.t -> t -> context -> unit

val output_list : Stdio.Out_channel.t -> t list -> context -> unit

val equal : t -> t -> bool
