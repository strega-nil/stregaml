type fixity = Nonfix : fixity | Infix : fixity | Prefix : fixity

(* mostly important for printing *)

type kind = Identifier : kind | Operator : kind

type t = Name : {string: Nfc_string.t; fixity: fixity; kind: kind} -> t

val string : t -> Nfc_string.t

val fixity : t -> fixity

val kind : t -> kind

val to_ident_string : t -> string

val equal : t -> t -> bool
