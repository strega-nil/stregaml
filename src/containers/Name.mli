type fixity = Nonfix | Infix | Prefix

(* mostly important for printing *)

type kind = Identifier | Operator

type t = {string: Nfc_string.t; fixity: fixity; kind: kind}

val to_ident_string : t -> string

val equal : t -> t -> bool
