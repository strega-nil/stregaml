type kind =
  | Identifier
  | Infix
  | Prefix

type t = {string: Nfc_string.t; kind: kind}

val to_string : t -> string

val equal : t -> t -> bool
