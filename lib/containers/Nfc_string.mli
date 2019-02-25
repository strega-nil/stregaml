(*
  note: guaranteed to be NFC-normalized
  for purposes of to_string and equal
*)

type t = private string

val of_uchar_list : Uchar.t list -> t

val of_string : string -> t

val of_string_unsafe : string -> t

val uchar_to_string : Uchar.t -> string

val empty : t

val equal : t -> t -> bool
