module Fixity : sig
  type anyfix = Any_fixity

  type nonfix = Nonfix_fixity

  type infix = Infix_fixity

  type prefix = Prefix_fixity

  type 'f t =
    | Nonfix : nonfix t
    | Infix : infix t
    | Prefix : prefix t
    | Anyfix : _ t -> anyfix t

  val erase : _ t -> anyfix t
end

type anyfix = Fixity.anyfix

type nonfix = Fixity.nonfix

type infix = Fixity.infix

type prefix = Fixity.prefix

type 'f fixity = 'f Fixity.t =
  | Nonfix : nonfix fixity
  | Infix : infix fixity
  | Prefix : prefix fixity
  | Anyfix : _ fixity -> anyfix fixity

(* mostly important for printing *)

type kind =
  | Identifier : kind
  | Operator : kind

type _ t =
  | Name : {string : Nfc_string.t; fixity : 'f fixity; kind : kind} -> 'f t

val erase : _ t -> anyfix t

val nonfix : _ t -> nonfix t option

val string : _ t -> Nfc_string.t

val fixity : 'f t -> 'f fixity

val kind : _ t -> kind

val to_ident_string : _ t -> string

val equal : _ t -> _ t -> bool
