open Cafec_spanned.Prelude

type t

module Monad_spanned : module type of Cafec_spanned.Monad (struct
  type nonrec t = t end)

val print : t -> unit

val print_spanned : t spanned -> unit
