open Cafec_spanned.Prelude

type t = Multiple_function_definitions of (string * span)

module Monad_spanned :
  Interfaces.Result_monad.Interface
  with type error = t
   and type 'a t = ('a, t) spanned_result

val print : t -> unit

val print_spanned : t spanned -> unit
