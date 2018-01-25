module Error : module type of Error

open Cafec_spanned.Prelude

type t

val make : Cafec_parse.Ast.t -> (t, Error.t) spanned_result

val run : t -> unit
