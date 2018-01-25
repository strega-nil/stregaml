open Cafec_spanned.Prelude

type t

val lexer: string -> t

val next_token: t -> (Token.t, Error.t) spanned_result
