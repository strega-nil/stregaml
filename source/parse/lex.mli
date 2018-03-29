type t

val lexer : string -> t

val next_token : t -> (Token.t, Error.t) Spanned.Result.t
