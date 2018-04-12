type t

val make : string -> t

val next_token : t -> (Token.t, Error.t) Spanned.Result.t
