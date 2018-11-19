open! Types.Pervasives

type t

val is_operator_ident : Ident.t -> bool

val make : Stdio.In_channel.t -> t

val next_token : t -> Token.t result
