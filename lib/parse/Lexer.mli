open! Types.Pervasives

type t

val make : Stdio.In_channel.t -> lang:(module Types.Language) -> t

val next_token : t -> Token.t result
