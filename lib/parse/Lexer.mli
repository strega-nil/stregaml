open! Types.Pervasives

type t

val lang : t -> (module Types.Language)

val make : Stdio.In_channel.t -> lang:(module Types.Language) -> t

val next_token : t -> Token.t result
