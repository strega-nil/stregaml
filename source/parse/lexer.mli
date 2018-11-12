open! Types.Pervasives

type t

val make : Stdio.In_channel.t -> t

val next_token : t -> Token.t result
