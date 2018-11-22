module Ast = Ast
module Error = Error
open! Types.Pervasives

val parse : Stdio.In_channel.t -> Ast.t result
