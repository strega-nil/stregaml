module Ast = Ast
module Error = Error
module Type = Type
open! Types.Pervasives

val parse : Stdio.In_channel.t -> Ast.t result
