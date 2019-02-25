module Ast = Ast
module Error = Error
module Token = Token
module Type = Type
open! Types.Pervasives

module type Language = Types.Language

val parse :
  Stdio.In_channel.t -> lang:(module Language) -> Ast.t result
