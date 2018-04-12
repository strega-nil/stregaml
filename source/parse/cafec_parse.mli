module Ast = Ast

module Error = Error

val parse : string -> (Ast.t, Error.t) Spanned.Result.t
