module Ast : module type of Ast

module Error : module type of Error

val parse : string -> (Ast.t, Error.t) Spanned.Result.t
