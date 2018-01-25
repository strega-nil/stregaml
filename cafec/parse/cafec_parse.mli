open Cafec_spanned.Prelude

module Ast : module type of Ast

module Error : module type of Error

val parse : string -> (Ast.t, Error.t) spanned_result
