open Cafec_spanned.Prelude

module Error : module type of Error

module Expr : module type of Expr

type func_decl = Internal.decl =
  {name: string; params: (string * Type.t) list; ret_ty: Type.t}

type t

val make : Cafec_parse.Ast.t -> (t, Error.t) spanned_result

val function_seq : t -> (func_decl spanned * Expr.t spanned) seq

val number_of_functions : t -> int
