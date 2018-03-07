open Cafec_spanned.Prelude

module Error : module type of Error

module Expr : module type of Expr

type func_decl = Internal.func_decl =
  {fname: string; params: (string * Type.t) list; ret_ty: Type.t}

(*
type type_kind = Internal.type_kind = Type_alias of Type.t

type type_def = Internal.type_def = {tname: string; kind: type_kind}
*)

type t

val make : Cafec_parse.Ast.t -> (t, Error.t * Type.context) spanned_result

(*
val number_of_types : t -> int

val type_seq : t -> type_def spanned seq
*)

val number_of_functions : t -> int

val function_seq : t -> (func_decl spanned * Expr.t spanned) seq
