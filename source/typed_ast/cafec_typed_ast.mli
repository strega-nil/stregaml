module Error = Error
module Expr = Expr
module Type = Type

module Function : sig
  type t = {name: string; params: (string * Type.t) list; ret_ty: Type.t}
end

(*
type type_kind = Internal.type_kind = Type_alias of Type.t

type type_def = Internal.type_def = {tname: string; kind: type_kind}
*)

type t

val make : Cafec_parse.Ast.t -> (t, Error.t) Spanned.Result.t

(*
val number_of_types : t -> int

val type_seq : t -> type_def spanned seq
*)

val number_of_functions : t -> int

val function_seq : t -> (Function.t Spanned.t * Expr.t Spanned.t) Sequence.t
