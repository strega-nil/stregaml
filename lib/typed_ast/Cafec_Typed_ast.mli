module Error = Error_print
module Binding = Ast.Binding
module Expr = Ast.Expr
module Stmt = Ast.Stmt
module Type = Type
module Function_declaration = Internal.Function_declaration

(*
type type_kind = Internal.type_kind = Type_alias of Type.t

type type_def = Internal.type_def = {tname: string; kind: type_kind}
*)

type t

val make :
  Cafec_Parse.Ast.t -> (t, Error.t * Type.Context.t) Spanned.Result.t

(*
val number_of_types : t -> int

val type_seq : t -> type_def spanned seq
*)

val number_of_functions : t -> int

val function_seq :
     t
  -> (Function_declaration.t Spanned.t * Expr.Block.t Spanned.t)
     Sequence.t
