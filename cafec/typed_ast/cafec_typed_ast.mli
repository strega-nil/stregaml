open Cafec_spanned.Prelude

module Error : module type of Error

module Expr : sig
  type function_index

  type builtin = Builtin_less_eq | Builtin_add | Builtin_sub

  type t =
    | Unit_literal
    | Bool_literal of bool
    | Integer_literal of int
    | If_else of (t spanned * t spanned * t spanned)
    | Call of (t spanned * t spanned list)
    | Builtin of builtin
    | Global_function of function_index
    | Parameter of int
end

type func_type = {params: (string * Type.t) list; ret_ty: Type.t}

type func = {name: string; decl: func_type; expr: Expr.t}

type t

val make : Cafec_parse.Ast.t -> (t, Error.t) spanned_result

val function_by_index : t -> Expr.function_index -> func

val function_index_by_name : t -> string -> Expr.function_index option
