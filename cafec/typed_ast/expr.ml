open Cafec_spanned.Prelude

type builtin = Builtin_less_eq | Builtin_add | Builtin_sub

type t =
  | Unit_literal
  | Bool_literal of bool
  | Integer_literal of int
  | If_else of (t spanned * t spanned * t spanned)
  | Call of (t spanned * t spanned list)
  | Builtin of builtin
  | Global_function of int
  | Parameter of int
