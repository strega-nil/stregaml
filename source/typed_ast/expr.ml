module Builtin = struct
  type t = Less_eq | Add | Sub | Mul

  let equal lhs rhs =
    match (lhs, rhs) with
    | Less_eq, Less_eq -> true
    | Add, Add -> true
    | Sub, Sub -> true
    | Mul, Mul -> true
    | _ -> false
end

type t =
  | Unit_literal
  | Bool_literal of bool
  | Integer_literal of int
  | If_else of t Spanned.t * t Spanned.t * t Spanned.t
  | Call of t Spanned.t * t Spanned.t list
  | Record_literal of
    { ty: Type.Context.index option
    ; members: (string * t Spanned.t) Spanned.t list }
  | Record_access of t Spanned.t * string
  | Builtin of Builtin.t
  | Global_function of int
  | Parameter of int
