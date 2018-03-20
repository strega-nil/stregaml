open Cafec_containers.Spanned.Prelude

module Builtin : sig
  type t = Less_eq | Add | Sub | Mul

  val equal : t -> t -> bool
end = struct
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
  | If_else of (t spanned * t spanned * t spanned)
  | Call of (t spanned * t spanned list)
  | Struct_literal of (Type.t * (int * t spanned) list)
  | Struct_access of (t spanned * int)
  | Builtin of Builtin.t
  | Global_function of int
  | Parameter of int
