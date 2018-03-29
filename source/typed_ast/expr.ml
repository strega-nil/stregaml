module Spanned = Cafec_containers.Spanned

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
  | If_else of (t Spanned.t * t Spanned.t * t Spanned.t)
  | Call of (t Spanned.t * t Spanned.t list)
  | Struct_literal of (Type.t * (int * t Spanned.t) list)
  | Struct_access of (t Spanned.t * int)
  | Builtin of Builtin.t
  | Global_function of int
  | Parameter of int
