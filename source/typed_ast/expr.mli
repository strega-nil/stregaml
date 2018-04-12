open Cafec_containers

module Builtin : sig
  type t = Less_eq | Add | Sub | Mul

  val equal : t -> t -> bool
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
