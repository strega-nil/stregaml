open Cafec_spanned.Prelude

module Type_declaration : sig
  type t
end

module Type : sig
  type builder = Named of string
 and t = builder spanned
end

module Expr : sig
  type builder =
    | Unit_literal
    | Bool_literal of bool
    | Integer_literal of int
    | If_else of (t * t * t)
    | Variable of string
    | Call of (t * t list)

  and t = builder spanned
end

module Function : sig
  type builder =
    { name: string
    ; params: (string * Type.t) list
    ; ret_ty: Type.t option
    ; expr: Expr.t }

  and t = builder spanned
end

type t = {funcs: Function.t list}

val make : Function.t list -> t

val print : t -> unit
