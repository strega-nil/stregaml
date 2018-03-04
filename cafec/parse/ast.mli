open Cafec_spanned.Prelude

module Type : sig
  type t = Named of string

  val print : t -> unit
end

module Expr : sig
  type t =
    | Unit_literal
    | Bool_literal of bool
    | Integer_literal of int
    | If_else of (t spanned * t spanned * t spanned)
    | Variable of string
    | Call of (t spanned * t spanned list)
end

module Function : sig
  type t =
    { name: string
    ; params: (string * Type.t spanned) list
    ; ret_ty: Type.t spanned option
    ; expr: Expr.t spanned }
end

type t = {funcs: Function.t spanned list}

val make : Function.t spanned list -> t

val print : t -> unit
