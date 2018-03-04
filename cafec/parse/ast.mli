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

module Item : sig
  type func =
    { fname: string
    ; params: (string * Type.t spanned) list
    ; ret_ty: Type.t spanned option
    ; expr: Expr.t spanned }

  type type_kind =
    | Alias of Type.t spanned

  type type_def =
    { tname: string
    ; kind: type_kind }
end

type t = {funcs: Item.func spanned list ; types: Item.type_def spanned list}

val print : t -> unit
