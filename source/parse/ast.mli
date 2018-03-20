open Cafec_containers.Spanned.Prelude

module Type : sig
  type t = Named of string | Function of (t spanned list * t spanned option)

  val output : Stdio.Out_channel.t -> t -> unit
end

module Expr : sig
  type t =
    | Unit_literal
    | Bool_literal of bool
    | Integer_literal of int
    | If_else of (t spanned * t spanned * t spanned)
    | Variable of string
    | Call of (t spanned * t spanned list)
    | Struct_literal of (Type.t * (string * t spanned) spanned list)
    | Struct_access of (t spanned * string)
end

module Item : sig
  type func =
    { fname: string
    ; params: (string * Type.t spanned) list
    ; ret_ty: Type.t spanned option
    ; expr: Expr.t spanned }

  type type_kind =
    | Alias of Type.t spanned
    | Struct of (string * Type.t spanned) list

  type type_def = {tname: string; kind: type_kind}
end

type t = {funcs: Item.func spanned list; types: Item.type_def spanned list}

val output : Stdio.Out_channel.t -> t -> unit
