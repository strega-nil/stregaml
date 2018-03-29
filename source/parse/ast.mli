module Spanned = Cafec_containers.Spanned

module Type : sig
  type t =
    | Named of string
    | Function of (t Spanned.t list * t Spanned.t option)

  val to_string : t -> string
end

module Expr : sig
  type t =
    | Unit_literal
    | Bool_literal of bool
    | Integer_literal of int
    | If_else of (t Spanned.t * t Spanned.t * t Spanned.t)
    | Variable of string
    | Call of (t Spanned.t * t Spanned.t list)
    | Struct_literal of (Type.t * (string * t Spanned.t) Spanned.t list)
    | Struct_access of (t Spanned.t * string)

  val to_string : t -> indent:int -> string
end

module Item : sig
  type func =
    { fname: string
    ; params: (string * Type.t Spanned.t) list
    ; ret_ty: Type.t Spanned.t option
    ; expr: Expr.t Spanned.t }

  type type_kind =
    | Alias of Type.t Spanned.t
    | Struct of (string * Type.t Spanned.t) list

  type type_def = {tname: string; kind: type_kind}
end

type t = {funcs: Item.func Spanned.t list; types: Item.type_def Spanned.t list}

val to_string : t -> string
