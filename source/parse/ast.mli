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

module Func : sig
  type t =
    { name: string
    ; params: (string * Type.t Spanned.t) list
    ; ret_ty: Type.t Spanned.t option
    ; expr: Expr.t Spanned.t }

  val to_string : t -> string
end

module Type_definition : sig
  module Kind : sig
    type t =
      | Alias of Type.t Spanned.t
      | Struct of (string * Type.t Spanned.t) list

    val to_string : t -> string
  end

  type t = {name: string; kind: Kind.t}

  val to_string : t -> string
end

type t = {funcs: Func.t Spanned.t list; types: Type_definition.t Spanned.t list}

val to_string : t -> string
