module Spanned = Cafec_containers.Spanned

module Type : sig
  type t =
    | Named of string
    | Function of t Spanned.t list * t Spanned.t option

  val to_string : t -> string

  module Data : sig
    type nonrec t = Record of (string * t) Spanned.t list

    val to_string : t -> string
  end

  module Definition : sig
    type kind = Alias of t | User_defined of {data: Data.t}

    type t = {name: string; kind: kind}
  end
end

module rec Stmt : sig
  type t = Expression of Expr.t

  val to_string : t -> indent:int -> string
end

and Expr : sig
  type block = {stmts: Stmt.t Spanned.t list; expr: t Spanned.t option}

  and t =
    | Unit_literal
    | Bool_literal of bool
    | Integer_literal of int
    | If_else of t Spanned.t * block Spanned.t * block Spanned.t
    | Variable of {path: string list; name: string}
    | Block of block
    | Call of t Spanned.t * t Spanned.t list
    | Record_literal of
        { ty: Type.t
        ; members: (string * t Spanned.t) Spanned.t list }
    | Record_access of t Spanned.t * string

  val to_string : t -> indent:int -> string

  val block_to_string : block -> indent:int -> string
end

module Func : sig
  type t =
    { name: string
    ; params: (string * Type.t) Spanned.t list
    ; ret_ty: Type.t Spanned.t option
    ; body: Expr.block Spanned.t }

  val to_string : t -> string
end

type t = {funcs: Func.t Spanned.t list; types: Type.Definition.t Spanned.t list}

val to_string : t -> string
