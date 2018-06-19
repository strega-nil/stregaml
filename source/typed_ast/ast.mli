module rec Stmt : sig
  type t = Expression of Expr.t
end

and Expr : sig
  module Builtin : sig
    type t = Less_eq | Add | Sub | Mul

    val equal : t -> t -> bool
  end

  type block = {stmts: Stmt.t list; expr: t option}

  and t =
    | Unit_literal
    | Bool_literal of bool
    | Integer_literal of int
    | If_else of t Spanned.t * block Spanned.t * block Spanned.t
    | Call of t Spanned.t * t Spanned.t list
    | Block of block
    | Record_literal of
        { ty: Type.Context.index
        ; members: (string * t Spanned.t) Spanned.t list }
    | Record_access of t Spanned.t * string
    | Builtin of Builtin.t
    | Global_function of int
    | Parameter of int
end
