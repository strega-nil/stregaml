module Keyword : sig
  type t = True | False | If | Else | Func | Type | Struct | Underscore

  val equal : t -> t -> bool

  val to_string : t -> string
end

type t =
  | Open_paren
  | Close_paren
  | Open_brace
  | Close_brace
  | Keyword of Keyword.t
  | Identifier of string
  | Operator of string
  | Integer_literal of int
  | Arrow
  | Colon
  | Equals
  | Semicolon
  | Dot
  | Comma
  | Eof

val equal : t -> t -> bool

val to_string : t -> string
