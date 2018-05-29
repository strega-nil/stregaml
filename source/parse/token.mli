module Keyword : sig
  type t = True | False | If | Else | Func | Type | Data | Alias | Underscore

  val equal : t -> t -> bool

  val to_string : t -> string
end

type t =
  | Open_paren
  | Close_paren
  | Open_brace
  | Close_brace
  | Open_record
  | Close_record
  | Keyword of Keyword.t
  | Identifier of string
  | Operator of string
  | Integer_literal of int
  | Arrow
  | Colon
  | Double_colon
  | Equals
  | Semicolon
  | Dot
  | Comma
  | Eof

val equal : t -> t -> bool

val to_string : t -> string
