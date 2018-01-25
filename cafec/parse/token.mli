module Prelude : sig
  type keyword =
    | Keyword_true
    | Keyword_false
    | Keyword_if
    | Keyword_else
    | Keyword_let
    | Keyword_underscore

  type t =
    | Open_paren
    | Close_paren
    | Open_brace
    | Close_brace
    | Keyword of keyword
    | Identifier of string
    | Operator of string
    | Integer_literal of int
    | Arrow
    | Colon
    | Equals
    | Semicolon
    | Comma
    | Eof
end

include module type of struct
    include Prelude
end

val print : t -> unit

val print_spanned : t Cafec_spanned.spanned -> unit
