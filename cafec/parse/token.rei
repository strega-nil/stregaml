module Prelude: {
  type keyword =
    | Keyword_true
    | Keyword_false
    | Keyword_if
    | Keyword_else
    | Keyword_let
    | Keyword_underscore;
  type t =
    | Open_paren
    | Close_paren
    | Open_brace
    | Close_brace
    | Keyword(keyword)
    | Identifier(string)
    | Operator(string)
    | Integer_literal(int)
    | Arrow
    | Colon
    | Equals
    | Semicolon
    | Comma
    | Eof;
};

include (module type of { include Prelude; });

let print: t => unit;
let print_spanned: Cafec_spanned.spanned(t) => unit;
