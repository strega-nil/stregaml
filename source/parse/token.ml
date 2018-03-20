module Spanned = Cafec_containers.Spanned

module Prelude = struct
  type keyword =
    | Keyword_true
    | Keyword_false
    | Keyword_if
    | Keyword_else
    | Keyword_func
    | Keyword_type
    | Keyword_struct
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
    | Dot
    | Comma
    | Eof
end

include Prelude
module Out = Stdio.Out_channel

let output_keyword f = function
  | Keyword_true -> Out.output_string f "true"
  | Keyword_false -> Out.output_string f "false"
  | Keyword_if -> Out.output_string f "if"
  | Keyword_else -> Out.output_string f "else"
  | Keyword_func -> Out.output_string f "func"
  | Keyword_type -> Out.output_string f "type"
  | Keyword_struct -> Out.output_string f "struct"
  | Keyword_underscore -> Out.output_char f '_'


let output f = function
  | Open_paren -> Out.output_string f "open paren `(`"
  | Close_paren -> Out.output_string f "close paren `)`"
  | Open_brace -> Out.output_string f "open brace `{`"
  | Close_brace -> Out.output_string f "close brace `}`"
  | Keyword kw ->
      Out.output_string f "keyword: `" ;
      output_keyword f kw ;
      Out.output_char f '`'
  | Operator op ->
      Out.output_string f "operator: `" ;
      Out.output_string f op ;
      Out.output_char f '`'
  | Identifier id ->
      Out.output_string f "identifier: `" ;
      Out.output_string f id ;
      Out.output_char f '`'
  | Integer_literal i ->
      Out.output_string f "int literal: " ;
      Out.fprintf f "%d" i
  | Arrow -> Out.output_string f "arrow `->`"
  | Colon -> Out.output_string f "colon `:`"
  | Equals -> Out.output_string f "equals `=`"
  | Semicolon -> Out.output_string f "semicolon `;`"
  | Comma -> Out.output_string f "comma `,`"
  | Dot -> Out.output_string f "dot `.`"
  | Eof -> Out.output_string f "end of file"


let output_spanned f (tok, sp) =
  output f tok ; Out.output_string f " at " ; Spanned.output_span f sp


let equal lhs rhs =
  match (lhs, rhs) with
  | Open_paren, Open_paren -> true
  | Close_paren, Close_paren -> true
  | Open_brace, Open_brace -> true
  | Close_brace, Close_brace -> true
  | Keyword kw1, Keyword kw2 -> (
    match (kw1, kw2) with
    | Keyword_true, Keyword_true -> true
    | Keyword_false, Keyword_false -> true
    | Keyword_if, Keyword_if -> true
    | Keyword_else, Keyword_else -> true
    | Keyword_func, Keyword_func -> true
    | Keyword_type, Keyword_type -> true
    | Keyword_struct, Keyword_struct -> true
    | Keyword_underscore, Keyword_underscore -> true
    | _ -> false )
  | Identifier id1, Identifier id2 -> String.equal id1 id2
  | Operator op1, Operator op2 -> String.equal op1 op2
  | Integer_literal i1, Integer_literal i2 -> i1 = i2
  | Arrow, Arrow -> true
  | Colon, Colon -> true
  | Equals, Equals -> true
  | Semicolon, Semicolon -> true
  | Dot, Dot -> true
  | Comma, Comma -> true
  | Eof, Eof -> true
  | _ -> false
