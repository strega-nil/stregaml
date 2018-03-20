module Spanned = Cafec_containers.Spanned
open Spanned.Prelude

type expected_token =
  | Expected_specific of Token.t
  | Expected_item_declarator
  | Expected_type_definition
  | Expected_identifier_or_under
  | Expected_identifier
  | Expected_type
  | Expected_expression
  | Expected_expression_follow

type t =
  | Unclosed_comment
  | Operator_including_comment_token of string
  | Malformed_number_literal
  | Reserved_token of string
  | Unrecognized_character of char
  | Unexpected_token of (expected_token * Token.t)

module Out = Stdio.Out_channel

let output_expected f = function
  | Expected_specific tok -> Token.output f tok
  | Expected_item_declarator -> Out.output_string f "either `func` or `type`"
  | Expected_type_definition ->
      Out.output_string f "`struct`, `variant`, or a type"
  | Expected_identifier -> Out.output_string f "an identifier"
  | Expected_identifier_or_under -> Out.output_string f "an identifier or `_`"
  | Expected_type -> Out.output_string f "the start of a type"
  | Expected_expression -> Out.output_string f "the start of an expression"
  | Expected_expression_follow ->
      Out.output_string f
        "an operator, semicolon, comma, dot, or closing brace (`}`, `)`)"


let output f = function
  | Malformed_number_literal -> Out.output_string f "malformed number literal"
  | Operator_including_comment_token s ->
      Out.fprintf f "operator %s includes a sequence of comment characters" s
  | Reserved_token tok -> Out.fprintf f "reserved token: %s" tok
  | Unrecognized_character ch ->
      Out.fprintf f "unrecognized character: `%c` (%d)" ch (Char.to_int ch)
  | Unclosed_comment -> Out.output_string f "unclosed comment"
  | Unexpected_token (exp, tok) ->
      Out.output_string f "expected: " ;
      output_expected f exp ;
      Out.output_string f ", found: " ;
      Token.output f tok


let output_spanned f (err, sp) =
  output f err ;
  Out.fprintf f "\n  from (%d, %d) to (%d, %d)" sp.start_line sp.start_column
    sp.end_line sp.end_column
