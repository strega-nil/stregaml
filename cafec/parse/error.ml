module Spanned = Cafec_spanned
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

module Monad_spanned = Spanned.Monad (struct
  type nonrec t = t
end)

let print_expected = function
  | Expected_specific tok -> Token.print tok
  | Expected_item_declarator -> print_string "either `func` or `type`"
  | Expected_type_definition -> print_string "`struct`, `variant`, or a type"
  | Expected_identifier -> print_string "an identifier"
  | Expected_identifier_or_under -> print_string "an identifier or `_`"
  | Expected_type -> print_string "the start of a type"
  | Expected_expression -> print_string "the start of an expression"
  | Expected_expression_follow ->
      print_string
        "an operator, semicolon, comma, dot, or closing brace (`}`, `)`)"


let print = function
  | Malformed_number_literal -> print_string "malformed number literal"
  | Operator_including_comment_token s ->
      Printf.printf "operator %s includes a sequence of comment characters" s
  | Reserved_token tok -> Printf.printf "reserved token: %s" tok
  | Unrecognized_character ch ->
      Printf.printf "unrecognized character: `%c` (%d)" ch (Char.code ch)
  | Unclosed_comment -> print_string "unclosed comment"
  | Unexpected_token (exp, tok) ->
      print_string "expected: " ;
      print_expected exp ;
      print_string ", found: " ;
      Token.print tok


let print_spanned (err, sp) =
  print err ;
  Printf.printf "\n  from (%d, %d) to (%d, %d)" sp.start_line sp.start_column
    sp.end_line sp.end_column
