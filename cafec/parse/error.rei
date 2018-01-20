open Cafec_spanned.Prelude;

type expected_token =
  | Expected_specific(Token.t)
  | Expected_item_declarator
  | Expected_identifier_or_under
  | Expected_identifier
  | Expected_expression
  | Expected_expression_follow;

type t =
  | Unclosed_comment
  | Malformed_number_literal
  | Reserved_token(string)
  | Unrecognized_character(char)
  | Unexpected_token(expected_token, Token.t);

module Monad_spanned
: Interfaces.Result_monad.Interface
  with type error = t
  and type t('a) = spanned_result('a, t);

let print: t => unit;
let print_spanned: spanned(t) => unit;
