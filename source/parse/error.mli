open Cafec_containers.Spanned.Prelude

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

val output : Stdio.Out_channel.t -> t -> unit

val output_spanned : Stdio.Out_channel.t -> t spanned -> unit
