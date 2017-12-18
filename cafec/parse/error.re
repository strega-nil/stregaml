open Spanned.Prelude;

type expected_token =
  | Expected_specific(Token.t)
  | Expected_item_declarator
  | Expected_identifier_or_under
  | Expected_identifier
  | Expected_expression
  | Expected_expression_follow;

type t =
  | Unclosed_comment
  | Malformed_number_literal(string)
  | Reserved_token(string)
  | Unrecognized_character(char)
  | Unexpected_token(expected_token, Token.t);

let print_expected = (exp) =>
  switch exp {
  | Expected_specific(tok) => Token.print(tok)
  | Expected_item_declarator => print_string("either `func` or `type`")
  | Expected_identifier => print_string("an identifier")
  | Expected_identifier_or_under => print_string("an identifier or `_`")
  | Expected_expression => print_string("the start of an expression")
  | Expected_expression_follow => print_string("an operator, semicolon, or closing brace (`}`, `)`)")
  };

let print = (err) =>
  switch err {
  | Malformed_number_literal(n) => Printf.printf("malformed number literal: %s", n)
  | Reserved_token(tok) => Printf.printf("reserved token: %s", tok)
  | Unrecognized_character(ch) =>
    Printf.printf("unrecognized character: `%c` (%d)", ch, Char.code(ch))
  | Unclosed_comment => print_string("unclosed comment")
  | Unexpected_token(exp, tok) =>
    print_string("expected: ");
    print_expected(exp);
    print_string(", found: ");
    Token.print(tok);
  };

let print_spanned = (err, sp) => {
  print(err);
  Printf.printf(
    " from (%d, %d) to (%d, %d)",
    sp.start_line,
    sp.start_column,
    sp.end_line,
    sp.end_column
  );
};
