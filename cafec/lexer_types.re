type token =
  | Token_identifier(string)
  | Token_open_paren
  | Token_close_paren
  | Token_open_brace
  | Token_close_brace;

type error =
  | Error_unrecognized_character(char)
  | Error_unclosed_comment;
