type keyword =
  | Keyword_true
  | Keyword_false
  | Keyword_if
  | Keyword_else
  | Keyword_func;

type token =
  | Token_open_paren
  | Token_close_paren
  | Token_open_brace
  | Token_close_brace
  | Token_keyword(keyword)
  | Token_identifier(string)
  | Token_operator(string)
  | Token_int_literal(int)
  | Token_colon
  | Token_equals
  | Token_semicolon
  | Token_comma;

type error =
  | Error_unclosed_comment
  | Error_malformed_number_literal(string)
  | Error_reserved_token(string)
  | Error_unrecognized_character(char);
