open Spanned.Prelude;

type t =
  | Unclosed_comment
  | Malformed_number_literal(string)
  | Reserved_token(string)
  | Unrecognized_character(char);

let print = (err) =>
  switch err {
  | Malformed_number_literal(n) => Printf.printf("malformed number literal: %s", n)
  | Reserved_token(tok) => Printf.printf("reserved token: %s", tok)
  | Unrecognized_character(ch) =>
    Printf.printf("unrecognized character: `%c` (%d)", ch, Char.code(ch))
  | Unclosed_comment => print_string("unclosed comment")
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
