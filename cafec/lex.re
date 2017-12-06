open Pred;

open Result.Monad;

include Lexer_types;

type t = {
  buffer: string,
  mutable index: int
};

let lexer = (s) => {buffer: s, index: 0};

let print_token = (tok) =>
  switch tok {
  | Token_identifier(id) => Printf.printf("identifier: %s", id)
  | Token_open_paren => print_string("open paren")
  | Token_close_paren => print_string("close paren")
  | Token_open_brace => print_string("open brace")
  | Token_close_brace => print_string("close brace")
  };

let print_error = (err) =>
  switch err {
  | Error_unrecognized_character(ch) =>
    Printf.printf("unrecognized character: `%c` (%d)", ch, Char.code(ch))
  | Error_unclosed_comment => print_string("unclosed comment")
  | Error_end_of_file => print_string("end of file")
  };

/* the actual lexer functions */
let is_whitespace = (ch) => ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r';

let is_alpha = (ch) => ch >= 'A' && ch <= 'Z' || ch >= 'a' && ch <= 'z';

let is_ident_start = (ch) => is_alpha(ch) || ch == '_';

let is_ident_continue = (ch) => is_ident_start(ch) || ch == '\'';

let make_lexer = (buff) => {buffer: buff, index: 0};

let rec next_token: t => Result.t(token, error) =
  (lex) => {
    let peek_ch = () =>
      if (String.length(lex.buffer) <= lex.index) {
        None;
      } else {
        Some(lex.buffer.[lex.index]);
      };
    let next_ch = () =>
      switch (peek_ch()) {
      | Some(ch) =>
        lex.index = lex.index + 1;
        Some(ch);
      | None => None
      };
    let eat_ch = () => next_ch() |> ignore;
    let rec eat_whitespace = () =>
      switch (peek_ch()) {
      | Some(ch) when is_whitespace(ch) =>
        eat_ch();
        eat_whitespace();
      | Some(_)
      | None => ()
      };
    let lex_ident = (fst) => {
      open String_buffer;
      let buff = with_capacity(16);
      let rec helper = (idx) =>
        switch (peek_ch()) {
        | Some(ch) when is_ident_continue(ch) =>
          eat_ch();
          push(buff, ch);
          helper(idx + 1);
        | Some(_)
        | None => to_string(buff)
        };
      push(buff, fst);
      helper(1);
    };
    let rec block_comment = () => {
      let rec eat_the_things = () =>
        switch (next_ch()) {
        | Some('*') =>
          switch (next_ch()) {
          | Some('/') => Result.Ok()
          | _ => eat_the_things()
          }
        | Some('/') =>
          switch (next_ch()) {
          | Some('*') => block_comment()
          | _ => eat_the_things()
          }
        | Some(_) => eat_the_things()
        | None => Err(Error_unclosed_comment)
        };
      eat_the_things();
    };
    let line_comment = () => {
      let rec eat_the_things = () =>
        switch (next_ch()) {
        | Some('\n')
        | None => ()
        | Some(_) => eat_the_things()
        };
      eat_the_things();
    };
    eat_whitespace();
    switch (next_ch()) {
    | Some('/') =>
      switch (peek_ch()) {
      | Some('*') =>
        eat_ch();
        block_comment() >>= (() => next_token(lex));
      | Some('/') =>
        eat_ch();
        line_comment();
        next_token(lex);
      | _ => Err(Error_unrecognized_character('/'))
      }
    | Some('(') => Ok(Token_open_paren)
    | Some(')') => Ok(Token_close_paren)
    | Some('{') => Ok(Token_open_brace)
    | Some('}') => Ok(Token_close_brace)
    | Some(ch) when is_ident_start(ch) => Ok(Token_identifier(lex_ident(ch)))
    | Some(ch) => Err(Error_unrecognized_character(ch))
    | None => Err(Error_end_of_file)
    };
  };

let iter = (lex) => Iter.from_next(() => next_token(lex));
