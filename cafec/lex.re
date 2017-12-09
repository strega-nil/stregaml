open Pred;

open Spanned.Prelude;

open Spanned.Monad;

include Lexer_types;

exception Bug_lexer(string);

type t = {
  buffer: string,
  mutable index: int,
  mutable line: int,
  mutable column: int
};

let lexer = (s) => {buffer: s, index: 0, line: 1, column: 1};

let print_keyword = (kw) =>
  switch kw {
  | Keyword_true => print_string("true")
  | Keyword_false => print_string("false")
  | Keyword_if => print_string("if")
  | Keyword_else => print_string("else")
  | Keyword_func => print_string("func")
  };

let print_token = (tok) =>
  switch tok {
  | Token_open_paren => print_string("open paren")
  | Token_close_paren => print_string("close paren")
  | Token_open_brace => print_string("open brace")
  | Token_close_brace => print_string("close brace")
  | Token_keyword(kw) =>
    print_string("keyword: ");
    print_keyword(kw);
  | Token_operator(op) =>
    print_string("operator: ");
    print_string(op);
  | Token_identifier(id) =>
    print_string("identifier: ");
    print_string(id);
  | Token_int_literal(i) =>
    print_string("int literal: ");
    print_int(i);
  | Token_colon => print_string("colon")
  | Token_equals => print_string("equals")
  | Token_semicolon => print_string("semicolon")
  | Token_comma => print_string("comma")
  };

let print_spanned_token = (tok, sp) => {
  print_token(tok);
  Printf.printf(
    " from (%d, %d) to (%d, %d)",
    sp.start_line,
    sp.start_column,
    sp.end_line,
    sp.end_column
  );
};

let print_error = (err) =>
  switch err {
  | Error_reserved_token(tok) => Printf.printf("reserved token: %s", tok)
  | Error_unrecognized_character(ch) =>
    Printf.printf("unrecognized character: `%c` (%d)", ch, Char.code(ch))
  | Error_unclosed_comment => print_string("unclosed comment")
  };

let print_spanned_error = (err, sp) => {
  print_error(err);
  Printf.printf(
    " from (%d, %d) to (%d, %d)",
    sp.start_line,
    sp.start_column,
    sp.end_line,
    sp.end_column
  );
};

/* the actual lexer functions */
let is_whitespace = (ch) => ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r';

let is_alpha = (ch) => ch >= 'A' && ch <= 'Z' || ch >= 'a' && ch <= 'z';

let is_ident_start = (ch) => is_alpha(ch) || ch == '_';

let is_ident_continue = (ch) => is_ident_start(ch) || ch == '\'';

let is_operator_start = (ch) =>
  switch ch {
  | '!'
  | '#'
  | '$'
  | '%'
  | '&'
  | '*'
  | '+'
  | '-'
  | '.'
  | '/'
  | ':'
  | '<'
  | '='
  | '>'
  | '?'
  | '@'
  | '\\'
  | '^'
  | '|'
  | '~' => true
  | _ => false
  };

let is_operator_continue = (ch) => is_operator_start(ch);

let is_number_continue = (ch, base) =>
  switch base {
  | 2 => ch == '0' || ch == '1'
  | 8 => ch >= '0' && ch <= '7'
  | 10 => ch >= '0' && ch <= '9'
  | 16 => ch >= '0' && ch <= '9' || ch >= 'a' && ch <= 'f' || ch >= 'A' && ch <= 'F'
  | _ => raise(Bug_lexer("Invalid base: " ++ string_of_int(base)))
  };

let is_number_start = (ch) => ch >= '0' && ch <= '9';

let rec next_token: t => option(spanned(token, error)) =
  (lex) => {
    let current_span = () => {
      start_line: lex.line,
      start_column: lex.column,
      end_line: lex.line,
      end_column: lex.column + 1
    };
    let peek_ch = () =>
      if (String.length(lex.buffer) <= lex.index) {
        None;
      } else {
        Some((lex.buffer.[lex.index], current_span()));
      };
    let next_ch = () =>
      switch (peek_ch()) {
      | Some((ch, sp)) =>
        lex.index = lex.index + 1;
        if (ch == '\n') {
          lex.line = lex.line + 1;
          lex.column = 1;
        } else {
          lex.column = lex.column + 1;
        };
        Some((ch, sp));
      | None => None
      };
    let eat_ch = () => next_ch() |> ignore;
    let rec eat_whitespace = () =>
      switch (peek_ch()) {
      | Some((ch, _)) when is_whitespace(ch) =>
        eat_ch();
        eat_whitespace();
      | Some(_)
      | None => ()
      };
    let lex_ident = (fst, sp) => {
      open String_buffer;
      let buff = with_capacity(16);
      let rec helper = (idx, sp) =>
        switch (peek_ch()) {
        | Some((ch, sp')) when is_ident_continue(ch) =>
          eat_ch();
          push(buff, ch);
          helper(idx + 1, Spanned.union(sp, sp'));
        | Some(_)
        | None =>
          {
            let kw = (k) => SOk(Token_keyword(k), sp);
            switch (to_string(buff)) {
            | "true" => kw(Keyword_true)
            | "false" => kw(Keyword_false)
            | "if" => kw(Keyword_if)
            | "else" => kw(Keyword_else)
            | "func" => kw(Keyword_func)
            | "let" as res => SErr(Error_reserved_token(res), sp)
            | "type" as res => SErr(Error_reserved_token(res), sp)
            | "struct" as res => SErr(Error_reserved_token(res), sp)
            | "variant" as res => SErr(Error_reserved_token(res), sp)
            | id => SOk(Token_identifier(id), sp)
            }
          }
        };
      push(buff, fst);
      helper(1, sp);
    };
    let lex_operator = (fst, sp) => {
      open String_buffer;
      let buff = with_capacity(8);
      let rec helper = (idx, sp) =>
        switch (peek_ch()) {
        | Some((ch, sp')) when is_operator_continue(ch) =>
          eat_ch();
          push(buff, ch);
          helper(idx + 1, Spanned.union(sp, sp'));
        | Some(_)
        | None =>
          switch (to_string(buff)) {
          | "|" as res => SErr(Error_reserved_token(res), sp)
          | "." as res => SErr(Error_reserved_token(res), sp)
          | op => SOk(Token_operator(op), sp)
          }
        };
      push(buff, fst);
      helper(1, sp);
    };
    let lex_number = (fst, sp) => unimplemented();
    let rec block_comment: span => spanned(unit, error) =
      (sp) => {
        let rec eat_the_things = () =>
          switch (next_ch()) {
          | Some(('*', _)) =>
            switch (next_ch()) {
            | Some(('/', _)) => pure()
            | _ => eat_the_things()
            }
          | Some(('/', sp')) =>
            switch (next_ch()) {
            | Some(('*', _)) => block_comment(sp')
            | _ => eat_the_things()
            }
          | Some(_) => eat_the_things()
          | None => SErr(Error_unclosed_comment, Spanned.union(sp, current_span()))
          };
        eat_the_things();
      };
    let line_comment = () => {
      let rec eat_the_things = () =>
        switch (next_ch()) {
        | Some(('\n', _))
        | None => ()
        | Some(_) => eat_the_things()
        };
      eat_the_things();
    };
    eat_whitespace();
    switch (next_ch()) {
    | Some(('/', sp)) =>
      switch (peek_ch()) {
      | Some(('*', _)) =>
        eat_ch();
        switch (block_comment(sp)) {
        | SOk((), _) => next_token(lex)
        | SErr(e, sp) => Some(SErr(e, sp))
        };
      | Some(('/', _)) =>
        eat_ch();
        line_comment();
        next_token(lex);
      | _ => Some(SErr(Error_unrecognized_character('/'), sp))
      }
    | Some(('(', sp)) => Some(SOk(Token_open_paren, sp))
    | Some((')', sp)) => Some(SOk(Token_close_paren, sp))
    | Some(('{', sp)) => Some(SOk(Token_open_brace, sp))
    | Some(('}', sp)) => Some(SOk(Token_close_brace, sp))
    | Some(('[', sp)) => Some(SErr(Error_reserved_token("["), sp))
    | Some((']', sp)) => Some(SErr(Error_reserved_token("]"), sp))
    | Some((';', sp)) => Some(SOk(Token_semicolon, sp))
    | Some((',', sp)) => Some(SOk(Token_comma, sp))
    | Some((ch, sp)) when is_ident_start(ch) => Some(lex_ident(ch, sp) >>= ((id) => pure(id)))
    | Some((ch, sp)) when is_operator_start(ch) =>
      Some(lex_operator(ch, sp) >>= ((op) => pure(op)))
    | Some((ch, sp)) when is_ident_start(ch) => Some(lex_number(ch, sp) >>= ((n) => pure(n)))
    | Some((ch, sp)) => Some(SErr(Error_unrecognized_character(ch), sp))
    | None => None
    };
  };

let iter = (lex) => Iter.from_next(() => next_token(lex));
