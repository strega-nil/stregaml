open Pred;

open Spanned.Prelude;
open Spanned.Result_monad;

module Error = Error;

exception Bug_lexer(string);

type t = {
  buffer: string,
  mutable index: int,
  mutable line: int,
  mutable column: int
};

let lexer = (s) => {buffer: s, index: 0, line: 1, column: 1};

/* the actual lexer functions */
let is_whitespace = (ch) => ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r';

let is_alpha = (ch) => ch >= 'A' && ch <= 'Z' || ch >= 'a' && ch <= 'z';

let is_number_start = (ch) => ch >= '0' && ch <= '9';

let is_number_continue = (ch, base) =>
  switch base {
  | 2 => ch == '0' || ch == '1'
  | 8 => ch >= '0' && ch <= '7'
  | 10 => ch >= '0' && ch <= '9'
  | 16 => ch >= '0' && ch <= '9' || ch >= 'a' && ch <= 'f' || ch >= 'A' && ch <= 'F'
  | _ => raise(Bug_lexer("Invalid base: " ++ string_of_int(base)))
  };

let is_ident_start = (ch) => is_alpha(ch) || ch == '_';

let is_ident_continue_no_prime = (ch) => is_ident_start(ch) || is_number_start(ch);

let is_ident_continue = (ch) => is_ident_continue_no_prime(ch) || ch == '\'';

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

let rec next_token = (lex) => {
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
        let kw = (k) => SOk(Token.Keyword(k), sp);
        switch (to_string(buff)) {
        | "true" => kw(Token.Keyword_true)
        | "false" => kw(Token.Keyword_false)
        | "if" => kw(Token.Keyword_if)
        | "else" => kw(Token.Keyword_else)
        | "func" => kw(Token.Keyword_func)
        | "_" => kw(Token.Keyword_underscore)
        | "let" as res => SErr(Error.Reserved_token(res), sp)
        | "type" as res => SErr(Error.Reserved_token(res), sp)
        | "struct" as res => SErr(Error.Reserved_token(res), sp)
        | "variant" as res => SErr(Error.Reserved_token(res), sp)
        | id => SOk(Token.Identifier(id), sp)
        };
      };
    push(buff, fst);
    helper(1, sp);
  };
  let lex_operator = (fst, sp) => {
    let rec block_comment: span => spanned_result(unit, Error.t) =
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
          | None => SErr(Error.Unclosed_comment, Spanned.union(sp, current_span()))
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
    switch (peek_ch()) {
    | Some(('*', _)) when fst == '/' =>
      eat_ch();
      switch (block_comment(sp)) {
      | SOk((), _) => next_token(lex)
      | SErr(e, sp) => SErr(e, sp)
      };
    | Some(('/', _)) when fst == '/' =>
      eat_ch();
      line_comment();
      next_token(lex);
    | _ =>
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
          | ":" => SOk(Token.Colon, sp)
          | "=" => SOk(Token.Equals, sp)
          | "->" as res => SErr(Error.Reserved_token(res), sp)
          | "|" as res => SErr(Error.Reserved_token(res), sp)
          | "." as res => SErr(Error.Reserved_token(res), sp)
          | op => SOk(Token.Operator(op), sp)
          }
        };
      push(buff, fst);
      helper(1, sp);
    };
  };
  let lex_number = (fst, sp) => {
    open String_buffer;
    let buff = with_capacity(22);
    let (base, idx, sp) =
      if (fst == '0') {
        switch (peek_ch()) {
        | Some(('x', sp')) =>
          eat_ch();
          (16, 0, Spanned.union(sp, sp'));
        | Some(('o', sp')) =>
          eat_ch();
          (8, 0, Spanned.union(sp, sp'));
        | Some(('b', sp')) =>
          eat_ch();
          (2, 0, Spanned.union(sp, sp'));
        | Some(_)
        | None =>
          push(buff, '0');
          (10, 1, sp);
        };
      } else {
        push(buff, fst);
        (10, 1, sp);
      };
    let rec helper = (idx, sp, space_allowed) =>
      switch (peek_ch()) {
      | Some((ch, sp')) when is_number_continue(ch, base) =>
        eat_ch();
        push(buff, ch);
        helper(idx + 1, Spanned.union(sp, sp'), true);
      | Some((' ', sp')) =>
        eat_ch();
        push(buff, ' ');
        helper(idx, Spanned.union(sp, sp'), false);
      | Some((ch, sp')) when space_allowed && (is_number_continue(ch, 10) || is_ident_continue(ch)) =>
        eat_ch();
        push(buff, ch);
        SErr(Error.Malformed_number_literal(to_string(buff)), Spanned.union(sp, sp'));
      | Some(_)
      | None =>
        let char_to_int = (ch) =>
          if (ch >= '0' && ch <= '9') {
            Char.code(ch) - Char.code('0');
          } else if (ch >= 'a' && ch <= 'f') {
            Char.code(ch) - Char.code('a') + 10;
          } else if (ch >= 'A' && ch <= 'F') {
            Char.code(ch) - Char.code('A') + 10;
          } else {
            assert false;
          };
        /* TODO(ubsan): fix overflow */
        let rec to_int = (idx, acc) =>
          if (idx < length(buff)) {
            let ch = get(buff, idx);
            if (ch == ' ') {
              to_int(idx + 1, acc);
            } else {
              to_int(idx + 1, acc * base + char_to_int(ch));
            };
          } else {
            acc;
          };
        SOk(Token.Integer_literal(to_int(0, 0)), sp);
      };
    helper(idx, sp, true);
  };
  eat_whitespace();
  switch (next_ch()) {
  | Some(('(', sp)) => SOk(Token.Open_paren, sp)
  | Some((')', sp)) => SOk(Token.Close_paren, sp)
  | Some(('{', sp)) => SOk(Token.Open_brace, sp)
  | Some(('}', sp)) => SOk(Token.Close_brace, sp)
  | Some(('[', sp)) => SErr(Error.Reserved_token("["), sp)
  | Some((']', sp)) => SErr(Error.Reserved_token("]"), sp)
  | Some((';', sp)) => SOk(Token.Semicolon, sp)
  | Some((',', sp)) => SOk(Token.Comma, sp)
  | Some(('\'', sp)) => SErr(Error.Reserved_token("'"), sp)
  | Some((ch, sp)) when is_ident_start(ch) => lex_ident(ch, sp)
  | Some((ch, sp)) when is_operator_start(ch) => lex_operator(ch, sp)
  | Some((ch, sp)) when is_number_start(ch) => lex_number(ch, sp)
  | Some((ch, sp)) => SErr(Error.Unrecognized_character(ch), sp)
  | None => SOk(Token.Eof, current_span())
  };
};
