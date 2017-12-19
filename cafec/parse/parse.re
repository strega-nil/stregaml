open Pred;

open Spanned.Prelude;

open Spanned.Monad;

module Error = Error;

module Ast = Untyped_ast;

type t = {
  lexer: Lex.t,
  mutable peek: option((Token.t, span))
};

let peek_token = (parser) =>
  switch parser.peek {
  | Some((pk, sp)) => SOk(pk, sp)
  | None =>
    switch (Lex.next_token(parser.lexer)) {
    | SOk(ret, sp) =>
      parser.peek = Some((ret, sp));
      SOk(ret, sp);
    | SErr(e, sp) => SErr(e, sp)
    }
  };

let next_token = (parser) => {
  let ret = peek_token(parser);
  parser.peek = None;
  ret;
};

let eat_token = (parser) =>
  switch (next_token(parser)) {
  | SOk(_, _) => ()
  | SErr(_, _) => assert false
  };

let is_expression_end = (tok) =>
  switch tok {
  | Token.Semicolon
  | Token.Comma
  | Token.Close_brace
  | Token.Close_paren => true
  | _ => false
  };

type item =
  | Item_func(string, Ast.Expr.t);

let get_ident = (parser) =>
  switch (next_token(parser)) {
  | SOk(Token.Identifier(id), sp) => SOk(id, sp)
  | SOk(tok, sp) => SErr(Error.Unexpected_token(Error.Expected_identifier, tok), sp)
  | SErr(e, sp) => SErr(e, sp)
  };

let maybe_get_specific = (parser, token) =>
  switch (peek_token(parser)) {
  | SOk(tok, sp) when tok == token =>
    eat_token(parser);
    SOk(Some(), sp);
  | SOk(_, sp) => SOk(None, sp)
  | SErr(e, sp) => SErr(e, sp)
  };

let get_specific = (parser, token) =>
  switch (maybe_get_specific(parser, token)) {
  | SOk(Some (), sp) => SOk((), sp)
  | SOk(None, _) =>
    next_token(parser)
    >>= (
      (tok) => pure_err(Error.Unexpected_token(Error.Expected_specific(token), tok))
    )
  | SErr(e, sp) => SErr(e, sp)
  };

let rec maybe_parse_expression = (parser) => {
  switch (peek_token(parser)) {
  | SOk(Token.Open_paren, sp) =>
    eat_token(parser);
    with_span(sp)
    >>= () => get_specific(parser, Token.Close_paren)
    >>= () => pure(Some(Ast.Expr.unit_literal()));
  | SOk(Token.Identifier(s), sp) =>
    eat_token(parser);
    with_span(sp)
    >>= () => pure(Some(Ast.Expr.variable(s)))
  | SOk(_, sp) => SOk(None, sp)
  | SErr(e, sp) => SErr(e, sp)
  }
  >>= (initial) =>
    switch initial {
    | Some(i) => parse_follow(parser, i) >>= (x) => pure(Some(x))
    | None => pure(None)
    }
}
and parse_expression = (parser) =>
  switch (maybe_parse_expression(parser)) {
  | SOk(Some(expr), sp) => SOk(expr, sp)
  | SOk(None, _) =>
    next_token(parser)
    >>= (tok) => pure_err(Error.Unexpected_token(Error.Expected_expression, tok))
  | SErr(e, sp) => SErr(e, sp)
  }
and parse_follow = (parser, initial) => {
  switch (peek_token(parser)) {
  | SOk(Token.Open_paren, sp) =>
    with_span(sp)
    >>= () => parse_argument_list(parser)
    >>= (_) => pure((Ast.Expr.call(initial), true))
  | SOk(tok, _) when is_expression_end(tok) => pure((initial, false))
  | SOk(tok, sp) => SErr(Error.Unexpected_token(Error.Expected_expression_follow, tok), sp)
  | SErr(e, sp) => SErr(e, sp)
  }
  >>= ((initial, should_continue)) =>
    if (should_continue) {
      parse_follow(parser, initial)
    } else {
      pure(initial)
    }
}
and _parse_parameter_list = (parser) => {
  get_specific(parser, Token.Open_paren)
  >>= () => get_specific(parser, Token.Close_paren)
  >>= () => pure([||])
}
and parse_argument_list = (parser) => {
  let rec get_args = (args, comma) =>
    switch (peek_token(parser)) {
    | SOk(Token.Close_paren, _) => pure(Dynamic_array.to_array(args))
    | SOk(Token.Comma, sp) =>
      if (comma) {
        eat_token(parser);
        get_args(args, false)
      } else {
        SErr(Error.Unexpected_token(Error.Expected_expression, Token.Comma), sp)
      }
    | SOk(tok, sp) =>
      if (comma) {
        SErr(Error.Unexpected_token(Error.Expected_specific(Token.Comma), tok), sp)
      } else {
        parse_expression(parser)
        >>= (expr) => {
          Dynamic_array.push(args, expr);
          get_args(args, true)
        }
      }
    | SErr(e, sp) => SErr(e, sp)
    };
  get_specific(parser, Token.Open_paren)
  >>= () => get_args(Dynamic_array.make(), false)
  >>= (arr) => get_specific(parser, Token.Close_paren)
  >>= () => pure(arr)
}
and parse_block = (parser) => {
  get_specific(parser, Token.Open_brace)
  >>= () => maybe_parse_expression(parser)
  >>= (opt_expr) =>
    switch (opt_expr) {
    | Some(e) =>
      get_specific(parser, Token.Close_brace)
      >>= () => pure(e)
    | None =>
      next_token(parser)
      >>= (tok) =>
        switch tok {
        | Token.Close_brace => pure(Ast.Expr.unit_literal())
        | tok => pure_err(Error.Unexpected_token(Error.Expected_expression, tok))
        }
    }
} and parse_item: t => spanned(option(item), Error.t) =
  (parser) => {
    next_token(parser)
    >>= (tok) =>
      switch tok {
      | Token.Keyword(Token.Keyword_func) =>
        get_ident(parser)
        >>= (name) => get_specific(parser, Token.Open_paren)
        >>= () => get_specific(parser, Token.Close_paren)
        >>= () => parse_block(parser)
        >>= (expr) => get_specific(parser, Token.Semicolon)
        >>= () => pure(Some(Item_func(name, expr)));
      | Token.Eof => pure(None)
      | tok =>
        pure_err(Error.Unexpected_token(Error.Expected_item_declarator, tok))
      }
  };

let parse_program: t => spanned(Ast.t, Error.t) =
(parser) => {
let rec helper = (parser, funcs, tys, sp) =>
switch (parse_item(parser)) {
| SOk(Some(Item_func(name, expr)), sp') =>
  let func = Ast.Function.make(name, expr);
  helper(parser, [func, ...funcs], tys, Spanned.union(sp, sp'));
| SOk(None, sp') =>
  SOk(Ast.make(array_of_rev_list(funcs), array_of_rev_list(tys)), Spanned.union(sp, sp'))
| SErr(e, sp) => SErr(e, sp)
};
helper(parser, [], [], Spanned.made_up);
};

let parse = (program) => {
let lexer = Lex.lexer(program);
let parser = {lexer, peek: None};
parse_program(parser);
};
