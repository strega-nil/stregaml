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
      (tok) => SErr(Error.Unexpected_token(Error.Expected_specific(token), tok), Spanned.made_up)
    )
  | SErr(e, sp) => SErr(e, sp)
  };

let maybe_parse_expression = (parser) =>
  switch (peek_token(parser)) {
  | SOk(Token.Open_paren, sp) =>
    eat_token(parser);
    with_span(sp)
    >>= () => get_specific(parser, Token.Close_paren)
    >>= () => pure(Some(Ast.Expr.unit_literal()));
  | SOk(_, sp) => SOk(None, sp)
  | SErr(e, sp) => SErr(e, sp)
  };

let _parse_expression = (parser) =>
  switch (maybe_parse_expression(parser)) {
  | SOk(Some(expr), sp) => SOk(expr, sp)
  | SOk(None, _) =>
    next_token(parser)
    >>= ((tok) => SErr(Error.Unexpected_token(Error.Expected_expression, tok), Spanned.made_up))
  | SErr(e, sp) => SErr(e, sp)
  };

let parse_item: t => spanned(option(item), Error.t) =
  (parser) => {
    next_token(parser)
    >>= (tok) =>
      switch tok {
      | Token.Keyword(Token.Keyword_func) =>
        get_ident(parser)
        >>= (name) => get_specific(parser, Token.Open_paren)
        >>= () => get_specific(parser, Token.Close_paren)
        >>= () => get_specific(parser, Token.Open_brace)
        >>= () => maybe_parse_expression(parser)
        >>= (opt_expr) => get_specific(parser, Token.Close_brace)
        >>= () => get_specific(parser, Token.Semicolon)
        >>= () => {
          let expr =
            switch opt_expr {
            | Some(expr) => expr
            | None => Ast.Expr.unit_literal()
            };
          pure(Some(Item_func(name, expr)));
        }
      | Token.Eof => pure(None)
      | tok =>
        SErr(Error.Unexpected_token(Error.Expected_item_declarator, tok), Spanned.made_up)
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
