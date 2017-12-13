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
  | SErr(e, sp) => assert false
  };

type item =
  | Item_func(string, Ast.Expr.t);

let get_ident = (parser) => {
  switch (next_token(parser)) {
  | SOk(Token.Identifier(id), sp) => SOk(id, sp)
  | SOk(tok, sp) => SErr(Error.Unexpected_token(Error.Expected_identifier, tok), sp)
  | SErr(e, sp) => SErr(e, sp)
  }
};

let get_specific = (parser, token) => {
  switch (next_token(parser)) {
  | SOk(tok, sp) when tok == token => SOk((), sp)
  | SOk(tok, sp) => SErr(Error.Unexpected_token(Error.Expected_specific(token), tok), sp)
  | SErr(e, sp) => SErr(e, sp)
  }
};

let parse_item: t => spanned(option(item), Error.t) =
  (parser) =>
    Token.Prelude.(
      next_token(parser)
      >>= (tok) =>
        switch (tok) {
        | Keyword(Keyword_func) =>
          get_ident(parser)
          >>= (name) => get_specific(parser, Open_paren)
          >>= () => get_specific(parser, Close_paren)
          >>= () => get_specific(parser, Open_brace)
          >>= () => get_specific(parser, Close_brace)
          >>= () => pure(Some(Item_func(name, Ast.Expr.unit_literal())))
        | End_of_file => pure(None)
        | tok => SErr(Error.Unexpected_token(Error.Expected_item_declarator, tok), Spanned.made_up)
        }
      );

let parse_program: t => spanned(Ast.t, Error.t) =
  (parser) => {
    let rec helper = (parser, funcs, tys, sp) =>
      switch (parse_item(parser)) {
      | SOk(Some(Item_func(name, expr)), sp') =>
        let func = Ast.Function.make(name, expr);
        helper(parser, [func, ...funcs], tys, Spanned.union(sp, sp'))
      | SOk(None, sp') =>
        SOk(Ast.make(funcs, tys), Spanned.union(sp, sp'))
      | SErr(e, sp) => SErr(e, sp)
      };
    helper(parser, [], [], Spanned.made_up);
  };

let parse = (program) => {
  let lexer = Lex.lexer(program);
  let parser = {lexer, peek: None};
  parse_program(parser);
};
