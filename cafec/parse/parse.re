open Pred;

open Spanned.Prelude;
open Spanned.Monad;

module Error = Error;

module Ast = Untyped_ast;

type t = {
  lexer: Lex.t,
  mutable peek: option((Token.t, span))
};

let peek_token = (parser) => {
  switch (parser.peek) {
  | Some((pk, sp)) => SOk(pk, sp)
  | None =>
    switch (Lex.next_token(parser.lexer)) {
    | SOk(ret, sp) =>
      parser.peek = Some((ret, sp));
      SOk(ret, sp)
    | SErr(e, sp) => SErr(e, sp)
    }
  }
};

let next_token = (parser) => {
  let ret = peek_token(parser);
  parser.peek = None;
  ret
};

let eat_token = (parser) =>
  switch (next_token(parser)) {
  | SOk(_, _) => ()
  | SErr(e, sp) => assert(false)
  };

type item =
  | Item_func(string, Ast.Expr.t);

let parse_item: t => spanned(option(item), Error.t) = (parser) => {
  unimplemented()
};

let parse_program: t => spanned(Ast.t, Error.t) = (parser) => {
  let rec helper = (parser, items) => {
    switch (parse_item(parser)) {
    | SOk(Some(item), sp) =>
      helper(parser, [(item, sp), ...items])
    | SOk(None, sp) => unimplemented()
    | SErr(e, sp) => SErr(e, sp)
    }
  };
  helper(parser, [])
};

let parse = (program) => {
  let lexer = Lex.lexer(program);
  let parser = {lexer, peek: None};
  parse_program(parser)
};
