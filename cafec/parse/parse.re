open Spanned.Prelude;

open Spanned.Result_monad;

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

let wrap_sok: spanned_result('o, 'e) => spanned_result(spanned('o), 'e) =
  fun
  | SOk(o, sp) => SOk((o, sp), sp)
  | SErr(e, sp) => SErr(e, sp);
let wrap_opt_sok: spanned_result(option('o), 'e) => spanned_result(option(spanned('o)), 'e) =
  fun
  | SOk(Some(o), sp) => SOk(Some((o, sp)), sp)
  | SOk(None, sp) => SOk(None, sp)
  | SErr(e, sp) => SErr(e, sp);

type item =
  | Item_func(Ast.Function.builder);

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

let rec maybe_parse_expression
  : 'a => spanned_result(option(Ast.Expr.t), Error.t)
  = (parser) => {
  switch (peek_token(parser)) {
  | SOk(Token.Keyword(Token.Keyword_true), sp) =>
    eat_token(parser);
    SOk(Some((Ast.Expr.Bool_literal(true), sp)), sp)
  | SOk(Token.Keyword(Token.Keyword_false), sp) =>
    eat_token(parser);
    SOk(Some((Ast.Expr.Bool_literal(false), sp)), sp);
  | SOk(Token.Integer_literal(n), sp) =>
    eat_token(parser);
    SOk(Some((Ast.Expr.Integer_literal(n), sp)), sp)
  | SOk(Token.Open_paren, sp) =>
    eat_token(parser);
    {
      with_span(sp)
      >>= () => get_specific(parser, Token.Close_paren)
      >>= () => pure(Some(Ast.Expr.Unit_literal))
    } |> wrap_opt_sok
  | SOk(Token.Keyword(Token.Keyword_if), sp) =>
    eat_token(parser);
    {
      with_span(sp)
      >>= () => get_specific(parser, Token.Open_paren)
      >>= () => parse_expression(parser)
      >>= (cond) => get_specific(parser, Token.Close_paren)
      >>= () => parse_block(parser)
      >>= (thn) => get_specific(parser, Token.Keyword(Token.Keyword_else))
      >>= () => parse_block(parser)
      >>= (els) => pure(Some(Ast.Expr.If_else(cond, thn, els)))
    } |> wrap_opt_sok
  | SOk(Token.Identifier(s), sp) =>
    eat_token(parser);
    SOk(Some((Ast.Expr.Variable(s), sp)), sp)
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
and parse_follow
  : ('a, Ast.Expr.t) => spanned_result(Ast.Expr.t, Error.t)
  = (parser, initial) => {
  let initial = switch (peek_token(parser)) {
  | SOk(Token.Open_paren, sp) =>
    with_span(sp)
    >>= () => parse_argument_list(parser)
    >>= (args) => pure((Ast.Expr.Call(initial, args), true))
  | SOk(tok, _) when is_expression_end(tok) =>
    let (i, sp) = initial;
    SOk((i, false), sp)
  | SOk(tok, sp) => SErr(Error.Unexpected_token(Error.Expected_expression_follow, tok), sp)
  | SErr(e, sp) => SErr(e, sp)
  };
  switch initial {
  | SOk((initial, true), sp) => parse_follow(parser, (initial, sp))
  | SOk((initial, false), sp) => SOk((initial, sp), sp)
  | SErr(e, sp) => SErr(e, sp)
  }
}
and parse_type = (parser) => {
  {
    get_ident(parser)
    >>= (name) => pure(Ast.Type.named(name))
  } |> wrap_sok
}
and parse_return_type = (parser) => {
  maybe_get_specific(parser, Token.Arrow)
  >>= (opt) =>
    switch opt {
    | Some() =>
      parse_type(parser)
      >>= (ty) => pure(Some(ty))
    | None => pure(None)
    }
}
and parse_parameter_list = (parser) => {
  /* TODO(ubsan): maybe abstract out list parsing? */
  let rec get_parms = (comma) =>
    switch (peek_token(parser)) {
    | SOk(Token.Close_paren, _) => pure([])
    | SOk(Token.Comma, sp) =>
      if (comma) {
        eat_token(parser);
        get_parms(false)
      } else {
        SErr(Error.Unexpected_token(Error.Expected_expression, Token.Comma), sp)
      }
    | SOk(tok, sp) =>
      if (comma) {
        SErr(Error.Unexpected_token(Error.Expected_specific(Token.Comma), tok), sp)
      } else {
        get_ident(parser)
        >>= (name) => get_specific(parser, Token.Colon)
        >>= () => parse_type(parser)
        >>= (ty) => {
          get_parms(true)
          >>= (tl) => pure([(name, ty), ...tl])
        }
      }
    | SErr(e, sp) => SErr(e, sp)
    };
  get_specific(parser, Token.Open_paren)
  >>= () => get_parms(false)
  >>= (parms) => get_specific(parser, Token.Close_paren)
  >>= () => pure(parms)
}
and parse_argument_list = (parser) => {
  let rec get_args = (comma) =>
    switch (peek_token(parser)) {
    | SOk(Token.Close_paren, _) => pure([])
    | SOk(Token.Comma, sp) =>
      if (comma) {
        eat_token(parser);
        get_args(false)
      } else {
        SErr(Error.Unexpected_token(Error.Expected_expression, Token.Comma), sp)
      }
    | SOk(tok, sp) =>
      if (comma) {
        SErr(
          Error.Unexpected_token(Error.Expected_specific(Token.Comma), tok),
          sp)
      } else {
        parse_expression(parser)
        >>= (expr) => {
          get_args(true)
          >>= (tl) => pure([expr, ...tl])
        }
      }
    | SErr(e, sp) => SErr(e, sp)
    };
  get_specific(parser, Token.Open_paren)
  >>= () => get_args(false)
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
      {
        next_token(parser)
        >>= (tok) =>
          switch tok {
          | Token.Close_brace => pure(Ast.Expr.Unit_literal)
          | tok => pure_err(Error.Unexpected_token(Error.Expected_expression, tok))
          }
      } |> wrap_sok
    }
};

let parse_item: t => spanned_result(option(item), Error.t) =
  (parser) => {
    next_token(parser)
    >>= (tok) =>
      switch tok {
      | Token.Keyword(Token.Keyword_func) =>
        get_ident(parser)
        >>= (name) => parse_parameter_list(parser)
        >>= (parms) => parse_return_type(parser)
        >>= (ret_ty) => parse_block(parser)
        >>= (expr) => get_specific(parser, Token.Semicolon)
        >>= () => pure(Some(Item_func(
          Ast.Function.make(name, parms, ret_ty, expr))));
      | Token.Eof => pure(None)
      | tok =>
        pure_err(Error.Unexpected_token(Error.Expected_item_declarator, tok))
      }
  };

let parse_program: t => spanned_result(Ast.t, Error.t) =
  (parser) => {
    let rec helper = (parser) =>
      switch (parse_item(parser)) {
      | SOk(Some(Item_func(func)), sp) =>
        helper(parser)
        >>= (tl) => SOk([(func, sp), ...tl], sp)
      | SOk(None, sp) => SOk([], sp)
      | SErr(e, sp) => SErr(e, sp)
      };
    helper(parser)
    >>= (funcs) => pure(Ast.make(funcs))
  };

let parse = (program) => {
  let lexer = Lex.lexer(program);
  let parser = {lexer, peek: None};
  parse_program(parser);
};
