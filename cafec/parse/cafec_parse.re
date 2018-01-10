open Pred;

module Spanned = Cafec_spanned;
open Spanned.Prelude;

open Error.Monad_spanned;

module Error = Error;

module Ast = Ast;

type t = {
  lexer: Lex.t,
  mutable peek: option((Token.t, span))
};

let peek_token = (parser) =>
  switch parser.peek {
  | Some((pk, sp)) => Ok((pk, sp))
  | None =>
    switch (Lex.next_token(parser.lexer)) {
    | Ok(ret) =>
      parser.peek = Some(ret);
      Ok(ret);
    | Error(e) => Error(e)
    }
  };

let next_token = (parser) => {
  let ret = peek_token(parser);
  parser.peek = None;
  ret;
};

let eat_token = (parser) =>
  switch (next_token(parser)) {
  | Ok(_) => ()
  | Error(_) => assert false
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
  | Ok((o, sp)) => Ok(((o, sp), sp))
  | Error((e, sp)) => Error((e, sp));
let wrap_opt_sok: spanned_result(option('o), 'e) => spanned_result(option(spanned('o)), 'e) =
  fun
  | Ok((Some(o), sp)) => Ok((Some((o, sp)), sp))
  | Ok((None, sp)) => Ok((None, sp))
  | Error(e) => Error(e);

type item =
  | Item_func(Ast.Function.builder);

let get_ident = (parser) =>
  switch%bind (next_token(parser)) {
  | Token.Identifier(id) => pure(id)
  | tok => pure_err(Error.Unexpected_token(Error.Expected_identifier, tok))
  };

let maybe_get_specific = (parser, token) => {
  let%bind tok = peek_token(parser);
  if (tok == token) {
    eat_token(parser);
    pure(Some())
  } else {
    pure(None)
  };
};

let get_specific = (parser, token) =>
  switch%bind (maybe_get_specific(parser, token)) {
  | Some() => pure(())
  | None =>
    let%bind tok = next_token(parser);
    pure_err(Error.Unexpected_token(Error.Expected_specific(token), tok))
  };

let rec maybe_parse_expression = (parser) => {
  let initial =
    switch%bind (peek_token(parser)) {
    | Token.Keyword(Token.Keyword_true) =>
      eat_token(parser);
      pure(Some(Ast.Expr.Bool_literal(true)))
    | Token.Keyword(Token.Keyword_false) =>
      eat_token(parser);
      pure(Some(Ast.Expr.Bool_literal(false)))
    | Token.Integer_literal(n) =>
      eat_token(parser);
      pure(Some(Ast.Expr.Integer_literal(n)))
    | Token.Open_paren =>
      eat_token(parser);
      let%bind () = get_specific(parser, Token.Close_paren);
      pure(Some(Ast.Expr.Unit_literal))
    | Token.Keyword(Token.Keyword_if) =>
      eat_token(parser);
      let%bind () = get_specific(parser, Token.Open_paren);
      let%bind cond = parse_expression(parser);
      let%bind () = get_specific(parser, Token.Close_paren);
      let%bind thn = parse_block(parser);
      let%bind () = get_specific(parser, Token.Keyword(Token.Keyword_else));
      let%bind els = parse_block(parser);
      pure(Some(Ast.Expr.If_else(cond, thn, els)))
    | Token.Identifier(s) =>
      eat_token(parser);
      pure(Some(Ast.Expr.Variable(s)))
    | _ => pure(None)
    };
  switch%bind (wrap_opt_sok(initial)) {
  | Some(i) =>
    let%bind x = parse_follow(parser, i);
    pure(Some(x))
  | None => pure(None)
  }
}
and parse_expression = (parser) =>
  switch%bind (maybe_parse_expression(parser)) {
  | Some(expr) => pure(expr)
  | None =>
    let%bind tok = next_token(parser);
    pure_err(Error.Unexpected_token(Error.Expected_expression, tok))
  }
and parse_follow = (parser, (initial, sp)) => {
  /* this function deals with spans weirdly */
  let module M = Result.Monad({type t = spanned(Error.t)});
  open! M;
  let%bind (tok, tok_sp) = peek_token(parser);
  let%bind (initial, cont, sp) = switch tok {
  | Token.Open_paren =>
    let%bind (args, sp') = parse_argument_list(parser);
    pure((Ast.Expr.Call((initial, sp), args), true, Spanned.union(sp, sp')))
  | tok when is_expression_end(tok) =>
    pure((initial, false, sp))
  | tok =>
    pure_err(
      (Error.Unexpected_token(Error.Expected_expression_follow, tok), tok_sp))
  };
  if (cont) {
    parse_follow(parser, (initial, sp))
  } else {
    pure(((initial, sp), sp))
  }
}
and parse_type = (parser) => {
  {
    let%bind name = get_ident(parser);
    pure(Ast.Type.named(name))
  } |> wrap_sok
}
and parse_return_type = (parser) => {
  switch%bind (maybe_get_specific(parser, Token.Arrow)) {
  | Some() =>
    let%bind ty = parse_type(parser);
    pure(Some(ty))
  | None => pure(None)
  }
}
and parse_parameter_list = (parser) => {
  /* TODO(ubsan): maybe abstract out list parsing? */
  let rec get_parms = (comma) =>
    switch%bind (peek_token(parser)) {
    | Token.Close_paren => pure([])
    | Token.Comma =>
      if (comma) {
        eat_token(parser);
        get_parms(false)
      } else {
        pure_err(Error.Unexpected_token(Error.Expected_expression, Token.Comma))
      }
    | tok =>
      if (comma) {
        pure_err(
          Error.Unexpected_token(Error.Expected_specific(Token.Comma), tok))
      } else {
        let%bind name = get_ident(parser);
        let%bind () = get_specific(parser, Token.Colon);
        let%bind ty = parse_type(parser);
        let%bind tl = get_parms(true);
        pure([(name, ty), ...tl])
      }
    };
  let%bind () = get_specific(parser, Token.Open_paren);
  let%bind parms = get_parms(false);
  let%bind () = get_specific(parser, Token.Close_paren);
  pure(parms)
}
and parse_argument_list = (parser) => {
  let rec get_args = (comma) =>
    switch%bind (peek_token(parser)) {
    | Token.Close_paren => pure([])
    | Token.Comma =>
      if (comma) {
        eat_token(parser);
        get_args(false)
      } else {
        pure_err(Error.Unexpected_token(Error.Expected_expression, Token.Comma))
      }
    | tok =>
      if (comma) {
        pure_err(
          Error.Unexpected_token(Error.Expected_specific(Token.Comma), tok))
      } else {
        let%bind expr = parse_expression(parser);
        let%bind tl = get_args(true);
        pure([expr, ...tl])
      }
    };
  let%bind () = get_specific(parser, Token.Open_paren);
  let%bind args = get_args(false);
  let%bind () = get_specific(parser, Token.Close_paren);
  pure(args)
}
and parse_block = (parser) => {
  let%bind () = get_specific(parser, Token.Open_brace);
  switch%bind (maybe_parse_expression(parser)) {
  | Some(e) =>
    let%bind () = get_specific(parser, Token.Close_brace);
    pure(e)
  | None =>
    switch%bind (next_token(parser)) {
    | Token.Close_brace => pure(Ast.Expr.Unit_literal)
    | tok => pure_err(Error.Unexpected_token(Error.Expected_expression, tok))
    } |> wrap_sok
  }
};

let parse_item: t => spanned_result(option(item), Error.t) = (parser) =>
  switch%bind (next_token(parser)) {
  | Token.Keyword(Token.Keyword_func) =>
    let%bind name = get_ident(parser);
    let%bind parms = parse_parameter_list(parser);
    let%bind ret_ty = parse_return_type(parser);
    let%bind expr = parse_block(parser);
    let%bind () = get_specific(parser, Token.Semicolon);
    pure(Some(Item_func(Ast.Function.make(name, parms, ret_ty, expr))));
  | Token.Eof => pure(None)
  | tok =>
    pure_err(Error.Unexpected_token(Error.Expected_item_declarator, tok))
  };

let parse_program: t => spanned_result(Ast.t, Error.t) = (parser) => {
  let rec helper = (parser) => {
    let%bind (item, sp) = parse_item(parser) |> wrap_sok;
    switch item {
    | Some(Item_func(func)) =>
      let%bind tl = helper(parser);
      pure([(func, sp), ...tl])
    | None => pure([])
    };
  };
  let%bind funcs = helper(parser);
  pure(Ast.make(funcs))
};

let parse = (program) => {
  let lexer = Lex.lexer(program);
  let parser = {lexer, peek: None};
  parse_program(parser);
};
