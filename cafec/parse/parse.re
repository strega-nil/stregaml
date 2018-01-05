open Pred;
open Spanned.Prelude;

open Error.Monad_spanned;

module Error = Error;

module Ast = Untyped_ast;

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
    | Err(e) => Err(e)
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
  | Err(_) => assert false
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
  | Err((e, sp)) => Err((e, sp));
let wrap_opt_sok: spanned_result(option('o), 'e) => spanned_result(option(spanned('o)), 'e) =
  fun
  | Ok((Some(o), sp)) => Ok((Some((o, sp)), sp))
  | Ok((None, sp)) => Ok((None, sp))
  | Err(e) => Err(e);

type item =
  | Item_func(Ast.Function.builder);

let get_ident = (parser) =>
  next_token(parser)
  >>= (tok) => switch tok {
  | Token.Identifier(id) => pure(id)
  | tok => pure_err(Error.Unexpected_token(Error.Expected_identifier, tok))
  };

let maybe_get_specific = (parser, token) =>
  peek_token(parser)
  >>= (tok) =>
    if (tok == token) {
      pure(Some())
    } else {
      pure(None)
    };

let get_specific = (parser, token) =>
  maybe_get_specific(parser, token)
  >>= (opt) => switch opt {
  | Some() => pure(())
  | None =>
    next_token(parser)
    >>= (tok) =>
      pure_err(Error.Unexpected_token(Error.Expected_specific(token), tok))
  };

let rec maybe_parse_expression
  : 'a => spanned_result(option(Ast.Expr.t), Error.t)
  = (parser) => {
  peek_token(parser)
  >>= (tok) => switch tok {
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
    get_specific(parser, Token.Close_paren)
    >>= () => pure(Some(Ast.Expr.Unit_literal))
  | Token.Keyword(Token.Keyword_if) =>
    eat_token(parser);
    get_specific(parser, Token.Open_paren)
    >>= () => parse_expression(parser)
    >>= (cond) => get_specific(parser, Token.Close_paren)
    >>= () => parse_block(parser)
    >>= (thn) => get_specific(parser, Token.Keyword(Token.Keyword_else))
    >>= () => parse_block(parser)
    >>= (els) => pure(Some(Ast.Expr.If_else(cond, thn, els)))
  | Token.Identifier(s) =>
    eat_token(parser);
    pure(Some(Ast.Expr.Variable(s)))
  | _ => pure(None)
  }
  |> wrap_opt_sok
  >>= (initial) => switch initial {
  | Some(i) => parse_follow(parser, i) >>= (x) => pure(Some(x))
  | None => pure(None)
  }
}
and parse_expression = (parser) =>
  maybe_parse_expression(parser)
  >>= (opt) => switch opt {
  | Some(expr) => pure(expr)
  | None =>
    next_token(parser)
    >>= (tok) =>
      pure_err(Error.Unexpected_token(Error.Expected_expression, tok))
  }
and parse_follow
  : ('a, Ast.Expr.t) => spanned_result(Ast.Expr.t, Error.t)
  = (parser, (initial, sp)) => {
  /* this function deals with spans weirdly */
  let module M = Result.Monad({type t = spanned(Error.t)});
  open! M;
  peek_token(parser)
  >>= ((tok, tok_sp)) => switch tok {
  | Token.Open_paren =>
    parse_argument_list(parser)
    >>= ((args, sp')) => pure(
      (Ast.Expr.Call((initial, sp), args), true, Spanned.union(sp, sp')))
  | tok when is_expression_end(tok) =>
    pure((initial, false, sp))
  | tok =>
    pure_err(
      (Error.Unexpected_token(Error.Expected_expression_follow, tok), tok_sp))
  }
  >>= ((initial, cont, sp)) =>
    if (cont) {
      parse_follow(parser, (initial, sp))
    } else {
      pure(((initial, sp), sp))
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
  >>= (opt) => switch opt {
  | Some() =>
    parse_type(parser)
    >>= (ty) => pure(Some(ty))
  | None => pure(None)
  }
}
and parse_parameter_list = (parser) => {
  /* TODO(ubsan): maybe abstract out list parsing? */
  let rec get_parms = (comma) =>
    peek_token(parser)
    >>= (tok) => switch tok {
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
        get_ident(parser)
        >>= (name) => get_specific(parser, Token.Colon)
        >>= () => parse_type(parser)
        >>= (ty) => {
          get_parms(true)
          >>= (tl) => pure([(name, ty), ...tl])
        }
      }
    };
  get_specific(parser, Token.Open_paren)
  >>= () => get_parms(false)
  >>= (parms) => get_specific(parser, Token.Close_paren)
  >>= () => pure(parms)
}
and parse_argument_list = (parser) => {
  let rec get_args = (comma) =>
    peek_token(parser)
    >>= (tok) => switch tok {
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
        parse_expression(parser)
        >>= (expr) => {
          get_args(true)
          >>= (tl) => pure([expr, ...tl])
        }
      }
    };
  get_specific(parser, Token.Open_paren)
  >>= () => get_args(false)
  >>= (arr) => get_specific(parser, Token.Close_paren)
  >>= () => pure(arr)
}
and parse_block = (parser) => {
  get_specific(parser, Token.Open_brace)
  >>= () => maybe_parse_expression(parser)
  >>= (opt_expr) => switch (opt_expr) {
  | Some(e) =>
    get_specific(parser, Token.Close_brace)
    >>= () => pure(e)
  | None =>
    {
      next_token(parser)
      >>= (tok) => switch tok {
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
      parse_item(parser)
      |> wrap_sok
      >>= ((item, sp)) => switch item {
      | Some(Item_func(func)) =>
        helper(parser)
        >>= (tl) => pure([(func, sp), ...tl])
      | None => pure([])
      };
    helper(parser)
    >>= (funcs) => pure(Ast.make(funcs))
  };

let parse = (program) => {
  let lexer = Lex.lexer(program);
  let parser = {lexer, peek: None};
  parse_program(parser);
};
