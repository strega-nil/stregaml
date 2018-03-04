module Spanned = Cafec_spanned
open Spanned.Prelude
open Error.Monad_spanned
module Error = Error
module Ast = Ast

type t = {lexer: Lex.t; mutable peek: (Token.t * span) option}

let peek_token parser =
  match parser.peek with
  | Some pk -> Ok pk
  | None ->
    match Lex.next_token parser.lexer with
    | Ok ret ->
        parser.peek <- Some ret ;
        Ok ret
    | Error e -> Error e


let next_token parser =
  let ret = peek_token parser in
  parser.peek <- None ;
  ret


let eat_token parser =
  match next_token parser with Ok _ -> () | Error _ -> assert false


let is_expression_end = function
  | Token.Semicolon | Token.Comma | Token.Close_brace | Token.Close_paren ->
      true
  | _ -> false


type item = Item_func of Ast.Function.builder

let get_ident parser =
  match%bind next_token parser with
  | Token.Identifier id, _ -> wrap id
  | tok, _ ->
      wrap_err (Error.Unexpected_token (Error.Expected_identifier, tok))


let maybe_get_specific parser token =
  let%bind tok, _ = peek_token parser in
  if tok = token then ( eat_token parser ; wrap (Some ()) ) else wrap None


let get_specific parser token =
  match%bind maybe_get_specific parser token with
  | Some (), _ -> wrap ()
  | None, _ ->
      let%bind tok, _ = next_token parser in
      wrap_err (Error.Unexpected_token (Error.Expected_specific token, tok))


let rec maybe_parse_expression parser =
  let initial =
    match%bind peek_token parser with
    | Token.Keyword Token.Keyword_true, _ ->
        eat_token parser ;
        wrap (Some (Ast.Expr.Bool_literal true))
    | Token.Keyword Token.Keyword_false, _ ->
        eat_token parser ;
        wrap (Some (Ast.Expr.Bool_literal false))
    | Token.Integer_literal n, _ ->
        eat_token parser ;
        wrap (Some (Ast.Expr.Integer_literal n))
    | Token.Open_paren, _ ->
        eat_token parser ;
        let%bind (), _ = get_specific parser Token.Close_paren in
        wrap (Some Ast.Expr.Unit_literal)
    | Token.Keyword Token.Keyword_if, _ ->
        eat_token parser ;
        let%bind (), _ = get_specific parser Token.Open_paren in
        let%bind cond, _ = parse_expression parser in
        let%bind (), _ = get_specific parser Token.Close_paren in
        let%bind thn, _ = parse_block parser in
        let%bind (), _ =
          get_specific parser (Token.Keyword Token.Keyword_else)
        in
        let%bind els, _ = parse_block parser in
        wrap (Some (Ast.Expr.If_else (cond, thn, els)))
    | Token.Identifier s, _ ->
        eat_token parser ;
        wrap (Some (Ast.Expr.Variable s))
    | _ -> wrap None
  in
  match%bind initial with
  | Some i, sp ->
      let%bind x, _ = parse_follow parser (i, sp) in
      wrap (Some x)
  | None, _ -> wrap None


and parse_expression parser =
  match%bind maybe_parse_expression parser with
  | Some expr, _ -> wrap expr
  | None, _ ->
      let%bind tok, _ = next_token parser in
      wrap_err (Error.Unexpected_token (Error.Expected_expression, tok))


and parse_follow parser (initial, sp) =
  (* this function deals with spans weirdly *)
  let module M = Result.Monad (struct
    type t = Error.t spanned
  end) in
  let open! M in
  let%bind tok, tok_sp = peek_token parser in
  let%bind initial, cont, sp =
    match tok with
    | Token.Open_paren ->
        let%bind args, sp' = parse_argument_list parser in
        wrap (Ast.Expr.Call ((initial, sp), args), true, Spanned.union sp sp')
    | tok when is_expression_end tok -> wrap (initial, false, sp)
    | tok ->
        wrap_err
          ( Error.Unexpected_token (Error.Expected_expression_follow, tok)
          , tok_sp )
  in
  if cont then parse_follow parser (initial, sp) else wrap ((initial, sp), sp)


and parse_type parser =
  let%bind name, sp = get_ident parser in
  wrap (Ast.Type.Named name, sp)


and maybe_parse_type_annotation parser =
  match%bind maybe_get_specific parser Token.Colon with
  | Some (), _ ->
      let%bind ty, _ = parse_type parser in
      wrap (Some ty)
  | None, _ -> wrap None


and parse_parameter_list parser =
  (* TODO(ubsan): maybe abstract out list parsing? *)
  let rec get_parms comma =
    match%bind peek_token parser with
    | Token.Close_paren, _ -> wrap []
    | Token.Comma, _ ->
        if comma then ( eat_token parser ; get_parms false )
        else
          wrap_err
            (Error.Unexpected_token (Error.Expected_expression, Token.Comma))
    | tok, _ ->
        if comma then
          wrap_err
            (Error.Unexpected_token (Error.Expected_specific Token.Comma, tok))
        else
          let%bind name, _ = get_ident parser in
          let%bind (), _ = get_specific parser Token.Colon in
          let%bind ty, _ = parse_type parser in
          let%bind tl, _ = get_parms true in
          wrap ((name, ty) :: tl)
  in
  let%bind (), _ = get_specific parser Token.Open_paren in
  let%bind parms, _ = get_parms false in
  let%bind (), _ = get_specific parser Token.Close_paren in
  wrap parms


and parse_argument_list parser =
  let rec get_args comma =
    match%bind peek_token parser with
    | Token.Close_paren, _ -> wrap []
    | Token.Comma, _ ->
        if comma then ( eat_token parser ; get_args false )
        else
          wrap_err
            (Error.Unexpected_token (Error.Expected_expression, Token.Comma))
    | tok, _ ->
        if comma then
          wrap_err
            (Error.Unexpected_token (Error.Expected_specific Token.Comma, tok))
        else
          let%bind expr, _ = parse_expression parser in
          let%bind tl, _ = get_args true in
          wrap (expr :: tl)
  in
  let%bind (), _ = get_specific parser Token.Open_paren in
  let%bind args, _ = get_args false in
  let%bind (), _ = get_specific parser Token.Close_paren in
  wrap args


and parse_block parser =
  let%bind (), _ = get_specific parser Token.Open_brace in
  match%bind maybe_parse_expression parser with
  | Some e, _ ->
      let%bind (), _ = get_specific parser Token.Close_brace in
      wrap e
  | None, sp ->
      match%bind next_token parser with
      | Token.Close_brace, sp' ->
          wrap (Ast.Expr.Unit_literal, Spanned.union sp sp')
      | tok, _ ->
          wrap_err (Error.Unexpected_token (Error.Expected_expression, tok))


let parse_item (parser: t) : (item option, Error.t) spanned_result =
  match%bind next_token parser with
  | Token.Keyword Token.Keyword_func, _ ->
      let%bind name, _ = get_ident parser in
      let%bind params, _ = parse_parameter_list parser in
      let%bind ret_ty, _ = maybe_parse_type_annotation parser in
      let%bind (), _ = get_specific parser Token.Equals in
      let%bind expr, _ = parse_expression parser in
      let%bind (), _ = get_specific parser Token.Semicolon in
      let func = Ast.Function.{name; params; ret_ty; expr} in
      wrap (Some (Item_func func))
  | Token.Eof, _ -> wrap None
  | tok, _ ->
      wrap_err (Error.Unexpected_token (Error.Expected_item_declarator, tok))


let parse_program (parser: t) : (Ast.t, Error.t) spanned_result =
  let rec helper parser =
    let%bind item, sp = parse_item parser in
    match item with
    | Some Item_func func ->
        let%bind tl, _ = helper parser in
        wrap ((func, sp) :: tl)
    | None -> wrap []
  in
  let%bind funcs, _ = helper parser in
  wrap (Ast.make funcs)


let parse program =
  let lexer = Lex.lexer program in
  let parser = {lexer; peek= None} in
  parse_program parser
