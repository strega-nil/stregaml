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


type item = Item_func of Ast.Item.func | Item_type of Ast.Item.type_def

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


let parse_list parser ~f ~sep ~close ~expected =
  let rec helper parser f expected sep close expect_sep =
    let%bind tok, _ = peek_token parser in
    match () with
    | () when tok = close -> wrap []
    | () when tok = sep ->
        if expect_sep then (
          eat_token parser ;
          helper parser f expected sep close false )
        else wrap_err (Error.Unexpected_token (expected, sep))
    | () when expect_sep ->
        wrap_err (Error.Unexpected_token (Error.Expected_specific sep, tok))
    | () ->
        let%bind x, _ = f parser in
        let%bind xs, _ = helper parser f expected sep close true in
        wrap (x :: xs)
  in
  helper parser f expected sep close false


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
  let%bind tok, tok_sp = peek_token parser in
  let%bind (initial, cont), sp =
    match (initial, tok) with
    | _, Token.Open_paren ->
        let%bind args, _ = parse_argument_list parser in
        wrap (Ast.Expr.Call ((initial, sp), args), true)
    | _, Token.Dot ->
        eat_token parser ;
        let%bind member, _ = get_ident parser in
        wrap (Ast.Expr.Struct_access ((initial, sp), member), true)
    | Ast.Expr.Variable name, Token.Open_brace ->
        let parse_member_initializer parser =
          let%bind name, name_sp = get_ident parser in
          let%bind (), _ = get_specific parser Token.Equals in
          let%bind expr, expr_sp = parse_expression parser in
          wrap ((name, expr), Spanned.union name_sp expr_sp)
        in
        eat_token parser ;
        let%bind members, _ =
          parse_list parser ~f:parse_member_initializer ~sep:Token.Semicolon
            ~close:Token.Close_brace ~expected:Error.Expected_identifier
        in
        let%bind (), _ = get_specific parser Token.Close_brace in
        let ty = Ast.Type.Named name in
        wrap (Ast.Expr.Struct_literal (ty, members), true)
    | _, tok when is_expression_end tok -> Ok ((initial, false), sp)
    | _, tok ->
        Error
          ( Error.Unexpected_token (Error.Expected_expression_follow, tok)
          , tok_sp )
  in
  if cont then parse_follow parser (initial, sp) else wrap (initial, sp)


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
  let get_parm parser =
    let%bind name, _ = get_ident parser in
    let%bind (), _ = get_specific parser Token.Colon in
    let%bind ty, _ = parse_type parser in
    wrap (name, ty)
  in
  let%bind (), _ = get_specific parser Token.Open_paren in
  let%bind parms, _ =
    parse_list parser ~f:get_parm ~sep:Token.Comma ~close:Token.Close_paren
      ~expected:Error.Expected_identifier_or_under
  in
  let%bind (), _ = get_specific parser Token.Close_paren in
  wrap parms


and parse_argument_list parser =
  let%bind (), _ = get_specific parser Token.Open_paren in
  let%bind args, _ =
    parse_list parser ~f:parse_expression ~sep:Token.Comma
      ~close:Token.Close_paren ~expected:Error.Expected_expression
  in
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


let parse_type_definition parser =
  match%bind peek_token parser with
  | Token.Keyword Token.Keyword_struct, _ ->
      eat_token parser ;
      let parse_member parser =
        let%bind name, _ = get_ident parser in
        let%bind (), _ = get_specific parser Token.Colon in
        let%bind ty, _ = parse_type parser in
        wrap (name, ty)
      in
      let%bind (), _ = get_specific parser Token.Open_brace in
      let%bind members, _ =
        parse_list parser ~f:parse_member ~sep:Token.Semicolon
          ~close:Token.Close_brace ~expected:Error.Expected_identifier
      in
      let%bind (), _ = get_specific parser Token.Close_brace in
      wrap (Ast.Item.Struct members)
  | _ ->
    match parse_type parser with
    | Ok (o, sp) -> Ok (Ast.Item.Alias o, sp)
    | Error (Error.Unexpected_token (_, tok), sp) ->
        Error (Error.Unexpected_token (Error.Expected_type_definition, tok), sp)
    | Error e -> Error e


let parse_item (parser: t) : (item option, Error.t) spanned_result =
  match%bind next_token parser with
  | Token.Keyword Token.Keyword_func, _ ->
      let%bind fname, _ = get_ident parser in
      let%bind params, _ = parse_parameter_list parser in
      let%bind ret_ty, _ = maybe_parse_type_annotation parser in
      let%bind (), _ = get_specific parser Token.Equals in
      let%bind expr, _ = parse_expression parser in
      let%bind (), _ = get_specific parser Token.Semicolon in
      let func = Ast.Item.{fname; params; ret_ty; expr} in
      wrap (Some (Item_func func))
  | Token.Keyword Token.Keyword_type, _ ->
      let%bind tname, _ = get_ident parser in
      let%bind (), _ = get_specific parser Token.Equals in
      let%bind kind, _ = parse_type_definition parser in
      let%bind (), _ = get_specific parser Token.Semicolon in
      let ty = Ast.Item.{tname; kind} in
      wrap (Some (Item_type ty))
  | Token.Eof, _ -> wrap None
  | tok, _ ->
      wrap_err (Error.Unexpected_token (Error.Expected_item_declarator, tok))


let parse_program (parser: t) : (Ast.t, Error.t) spanned_result =
  let rec helper parser =
    let%bind item, sp = parse_item parser in
    match item with
    | Some Item_func func ->
        let%bind (ftl, ttl), _ = helper parser in
        wrap ((func, sp) :: ftl, ttl)
    | Some Item_type ty ->
        let%bind (ftl, ttl), _ = helper parser in
        wrap (ftl, (ty, sp) :: ttl)
    | None -> wrap ([], [])
  in
  let%bind (funcs, types), _ = helper parser in
  wrap Ast.{funcs; types}


let parse program =
  let lexer = Lex.lexer program in
  let parser = {lexer; peek= None} in
  parse_program parser
