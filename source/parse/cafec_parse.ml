module Error = Error
module Ast = Ast
open Spanned.Result.Monad

type t = {lexer: Lex.t; mutable peek: Token.t Spanned.t option}

let peek_token parser =
  match parser.peek with
  | Some (pk, sp) -> (Ok pk, sp)
  | None ->
    match Lex.next_token parser.lexer with
    | Ok ret, sp ->
        parser.peek <- Some (ret, sp) ;
        (Ok ret, sp)
    | Error e, sp -> (Error e, sp)


let next_token parser =
  let ret = peek_token parser in
  parser.peek <- None ;
  ret


let eat_token parser =
  match next_token parser with Ok _, _ -> () | Error _, _ -> assert false


let is_expression_end = function
  | Token.Semicolon | Token.Comma | Token.Close_brace | Token.Close_paren ->
      true
  | _ -> false


type item = Item_func of Ast.Item.func | Item_type of Ast.Item.type_def

let get_ident parser =
  match%bind next_token parser with
  | Token.Identifier id -> return id
  | tok -> return_err (Error.Unexpected_token (Error.Expected.Identifier, tok))


let maybe_get_specific parser token =
  let%bind tok = peek_token parser in
  if Token.equal tok token then ( eat_token parser ; return (Some ()) )
  else return None


let get_specific parser token =
  match%bind maybe_get_specific parser token with
  | Some () -> return ()
  | None ->
      let%bind tok = next_token parser in
      return_err (Error.Unexpected_token (Error.Expected.Specific token, tok))


let parse_list parser ~f ~sep ~close ~expected =
  let rec helper parser f expected sep close expect_sep =
    let%bind tok = peek_token parser in
    match () with
    | () when Token.equal tok close -> return []
    | () when Token.equal tok sep ->
        if expect_sep then (
          eat_token parser ;
          helper parser f expected sep close false )
        else return_err (Error.Unexpected_token (expected, sep))
    | () when expect_sep ->
        return_err (Error.Unexpected_token (Error.Expected.Specific sep, tok))
    | () ->
        let%bind x = f parser in
        let%bind xs = helper parser f expected sep close true in
        return (x :: xs)
  in
  helper parser f expected sep close false


let rec maybe_parse_expression parser =
  let initial =
    match%bind peek_token parser with
    | Token.Keyword Token.Keyword.True ->
        eat_token parser ;
        return (Some (Ast.Expr.Bool_literal true))
    | Token.Keyword Token.Keyword.False ->
        eat_token parser ;
        return (Some (Ast.Expr.Bool_literal false))
    | Token.Integer_literal n ->
        eat_token parser ;
        return (Some (Ast.Expr.Integer_literal n))
    | Token.Open_paren ->
        eat_token parser ;
        let%bind () = get_specific parser Token.Close_paren in
        return (Some Ast.Expr.Unit_literal)
    | Token.Keyword Token.Keyword.If ->
        eat_token parser ;
        let%bind () = get_specific parser Token.Open_paren in
        let%bind cond = parse_expression parser in
        let%bind () = get_specific parser Token.Close_paren in
        let%bind thn = parse_block parser in
        let%bind () = get_specific parser (Token.Keyword Token.Keyword.Else) in
        let%bind els = parse_block parser in
        return (Some (Ast.Expr.If_else (cond, thn, els)))
    | Token.Identifier s ->
        eat_token parser ;
        return (Some (Ast.Expr.Variable s))
    | _ -> return None
  in
  match%bind spanned_bind initial with
  | Some i, sp ->
      let%bind x = parse_follow parser (i, sp) in
      return (Some x)
  | None, _ -> return None


and parse_expression parser =
  match%bind maybe_parse_expression parser with
  | Some expr -> return expr
  | None ->
      let%bind tok = next_token parser in
      return_err (Error.Unexpected_token (Error.Expected.Expression, tok))


and parse_follow parser (initial, sp) =
  let%bind tok, tok_sp = spanned_bind (peek_token parser) in
  let%bind initial, cont =
    match (initial, tok) with
    | _, Token.Open_paren ->
        let%bind args = parse_argument_list parser in
        return (Ast.Expr.Call ((initial, sp), args), true)
    | _, Token.Dot ->
        eat_token parser ;
        let%bind member = get_ident parser in
        return (Ast.Expr.Struct_access ((initial, sp), member), true)
    | Ast.Expr.Variable name, Token.Open_brace ->
        let parse_member_initializer parser =
          let%bind name, name_sp = spanned_bind (get_ident parser) in
          let%bind () = get_specific parser Token.Equals in
          let%bind expr, expr_sp = spanned_bind (parse_expression parser) in
          return ((name, expr), Spanned.Span.union name_sp expr_sp)
        in
        eat_token parser ;
        let%bind members =
          parse_list parser ~f:parse_member_initializer ~sep:Token.Semicolon
            ~close:Token.Close_brace ~expected:Error.Expected.Identifier
        in
        let%bind () = get_specific parser Token.Close_brace in
        let ty = Ast.Type.Named name in
        return (Ast.Expr.Struct_literal (ty, members), true)
    | _, tok when is_expression_end tok -> (Ok (initial, false), sp)
    | _, tok ->
        ( Error (Error.Unexpected_token (Error.Expected.Expression_follow, tok))
        , tok_sp )
  in
  if cont then parse_follow parser (initial, sp) else return (initial, sp)


and parse_type parser =
  (*
    TODO(ubsan): figure out why the heck this is doing this weird spanned thing
  *)
  let%bind tok, sp = spanned_bind (next_token parser) in
  match tok with
  | Token.Identifier id -> return (Ast.Type.Named id, sp)
  | Token.Keyword Token.Keyword.Func -> (
      let%bind () = get_specific parser Token.Open_paren in
      let%bind params =
        parse_list parser ~f:parse_type ~sep:Token.Comma
          ~close:Token.Close_paren ~expected:Error.Expected.Type
      in
      let%bind (), spp =
        spanned_bind (get_specific parser Token.Close_paren)
      in
      match%bind maybe_get_specific parser Token.Arrow with
      | Some () ->
          let%bind ret_ty, spr = spanned_bind (parse_type parser) in
          return
            (Ast.Type.Function (params, Some ret_ty), Spanned.Span.union sp spr)
      | None ->
          return (Ast.Type.Function (params, None), Spanned.Span.union sp spp)
      )
  | tok -> return_err (Error.Unexpected_token (Error.Expected.Type, tok))


and maybe_parse_type_annotation parser =
  match%bind maybe_get_specific parser Token.Colon with
  | Some () ->
      let%bind ty = parse_type parser in
      return (Some ty)
  | None -> return None


and parse_parameter_list parser =
  let get_parm parser =
    let%bind name = get_ident parser in
    let%bind () = get_specific parser Token.Colon in
    let%bind ty = parse_type parser in
    return (name, ty)
  in
  let%bind () = get_specific parser Token.Open_paren in
  let%bind parms =
    parse_list parser ~f:get_parm ~sep:Token.Comma ~close:Token.Close_paren
      ~expected:Error.Expected.Identifier_or_under
  in
  let%bind () = get_specific parser Token.Close_paren in
  return parms


and parse_argument_list parser =
  let%bind () = get_specific parser Token.Open_paren in
  let%bind args =
    parse_list parser ~f:parse_expression ~sep:Token.Comma
      ~close:Token.Close_paren ~expected:Error.Expected.Expression
  in
  let%bind () = get_specific parser Token.Close_paren in
  return args


and parse_block parser =
  (* TODO(ubsan): also does weird stuff with spans *)
  let%bind () = get_specific parser Token.Open_brace in
  match%bind spanned_bind (maybe_parse_expression parser) with
  | Some e, _ ->
      let%bind () = get_specific parser Token.Close_brace in
      return e
  | None, sp ->
      match%bind spanned_bind (next_token parser) with
      | Token.Close_brace, sp' ->
          return (Ast.Expr.Unit_literal, Spanned.Span.union sp sp')
      | tok, _ ->
          return_err (Error.Unexpected_token (Error.Expected.Expression, tok))


let parse_type_definition parser =
  match%bind peek_token parser with
  | Token.Keyword Token.Keyword.Struct ->
      eat_token parser ;
      let parse_member parser =
        let%bind name = get_ident parser in
        let%bind () = get_specific parser Token.Colon in
        let%bind ty = parse_type parser in
        return (name, ty)
      in
      let%bind () = get_specific parser Token.Open_brace in
      let%bind members =
        parse_list parser ~f:parse_member ~sep:Token.Semicolon
          ~close:Token.Close_brace ~expected:Error.Expected.Identifier
      in
      let%bind () = get_specific parser Token.Close_brace in
      return (Ast.Item.Struct members)
  | _ ->
    match parse_type parser with
    | Ok o, sp -> (Ok (Ast.Item.Alias o), sp)
    | Error Error.Unexpected_token (_, tok), sp ->
        ( Error (Error.Unexpected_token (Error.Expected.Type_definition, tok))
        , sp )
    | Error e, sp -> (Error e, sp)


let parse_item (parser: t) : (item option, Error.t) Spanned.Result.t =
  match%bind next_token parser with
  | Token.Keyword Token.Keyword.Func ->
      let%bind fname = get_ident parser in
      let%bind params = parse_parameter_list parser in
      let%bind ret_ty = maybe_parse_type_annotation parser in
      let%bind () = get_specific parser Token.Equals in
      let%bind expr = parse_expression parser in
      let%bind () = get_specific parser Token.Semicolon in
      let func = Ast.Item.{fname; params; ret_ty; expr} in
      return (Some (Item_func func))
  | Token.Keyword Token.Keyword.Type ->
      let%bind tname = get_ident parser in
      let%bind () = get_specific parser Token.Equals in
      let%bind kind = parse_type_definition parser in
      let%bind () = get_specific parser Token.Semicolon in
      let ty = Ast.Item.{tname; kind} in
      return (Some (Item_type ty))
  | Token.Eof -> return None
  | tok ->
      return_err (Error.Unexpected_token (Error.Expected.Item_declarator, tok))


let parse_program (parser: t) : (Ast.t, Error.t) Spanned.Result.t =
  let rec helper parser =
    let%bind item, sp = spanned_bind (parse_item parser) in
    match item with
    | Some Item_func func ->
        let%bind ftl, ttl = helper parser in
        return ((func, sp) :: ftl, ttl)
    | Some Item_type ty ->
        let%bind ftl, ttl = helper parser in
        return (ftl, (ty, sp) :: ttl)
    | None -> return ([], [])
  in
  let%bind funcs, types = helper parser in
  return Ast.{funcs; types}


let parse program =
  let lexer = Lex.lexer program in
  let parser = {lexer; peek= None} in
  parse_program parser
