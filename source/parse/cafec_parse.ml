module Error = Error
module Ast = Ast
open Spanned.Result.Monad

type t = {lexer: Lexer.t; mutable peek: Token.t Spanned.t option}

let peek_token parser =
  match parser.peek with
  | Some (pk, sp) -> (Ok pk, sp)
  | None ->
    match Lexer.next_token parser.lexer with
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


module Item = struct
  type t = Func of Ast.Func.t | Type_alias of (string * Ast.Type.t)
end

type 'a result = ('a, Error.t) Spanned.Result.t

let get_ident (parser: t) : string result =
  match%bind next_token parser with
  | Token.Identifier id -> return id
  | tok -> return_err (Error.Unexpected_token (Error.Expected.Identifier, tok))


let maybe_get_specific (parser: t) (token: Token.t) : unit option result =
  let%bind tok = peek_token parser in
  if Token.equal tok token then ( eat_token parser ; return (Some ()) )
  else return None


let get_specific (parser: t) (token: Token.t) : unit result =
  match%bind maybe_get_specific parser token with
  | Some () -> return ()
  | None ->
      let%bind tok = next_token parser in
      return_err (Error.Unexpected_token (Error.Expected.Specific token, tok))


let parse_list (parser: t) ~(f: t -> 'a result) ~(sep: Token.t)
    ~(close: Token.t) ~(expected: Error.Expected.t) : 'a list result =
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


let rec maybe_parse_expression (parser: t) : Ast.Expr.t option result =
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
        let%bind cond = spanned_bind (parse_expression parser) in
        let%bind () = get_specific parser Token.Close_paren in
        let%bind thn = spanned_bind (parse_block parser) in
        let%bind () = get_specific parser (Token.Keyword Token.Keyword.Else) in
        let%bind els = spanned_bind (parse_block parser) in
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


and parse_expression (parser: t) : Ast.Expr.t result =
  match%bind maybe_parse_expression parser with
  | Some expr -> return expr
  | None ->
      let%bind tok = next_token parser in
      return_err (Error.Unexpected_token (Error.Expected.Expression, tok))


and parse_follow (parser: t) ((initial, sp): Ast.Expr.t Spanned.t)
    : Ast.Expr.t result =
  let%bind tok, tok_sp = spanned_bind (peek_token parser) in
  let%bind total, cont =
    match tok with
    | Token.Open_paren ->
        let%bind args = parse_argument_list parser in
        return (Ast.Expr.Call ((initial, sp), args), true)
    | Token.Dot ->
        eat_token parser ;
        let%bind member = get_ident parser in
        return (Ast.Expr.Struct_access ((initial, sp), member), true)
    | tok when is_expression_end tok -> (Ok (initial, false), sp)
    | tok ->
        ( Error (Error.Unexpected_token (Error.Expected.Expression_follow, tok))
        , tok_sp )
  in
  if cont then parse_follow parser (total, sp) else return total


and parse_type (parser: t) : Ast.Type.t result =
  let%bind tok = next_token parser in
  match tok with
  | Token.Identifier id -> return (Ast.Type.Named id)
  | Token.Operator "<" ->
      let%bind members =
        let f parser =
          let%bind name, nsp = spanned_bind (get_ident parser) in
          let%bind () = get_specific parser Token.Colon in
          let%bind ty, tsp = spanned_bind (parse_type parser) in
          return ((name, ty), Spanned.Span.union nsp tsp)
        in
        parse_list parser ~f ~sep:Token.Semicolon ~close:(Token.Operator ">")
          ~expected:Error.Expected.Variable_decl
      in
      let%bind () = get_specific parser (Token.Operator ">") in
      return (Ast.Type.Record members)
  | Token.Keyword Token.Keyword.Func -> (
      let%bind () = get_specific parser Token.Open_paren in
      let%bind params =
        let f parser = spanned_bind (parse_type parser) in
        parse_list parser ~f ~sep:Token.Comma ~close:Token.Close_paren
          ~expected:Error.Expected.Type
      in
      let%bind () = get_specific parser Token.Close_paren in
      match%bind maybe_get_specific parser Token.Arrow with
      | Some () ->
          let%bind ret_ty = spanned_bind (parse_type parser) in
          return (Ast.Type.Function (params, Some ret_ty))
      | None -> return (Ast.Type.Function (params, None)) )
  | tok -> return_err (Error.Unexpected_token (Error.Expected.Type, tok))


and maybe_parse_type_annotation (parser: t) : Ast.Type.t option result =
  match%bind maybe_get_specific parser Token.Colon with
  | Some () ->
      let%bind ty = parse_type parser in
      return (Some ty)
  | None -> return None


and parse_parameter_list (parser: t)
    : (string * Ast.Type.t) Spanned.t list result =
  let get_parm parser =
    let%bind name, nsp = spanned_bind (get_ident parser) in
    let%bind () = get_specific parser Token.Colon in
    let%bind ty, tsp = spanned_bind (parse_type parser) in
    return ((name, ty), Spanned.Span.union nsp tsp)
  in
  let%bind () = get_specific parser Token.Open_paren in
  let%bind parms =
    parse_list parser ~f:get_parm ~sep:Token.Comma ~close:Token.Close_paren
      ~expected:Error.Expected.Variable_decl
  in
  let%bind () = get_specific parser Token.Close_paren in
  return parms


and parse_argument_list (parser: t) : Ast.Expr.t Spanned.t list result =
  let f parser = spanned_bind (parse_expression parser) in
  let%bind () = get_specific parser Token.Open_paren in
  let%bind args =
    parse_list parser ~f ~sep:Token.Comma ~close:Token.Close_paren
      ~expected:Error.Expected.Expression
  in
  let%bind () = get_specific parser Token.Close_paren in
  return args


and parse_block (parser: t) : Ast.Expr.t result =
  let%bind () = get_specific parser Token.Open_brace in
  match%bind maybe_parse_expression parser with
  | Some e ->
      let%bind () = get_specific parser Token.Close_brace in
      return e
  | None ->
      match%bind next_token parser with
      | Token.Close_brace -> return Ast.Expr.Unit_literal
      | tok ->
          return_err (Error.Unexpected_token (Error.Expected.Expression, tok))


let parse_item (parser: t) : (Item.t option, Error.t) Spanned.Result.t =
  match%bind next_token parser with
  | Token.Keyword Token.Keyword.Func ->
      let%bind name = get_ident parser in
      let%bind params = parse_parameter_list parser in
      let%bind ret_ty =
        match%bind spanned_bind (maybe_parse_type_annotation parser) with
        | Some e, sp -> return (Some (e, sp))
        | None, _ -> return None
      in
      let%bind () = get_specific parser Token.Equals in
      let%bind expr = spanned_bind (parse_expression parser) in
      let%bind () = get_specific parser Token.Semicolon in
      let func = Ast.Func.{name; params; ret_ty; expr} in
      return (Some (Item.Func func))
  | Token.Keyword Token.Keyword.Type ->
      let%bind name = get_ident parser in
      let%bind () = get_specific parser Token.Equals in
      let%bind def = parse_type parser in
      let%bind () = get_specific parser Token.Semicolon in
      return (Some (Item.Type_alias (name, def)))
  | Token.Eof -> return None
  | tok ->
      return_err (Error.Unexpected_token (Error.Expected.Item_declarator, tok))


let parse_program (parser: t) : (Ast.t, Error.t) Spanned.Result.t =
  let rec helper parser =
    let%bind item, sp = spanned_bind (parse_item parser) in
    match item with
    | Some Item.Func func ->
        let%bind ftl, ttl = helper parser in
        return ((func, sp) :: ftl, ttl)
    | Some Item.Type_alias ty ->
        let%bind ftl, ttl = helper parser in
        return (ftl, (ty, sp) :: ttl)
    | None -> return ([], [])
  in
  let%bind funcs, aliases = helper parser in
  return Ast.{funcs; aliases}


let parse program =
  let lexer = Lexer.make program in
  let parser = {lexer; peek= None} in
  parse_program parser
