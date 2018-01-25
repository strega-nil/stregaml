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


let wrap_sok : ('o, 'e) spanned_result -> ('o spanned, 'e) spanned_result =
  function
  | Ok (o, sp) -> Ok ((o, sp), sp)
  | Error e -> Error e


let wrap_opt_sok
    : ('o option, 'e) spanned_result -> ('o spanned option, 'e) spanned_result =
  function
  | Ok (Some o, sp) -> Ok (Some (o, sp), sp)
  | Ok (None, sp) -> Ok (None, sp)
  | Error e -> Error e


type item = Item_func of Ast.Function.builder

let get_ident parser =
  [%bind
    match next_token parser with
    | Token.Identifier id -> wrap id
    | tok -> wrap_err (Error.Unexpected_token (Error.Expected_identifier, tok))]


let maybe_get_specific parser token =
  let%bind tok = peek_token parser in
  if tok = token then ( eat_token parser ; wrap (Some ()) ) else wrap None


let get_specific parser token =
  [%bind
    match maybe_get_specific parser token with
    | Some () -> wrap ()
    | None ->
        let%bind tok = next_token parser in
        wrap_err (Error.Unexpected_token (Error.Expected_specific token, tok))]


let rec maybe_parse_expression parser =
  let initial =
    [%bind
      match peek_token parser with
      | Token.Keyword Token.Keyword_true ->
          eat_token parser ;
          wrap (Some (Ast.Expr.Bool_literal true))
      | Token.Keyword Token.Keyword_false ->
          eat_token parser ;
          wrap (Some (Ast.Expr.Bool_literal false))
      | Token.Integer_literal n ->
          eat_token parser ;
          wrap (Some (Ast.Expr.Integer_literal n))
      | Token.Open_paren ->
          eat_token parser ;
          let%bind () = get_specific parser Token.Close_paren in
          wrap (Some Ast.Expr.Unit_literal)
      | Token.Keyword Token.Keyword_if ->
          eat_token parser ;
          let%bind () = get_specific parser Token.Open_paren in
          let%bind cond = parse_expression parser in
          let%bind () = get_specific parser Token.Close_paren in
          let%bind thn = parse_block parser in
          let%bind () =
            get_specific parser (Token.Keyword Token.Keyword_else)
          in
          let%bind els = parse_block parser in
          wrap (Some (Ast.Expr.If_else (cond, thn, els)))
      | Token.Identifier s ->
          eat_token parser ;
          wrap (Some (Ast.Expr.Variable s))
      | _ -> wrap None]
  in
  [%bind
    match wrap_opt_sok initial with
    | Some i ->
        let%bind x = parse_follow parser i in
        wrap (Some x)
    | None -> wrap None]


and parse_expression parser =
  [%bind
    match maybe_parse_expression parser with
    | Some expr -> wrap expr
    | None ->
        let%bind tok = next_token parser in
        wrap_err (Error.Unexpected_token (Error.Expected_expression, tok))]


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
  let unwrapped =
    let%bind name = get_ident parser in
    wrap (Ast.Type.Named name)
  in
  wrap_sok unwrapped


and maybe_parse_type_annotation parser =
  [%bind
    match maybe_get_specific parser Token.Colon with
    | Some () ->
        let%bind ty = parse_type parser in
        wrap (Some ty)
    | None -> wrap None]


and parse_parameter_list parser =
  (* TODO(ubsan): maybe abstract out list parsing? *)
  let rec get_parms comma =
    [%bind
      match peek_token parser with
      | Token.Close_paren -> wrap []
      | Token.Comma ->
          if comma then ( eat_token parser ; get_parms false )
          else
            wrap_err
              (Error.Unexpected_token (Error.Expected_expression, Token.Comma))
      | tok ->
          if comma then
            wrap_err
              (Error.Unexpected_token (Error.Expected_specific Token.Comma, tok))
          else
            let%bind name = get_ident parser in
            let%bind () = get_specific parser Token.Colon in
            let%bind ty = parse_type parser in
            let%bind tl = get_parms true in
            wrap ((name, ty) :: tl)]
  in
  let%bind () = get_specific parser Token.Open_paren in
  let%bind parms = get_parms false in
  let%bind () = get_specific parser Token.Close_paren in
  wrap parms


and parse_argument_list parser =
  let rec get_args comma =
    [%bind
      match peek_token parser with
      | Token.Close_paren -> wrap []
      | Token.Comma ->
          if comma then ( eat_token parser ; get_args false )
          else
            wrap_err
              (Error.Unexpected_token (Error.Expected_expression, Token.Comma))
      | tok ->
          if comma then
            wrap_err
              (Error.Unexpected_token (Error.Expected_specific Token.Comma, tok))
          else
            let%bind expr = parse_expression parser in
            let%bind tl = get_args true in
            wrap (expr :: tl)]
  in
  let%bind () = get_specific parser Token.Open_paren in
  let%bind args = get_args false in
  let%bind () = get_specific parser Token.Close_paren in
  wrap args


and parse_block parser =
  let%bind () = get_specific parser Token.Open_brace in
  [%bind
    match maybe_parse_expression parser with
    | Some e ->
        let%bind () = get_specific parser Token.Close_brace in
        wrap e
    | None ->
        wrap_sok
          [%bind
            match next_token parser with
            | Token.Close_brace -> wrap Ast.Expr.Unit_literal
            | tok ->
                wrap_err
                  (Error.Unexpected_token (Error.Expected_expression, tok))]]


let parse_item (parser: t) : (item option, Error.t) spanned_result =
  [%bind
    match next_token parser with
    | Token.Keyword Token.Keyword_let ->
        let%bind name = get_ident parser in
        let%bind params = parse_parameter_list parser in
        let%bind ret_ty = maybe_parse_type_annotation parser in
        let%bind () = get_specific parser Token.Equals in
        let%bind expr = parse_expression parser in
        let%bind () = get_specific parser Token.Semicolon in
        let func = Ast.Function.{name; params; ret_ty; expr} in
        wrap (Some (Item_func func))
    | Token.Eof -> wrap None
    | tok ->
        wrap_err (Error.Unexpected_token (Error.Expected_item_declarator, tok))]


let parse_program (parser: t) : (Ast.t, Error.t) spanned_result =
  let rec helper parser =
    let%bind item, sp = parse_item parser |> wrap_sok in
    match item with
    | Some Item_func func ->
        let%bind tl = helper parser in
        wrap ((func, sp) :: tl)
    | None -> wrap []
  in
  let%bind funcs = helper parser in
  wrap (Ast.make funcs)


let parse program =
  let lexer = Lex.lexer program in
  let parser = {lexer; peek= None} in
  parse_program parser
