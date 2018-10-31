module Error = Error
module Ast = Ast
open Spanned.Result.Monad

type t = {lexer: Lexer.t; mutable peek: Token.t Spanned.t option}

let peek_token parser =
  match parser.peek with
  | Some (pk, sp) -> (Ok pk, sp)
  | None -> (
    match Lexer.next_token parser.lexer with
    | Ok ret, sp ->
        parser.peek <- Some (ret, sp) ;
        (Ok ret, sp)
    | Error e, sp -> (Error e, sp) )

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
  type t = Func of Ast.Func.t | Type_definition of Ast.Type.Definition.t
end

type 'a result = ('a, Error.t) Spanned.Result.t

let get_ident (parser : t) : string result =
  match%bind next_token parser with
  | Token.Identifier id -> return id
  | tok -> return_err (Error.Unexpected_token (Error.Expected.Identifier, tok))

let maybe_get_specific (parser : t) (token : Token.t) : unit option result =
  let%bind tok = peek_token parser in
  if Token.equal tok token then ( eat_token parser ; return (Some ()) )
  else return None

let get_specific (parser : t) (token : Token.t) : unit result =
  match%bind maybe_get_specific parser token with
  | Some () -> return ()
  | None ->
      let%bind tok = next_token parser in
      return_err (Error.Unexpected_token (Error.Expected.Specific token, tok))

let parse_list (parser : t) ~(f : t -> 'a result) ~(sep : Token.t)
    ~(close : Token.t) ~(expected : Error.Expected.t) :
    'a Spanned.t list result =
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
        let%bind x = spanned_bind (f parser) in
        let%bind xs = helper parser f expected sep close true in
        return (x :: xs)
  in
  helper parser f expected sep close false

let rec maybe_parse_expression ?(gt_ends : bool option) (parser : t) :
    Ast.Expr.t option result =
  let initial =
    match%bind spanned_bind (peek_token parser) with
    | Token.Keyword Token.Keyword.True, _ ->
        eat_token parser ;
        return (Some (Ast.Expr.Bool_literal true))
    | Token.Keyword Token.Keyword.False, _ ->
        eat_token parser ;
        return (Some (Ast.Expr.Bool_literal false))
    | Token.Integer_literal n, _ ->
        eat_token parser ;
        return (Some (Ast.Expr.Integer_literal n))
    | Token.Open_paren, _ ->
        eat_token parser ;
        let%bind () = get_specific parser Token.Close_paren in
        return (Some Ast.Expr.Unit_literal)
    | Token.Keyword Token.Keyword.If, _ ->
        eat_token parser ;
        let%bind () = get_specific parser Token.Open_paren in
        let%bind cond = spanned_bind (parse_expression parser) in
        let%bind () = get_specific parser Token.Close_paren in
        let%bind thn = spanned_bind (parse_block parser) in
        let%bind () = get_specific parser (Token.Keyword Token.Keyword.Else) in
        let%bind els = spanned_bind (parse_block parser) in
        return (Some (Ast.Expr.If_else {cond; thn; els}))
    | Token.Identifier name, sp -> (
        eat_token parser ;
        match%bind maybe_get_specific parser Token.Double_colon with
        | Some () ->
            let%bind expr = parse_path_expression parser ~start:(name, sp) in
            return (Some expr)
        | None -> return (Some (Ast.Expr.Variable {path= []; name})) )
    | _ -> return None
  in
  match%bind spanned_bind initial with
  | Some i, sp ->
      let%bind x = parse_follow ?gt_ends parser (i, sp) in
      return (Some x)
  | None, _ -> return None

and parse_path_expression (parser : t) ~(start : string Spanned.t) :
    Ast.Expr.t result =
  let rec helper parser ~(path : string list Spanned.t) =
    let path, sp = path in
    match%bind spanned_bind (peek_token parser) with
    | Token.Identifier name, sp' -> (
        eat_token parser ;
        match%bind peek_token parser with
        | Token.Double_colon ->
            eat_token parser ;
            helper parser ~path:(name :: path, Spanned.Span.union sp sp')
        | _ -> return (Ast.Expr.Variable {path; name}) )
    | Token.Open_brace, sp' ->
        parse_record_literal parser ~path:(path, Spanned.Span.union sp sp')
    | tok, _ ->
        return_err
          (Error.Unexpected_token (Error.Expected.Path_expression, tok))
  in
  let start, sp = start in
  helper parser ~path:([start], sp)

and parse_record_literal (parser : t) ~(path : string list Spanned.t) :
    Ast.Expr.t result =
  let f parser =
    let%bind name = get_ident parser in
    let%bind () = get_specific parser Token.Equals in
    let%bind expr = spanned_bind (parse_expression ~gt_ends:true parser) in
    return (name, expr)
  in
  let%bind () = get_specific parser Token.Open_brace in
  let%bind members =
    parse_list parser ~f ~sep:Token.Semicolon ~close:Token.Close_brace
      ~expected:Error.Expected.Variable_decl
  in
  let%bind () = get_specific parser Token.Close_brace in
  let ty =
    match path with
    | [ty], sp -> (Ast.Type.Named ty, sp)
    | _ -> failwith "no paths in types yet"
  in
  return (Ast.Expr.Record_literal {ty; members})

and parse_expression ?(gt_ends : bool option) (parser : t) : Ast.Expr.t result
    =
  match%bind maybe_parse_expression ?gt_ends parser with
  | Some expr -> return expr
  | None ->
      let%bind tok = next_token parser in
      return_err (Error.Unexpected_token (Error.Expected.Expression, tok))

and parse_follow ?(gt_ends : bool = false) (parser : t)
    ((initial, sp) : Ast.Expr.t Spanned.t) : Ast.Expr.t result =
  let%bind tok, tok_sp = spanned_bind (peek_token parser) in
  let%bind total, cont =
    match tok with
    | Token.Open_paren ->
        let%bind args = parse_argument_list parser in
        return (Ast.Expr.Call ((initial, sp), args), true)
    | Token.Dot ->
        eat_token parser ;
        let%bind member = get_ident parser in
        return (Ast.Expr.Record_access ((initial, sp), member), true)
    | tok when is_expression_end tok -> (Ok (initial, false), sp)
    | tok ->
        ( Error (Error.Unexpected_token (Error.Expected.Expression_follow, tok))
        , tok_sp )
  in
  if cont then parse_follow ~gt_ends parser (total, sp) else return total

and parse_return_type (parser : t) : Ast.Type.t Spanned.t option result =
  match%bind maybe_get_specific parser Token.Arrow with
  | Some () ->
      let%bind ret_ty = spanned_bind (parse_type parser) in
      return (Some ret_ty)
  | None -> return None

and parse_type (parser : t) : Ast.Type.t result =
  let%bind tok = next_token parser in
  match tok with
  | Token.Identifier id -> return (Ast.Type.Named id)
  | Token.Keyword Token.Keyword.Func ->
      let%bind () = get_specific parser Token.Open_paren in
      let%bind params =
        parse_list parser ~f:parse_type ~sep:Token.Comma
          ~close:Token.Close_paren ~expected:Error.Expected.Type
      in
      let%bind () = get_specific parser Token.Close_paren in
      let%bind ret_ty = parse_return_type parser in
      return (Ast.Type.Function (params, ret_ty))
  | tok -> return_err (Error.Unexpected_token (Error.Expected.Type, tok))

and parse_data (parser : t) : Ast.Type.Data.t result =
  let%bind tok = next_token parser in
  match tok with
  | Token.Keyword Token.Keyword.Record ->
      let%bind () = get_specific parser Token.Open_brace in
      let%bind members =
        let f parser =
          let%bind name = get_ident parser in
          let%bind () = get_specific parser Token.Colon in
          let%bind ty = parse_type parser in
          return (name, ty)
        in
        parse_list parser ~f ~sep:Token.Semicolon ~close:Token.Close_brace
          ~expected:Error.Expected.Variable_decl
      in
      let%bind () = get_specific parser Token.Close_brace in
      return (Ast.Type.Data.Record members)
  | tok -> return_err (Error.Unexpected_token (Error.Expected.Data, tok))

and maybe_parse_type_annotation (parser : t) :
    Ast.Type.t Spanned.t option result =
  match%bind maybe_get_specific parser Token.Colon with
  | Some () ->
      let%bind ty = spanned_bind (parse_type parser) in
      return (Some ty)
  | None -> return None

and parse_parameter_list (parser : t) :
    (string Spanned.t * Ast.Type.t Spanned.t) Spanned.t list result =
  let f parser =
    let%bind name = spanned_bind (get_ident parser) in
    let%bind () = get_specific parser Token.Colon in
    let%bind ty = spanned_bind (parse_type parser) in
    return (name, ty)
  in
  let%bind () = get_specific parser Token.Open_paren in
  let%bind parms =
    parse_list parser ~f ~sep:Token.Comma ~close:Token.Close_paren
      ~expected:Error.Expected.Variable_decl
  in
  let%bind () = get_specific parser Token.Close_paren in
  return parms

and parse_argument_list (parser : t) : Ast.Expr.t Spanned.t list result =
  let%bind () = get_specific parser Token.Open_paren in
  let%bind args =
    let f parser = parse_expression parser in
    parse_list parser ~f ~sep:Token.Comma ~close:Token.Close_paren
      ~expected:Error.Expected.Expression
  in
  let%bind () = get_specific parser Token.Close_paren in
  return args

and parse_block_no_open (parser : t) : Ast.Expr.block result =
  match%bind peek_token parser with
  | Token.Keyword Token.Keyword.Let ->
      eat_token parser ;
      let%bind name, name_sp = spanned_bind (get_ident parser) in
      let name = (name, name_sp) in
      let%bind ty = maybe_parse_type_annotation parser in
      let%bind () = get_specific parser Token.Equals in
      let%bind expr = spanned_bind (parse_expression parser) in
      let%bind (), semi_sp =
        spanned_bind (get_specific parser Token.Semicolon)
      in
      let%bind blk = parse_block_no_open parser in
      let full_sp = Spanned.Span.union name_sp semi_sp in
      let stmt = (Ast.Stmt.(Let {name; ty; expr}), full_sp) in
      return Ast.Expr.{blk with stmts= stmt :: blk.stmts}
  | _ -> (
      match%bind spanned_bind (maybe_parse_expression parser) with
      | Some e, sp -> (
          match%bind spanned_bind (next_token parser) with
          | Token.Close_brace, _ ->
              return Ast.Expr.{stmts= []; expr= Some (e, sp)}
          | Token.Semicolon, semi_sp ->
              let%bind blk = parse_block_no_open parser in
              let full_sp = Spanned.Span.union sp semi_sp in
              let stmt = (Ast.Stmt.Expression (e, sp), full_sp) in
              return Ast.Expr.{blk with stmts= stmt :: blk.stmts}
          | tok, _ ->
              return_err
                (Error.Unexpected_token (Error.Expected.Statement_end, tok)) )
      | None, _ -> (
          match%bind next_token parser with
          | Token.Close_brace -> return Ast.Expr.{stmts= []; expr= None}
          | tok ->
              return_err
                (Error.Unexpected_token (Error.Expected.Expression, tok)) ) )

and parse_block (parser : t) : Ast.Expr.block result =
  let%bind () = get_specific parser Token.Open_brace in
  parse_block_no_open parser

and parse_user_defined_type (parser : t) : Ast.Type.Definition.t result =
  let%bind name = get_ident parser in
  let%bind () = get_specific parser Token.Open_brace in
  let%bind () = get_specific parser (Token.Keyword Token.Keyword.Data) in
  let%bind () = get_specific parser Token.Equals in
  let%bind data = parse_data parser in
  let%bind () = get_specific parser Token.Semicolon in
  let%bind () = get_specific parser Token.Close_brace in
  let kind = Ast.Type.Definition.User_defined {data} in
  return Ast.Type.Definition.{name; kind}

let parse_item (parser : t) : (Item.t option, Error.t) Spanned.Result.t =
  match%bind next_token parser with
  | Token.Keyword Token.Keyword.Func ->
      let%bind name = get_ident parser in
      let%bind params = parse_parameter_list parser in
      let%bind ret_ty = parse_return_type parser in
      let%bind body = spanned_bind (parse_block parser) in
      let func = Ast.Func.{name; params; ret_ty; body} in
      return (Some (Item.Func func))
  | Token.Keyword Token.Keyword.Alias ->
      let%bind name = get_ident parser in
      let%bind () = get_specific parser Token.Equals in
      let%bind ty = parse_type parser in
      let%bind () = get_specific parser Token.Semicolon in
      let kind = Ast.Type.Definition.Alias ty in
      let def = Ast.Type.Definition.{name; kind} in
      return (Some (Item.Type_definition def))
  | Token.Keyword Token.Keyword.Type ->
      let%bind def = parse_user_defined_type parser in
      return (Some (Item.Type_definition def))
  | Token.Eof -> return None
  | tok ->
      return_err (Error.Unexpected_token (Error.Expected.Item_declarator, tok))

let parse_program (parser : t) : (Ast.t, Error.t) Spanned.Result.t =
  let rec helper parser =
    let%bind item, sp = spanned_bind (parse_item parser) in
    match item with
    | None -> return Ast.{funcs= []; types= []}
    | Some item -> (
        let%bind rest = helper parser in
        match item with
        | Item.Func func ->
            return Ast.{rest with funcs= (func, sp) :: rest.funcs}
        | Item.Type_definition def ->
            return Ast.{rest with types= (def, sp) :: rest.types} )
  in
  helper parser

let parse program =
  let lexer = Lexer.make program in
  let parser = {lexer; peek= None} in
  parse_program parser
