open! Types.Pervasives
module Ast = Ast
module Error = Error
module Type = Type

type t =
  | Parser : {lexer : Lexer.t; mutable peek : Token.t Spanned.t option} -> t

let peek_token (Parser r) =
  match r.peek with
  | Some (pk, sp) -> (Ok pk, sp)
  | None -> (
    match Lexer.next_token r.lexer with
    | Ok ret, sp ->
        r.peek <- Some (ret, sp) ;
        (Ok ret, sp)
    | Error e, sp -> (Error e, sp) )

let next_token parser =
  let ret = peek_token parser in
  let (Parser r) = parser in
  r.peek <- None ;
  ret

let eat_token parser =
  match next_token parser with Ok _, _ -> () | Error _, _ -> assert false

(* context sensitive keywords *)
module Ctxt_keyword = struct
  let equal = Nfc_string.of_string_unsafe "="

  let less = Nfc_string.of_string_unsafe "<"

  let greater = Nfc_string.of_string_unsafe ">"

  let equal_tok = Token.Operator equal

  (*
    let less_tok = Token.Operator less
    let greater_tok = Token.Operator greater
  *)

  let reference = Nfc_string.of_string_unsafe "&"

  let dereference = Nfc_string.of_string_unsafe "*"

  (*
  let reference_tok = Token.Operator reference

  let dereference_tok = Token.Operator dereference
  *)

  let assoc_start = Nfc_string.of_string_unsafe "start"

  let assoc_end = Nfc_string.of_string_unsafe "end"

  let assoc_none = Nfc_string.of_string_unsafe "none"

  let precedence = Nfc_string.of_string_unsafe "precedence"

  let associativity = Nfc_string.of_string_unsafe "associativity"

  (*
  let assoc_start_tok = Token.Identifier assoc_start
  let assoc_end_tok = Token.Identifier assoc_end
  let assoc_none_tok = Token.Identifier assoc_none

  let precedence_tok = Token.Identifier precedence
  let associativity_tok = Token.Identifier associativity
  *)
end

module Item = struct
  type t =
    | Func of Ast.Func.t
    | Infix_group of Ast.Infix_group.t
    | Infix_declaration of Ast.Infix_declaration.t
    | Type_definition of Type.Definition.t
end

let is_postfix_token = function
  | Token.Dot -> true
  | Token.Open_paren -> true
  | _ -> false

let is_infix_token = function
  | Token.Operator _ -> true
  | Token.Identifier_operator _ -> true
  | Token.Assign -> true
  | _ -> false

let is_identifier_token = function
  | Token.Identifier _ -> true
  | Token.Open_paren -> true
  | _ -> false

let is_place_token = function
  | Token.Keyword_mut -> true
  | Token.Keyword_ref -> true
  | _ -> false

let unexpected (tok : Token.t) (e : Error.Expected.t) =
  return_err (Error.Unexpected_token (e, tok))

let maybe_get_specific (parser : t) (token : Token.t) : unit option result =
  let%bind tok = peek_token parser in
  if Token.equal tok token
  then ( eat_token parser ; return (Some ()) )
  else return None

let get_specific (parser : t) (token : Token.t) : unit result =
  match%bind maybe_get_specific parser token with
  | Some () -> return ()
  | None ->
      let%bind tok = next_token parser in
      unexpected tok (Error.Expected.Specific token)

let maybe_get_infix_operator (parser : t) : Name.infix Name.t option result =
  match%bind peek_token parser with
  | Token.Operator string ->
      eat_token parser ;
      let op = Name.Name {string; kind = Name.Operator; fixity = Name.Infix} in
      return (Some op)
  | Token.Identifier_operator string ->
      eat_token parser ;
      let op =
        Name.Name {string; kind = Name.Identifier; fixity = Name.Infix}
      in
      return (Some op)
  | _ -> return None

let get_infix_operator (parser : t) : Name.infix Name.t result =
  match%bind maybe_get_infix_operator parser with
  | Some op -> return op
  | None ->
      let%bind tok = next_token parser in
      unexpected tok Error.Expected.Operator

let get_prefix_operator (parser : t) : Name.prefix Name.t result =
  let%bind (Name.Name {string; kind; _}) = get_infix_operator parser in
  return (Name.Name {string; kind; fixity = Name.Prefix})

let get_identifier (parser : t) : Nfc_string.t result =
  match%bind next_token parser with
  | Token.Identifier id -> return id
  | tok -> unexpected tok Error.Expected.Identifier

let get_name (parser : t) : Name.anyfix Name.t result =
  match%bind next_token parser with
  | Token.Open_paren ->
      let%bind name =
        match%bind next_token parser with
        | Token.Operator string ->
            let fixity = Name.Anyfix Name.Nonfix in
            return (Name.Name {string; kind = Name.Operator; fixity})
        | Token.Identifier_operator string ->
            let fixity = Name.Anyfix Name.Nonfix in
            return (Name.Name {string; kind = Name.Identifier; fixity})
        | Token.Keyword_infix ->
            Return.map (get_infix_operator parser) ~f:Name.erase
        | Token.Keyword_prefix ->
            Return.map (get_prefix_operator parser) ~f:Name.erase
        | tok -> unexpected tok Error.Expected.Operator
      in
      let%bind () = get_specific parser Token.Close_paren in
      return name
  | Token.Identifier string ->
      let fixity = Name.Anyfix Name.Nonfix in
      return (Name.Name {string; kind = Name.Identifier; fixity})
  | tok -> unexpected tok Error.Expected.Name

let parse_list (parser : t) ~(f : t -> 'a result) ~(sep : Token.t)
    ~(close : Token.t) ~(expected : Error.Expected.t) :
    'a Spanned.t list result =
  let rec helper parser f expected sep close expect_sep =
    let%bind tok = peek_token parser in
    match () with
    | () when Token.equal tok close -> return []
    | () when Token.equal tok sep ->
        if expect_sep
        then (
          eat_token parser ;
          helper parser f expected sep close false )
        else unexpected sep expected
    | () when expect_sep -> unexpected tok (Error.Expected.Specific sep)
    | () ->
        let%bind x = spanned_bind (f parser) in
        let%bind xs = helper parser f expected sep close true in
        return (x :: xs)
  in
  helper parser f expected sep close false

let rec maybe_parse_expression_no_infix (parser : t) : Ast.Expr.t option result
    =
  let get_postfix expr =
    let rec parse_all_postfix (expr : Ast.Expr.t Spanned.t) parser =
      let%bind tok = peek_token parser in
      if is_postfix_token tok
      then
        let%bind expr = spanned_bind (parse_postfix parser expr) in
        parse_all_postfix expr parser
      else
        let expr, _ = expr in
        return expr
    in
    parse_all_postfix expr parser
  in
  match%bind spanned_bind (peek_token parser) with
  | Token.Keyword_true, sp ->
      eat_token parser ;
      let%bind expr = get_postfix (Ast.Expr.Bool_literal true, sp) in
      return (Some expr)
  | Token.Keyword_false, sp ->
      eat_token parser ;
      let%bind expr = get_postfix (Ast.Expr.Bool_literal false, sp) in
      return (Some expr)
  | Token.Integer_literal n, sp ->
      eat_token parser ;
      let%bind expr = get_postfix (Ast.Expr.Integer_literal n, sp) in
      return (Some expr)
  | Token.Open_paren, sp ->
      eat_token parser ;
      let%bind expr =
        match%bind spanned_bind (peek_token parser) with
        | Token.Close_paren, sp' ->
            eat_token parser ;
            get_postfix (Ast.Expr.Unit_literal, Spanned.Span.union sp sp')
        | _ ->
            let%bind expr = parse_expression parser in
            let%bind (), sp' =
              spanned_bind (get_specific parser Token.Close_paren)
            in
            get_postfix (expr, Spanned.Span.union sp sp')
      in
      return (Some expr)
  | Token.Open_brace, _ ->
      let%bind blk, sp = spanned_bind (parse_block parser) in
      let%bind expr = get_postfix (Ast.Expr.Block (blk, sp), sp) in
      return (Some expr)
  | Token.Keyword_match, _ ->
      let%bind expr = spanned_bind (parse_match_expression parser) in
      let%bind expr = get_postfix expr in
      return (Some expr)
  | Token.Keyword_if, sp ->
      eat_token parser ;
      let%bind () = get_specific parser Token.Open_paren in
      let%bind cond = spanned_bind (parse_expression parser) in
      let%bind () = get_specific parser Token.Close_paren in
      let%bind thn = spanned_bind (parse_block parser) in
      let%bind () = get_specific parser Token.Keyword_else in
      let%bind els = spanned_bind (parse_block parser) in
      let _, sp' = els in
      let sp = Spanned.Span.union sp sp' in
      let%bind expr = get_postfix (Ast.Expr.If_else {cond; thn; els}, sp) in
      return (Some expr)
  | Token.Keyword_ref, sp ->
      eat_token parser ;
      let%bind () = with_span sp in
      let mutability = (Type.Immutable, sp) in
      let%bind expr = spanned_bind (parse_expression_no_infix parser) in
      return (Some (Ast.Expr.Place {mutability; expr}))
  | Token.Keyword_mut, sp ->
      eat_token parser ;
      let%bind () = with_span sp in
      let mutability = (Type.Mutable, sp) in
      let%bind expr = spanned_bind (parse_expression_no_infix parser) in
      return (Some (Ast.Expr.Place {mutability; expr}))
  | Token.Operator op, _ when Nfc_string.equal op Ctxt_keyword.reference ->
      eat_token parser ;
      let%bind place = spanned_bind (parse_expression_no_infix parser) in
      return (Some (Ast.Expr.Reference place))
  | Token.Operator _, sp | Token.Identifier_operator _, sp ->
      let%bind op = get_prefix_operator parser in
      let%bind expr = spanned_bind (parse_expression_no_infix parser) in
      if Nfc_string.equal (Name.string op) Ctxt_keyword.dereference
      then return (Some (Ast.Expr.Dereference expr))
      else return (Some (Ast.Expr.Prefix_operator ((op, sp), expr)))
  | Token.Keyword_builtin, sp ->
      eat_token parser ;
      let%bind () = get_specific parser Token.Open_paren in
      let%bind name = spanned_bind (get_identifier parser) in
      let%bind () = get_specific parser Token.Close_paren in
      let%bind args, sp' = spanned_bind (parse_argument_list parser) in
      let sp = Spanned.Span.union sp sp' in
      let%bind expr = get_postfix (Ast.Expr.Builtin (name, args), sp) in
      return (Some expr)
  | tok, _ when is_identifier_token tok ->
      let%bind path, sp = spanned_bind (parse_path_expression parser) in
      let%bind expr = get_postfix (path, sp) in
      return (Some expr)
  | _ -> return None

and parse_path_expression (parser : t) : Ast.Expr.t result =
  let rec helper parser ~(path : Nfc_string.t Spanned.t list Spanned.t) =
    let path, sp = path in
    match%bind spanned_bind (peek_token parser) with
    | Token.Identifier string, sp' -> (
        eat_token parser ;
        match%bind peek_token parser with
        | Token.Double_colon ->
            eat_token parser ;
            helper parser
              ~path:((string, sp') :: path, Spanned.Span.union sp sp')
        | _ ->
            let name =
              ( Name.Name {string; kind = Name.Identifier; fixity = Name.Nonfix}
              , sp' )
            in
            return Ast.Expr.(Name (Qualified {path; name})) )
    | Token.Open_paren, _ ->
        let%bind name = spanned_bind (get_name parser) in
        return Ast.Expr.(Name (Qualified {path; name}))
    | Token.Open_brace, sp' ->
        parse_record_literal parser ~path:(path, Spanned.Span.union sp sp')
    | tok, _ -> unexpected tok Error.Expected.Path_expression
  in
  helper parser ~path:([], Spanned.Span.made_up)

(*
  not very DRY with parse_path_expression
  TODO: figure that out
*)
and parse_qualified_name (parser : t) : Name.nonfix Ast.Expr.qualified result =
  let rec helper parser ~(path : Nfc_string.t Spanned.t list) =
    match%bind spanned_bind (peek_token parser) with
    | Token.Identifier string, sp' -> (
        eat_token parser ;
        match%bind peek_token parser with
        | Token.Double_colon ->
            eat_token parser ;
            helper parser ~path:((string, sp') :: path)
        | _ ->
            let fixity = Name.Nonfix in
            let name =
              (Name.Name {string; kind = Name.Identifier; fixity}, sp')
            in
            return (Ast.Expr.Qualified {path; name}) )
    | tok, _ -> unexpected tok Error.Expected.Path
  in
  helper parser ~path:[]

and parse_match_expression (parser : t) : Ast.Expr.t result =
  let rec parse_match_arms (parser : t) :
      (Ast.Expr.pattern Spanned.t * Ast.Expr.Block.t Spanned.t) list result =
    match%bind peek_token parser with
    | Token.Close_brace -> return []
    | Token.Identifier _ ->
        let%bind constructor = spanned_bind (parse_qualified_name parser) in
        let%bind () = get_specific parser Token.Open_paren in
        let%bind binding = spanned_bind (get_name parser) in
        let%bind () = get_specific parser Token.Close_paren in
        let%bind () = get_specific parser Token.Thicc_arrow in
        let%bind block = spanned_bind (parse_block parser) in
        let%bind rest = parse_match_arms parser in
        let pattern =
          let pattern_sp =
            let _, csp = constructor in
            let _, bsp = binding in
            Spanned.Span.union csp bsp
          in
          (Ast.Expr.Pattern {constructor; binding}, pattern_sp)
        in
        let arm = (pattern, block) in
        return (arm :: rest)
    | tok -> unexpected tok Error.Expected.Match_arm
  in
  let%bind () = get_specific parser Token.Keyword_match in
  let%bind () = get_specific parser Token.Open_paren in
  let%bind cond = spanned_bind (parse_expression parser) in
  let%bind () = get_specific parser Token.Close_paren in
  let%bind () = get_specific parser Token.Open_brace in
  let%bind arms = parse_match_arms parser in
  let%bind () = get_specific parser Token.Close_brace in
  return (Ast.Expr.Match {cond; arms})

and parse_record_literal (parser : t)
    ~(path : Nfc_string.t Spanned.t list Spanned.t) : Ast.Expr.t result =
  let f parser =
    let%bind name =
      let%bind string = get_identifier parser in
      return (Name.Name {string; kind = Name.Identifier; fixity = Name.Nonfix})
    in
    let%bind () = get_specific parser Ctxt_keyword.equal_tok in
    let%bind expr = spanned_bind (parse_expression parser) in
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
    | [(ty, _)], sp -> (Type.Named ty, sp)
    | _ -> failwith "no paths in types yet"
  in
  return (Ast.Expr.Record_literal {ty; members})

and maybe_parse_expression (parser : t) : Ast.Expr.t option result =
  match%bind spanned_bind (maybe_parse_expression_no_infix parser) with
  | Some e, e_sp ->
      let%bind tok = peek_token parser in
      if is_infix_token tok
      then
        let%bind infix = parse_infix parser in
        return (Some (Ast.Expr.Infix_list ((e, e_sp), infix)))
      else return (Some e)
  | None, _ -> return None

and parse_expression_no_infix (parser : t) : Ast.Expr.t result =
  match%bind maybe_parse_expression_no_infix parser with
  | Some expr -> return expr
  | None ->
      let%bind tok = next_token parser in
      unexpected tok Error.Expected.Expression

and parse_expression (parser : t) : Ast.Expr.t result =
  match%bind maybe_parse_expression parser with
  | Some expr -> return expr
  | None ->
      let%bind tok = next_token parser in
      unexpected tok Error.Expected.Expression

and parse_infix (parser : t) :
    (Ast.Expr.infix Spanned.t * Ast.Expr.t Spanned.t) list result =
  let%bind tok, tok_sp = spanned_bind (peek_token parser) in
  let%bind infix =
    match tok with
    | Token.Assign ->
        eat_token parser ;
        return (Ast.Expr.Infix_assign, tok_sp)
    | _ ->
        let%bind op = get_infix_operator parser in
        return (Ast.Expr.Infix_name op, tok_sp)
  in
  let%bind expr = spanned_bind (parse_expression_no_infix parser) in
  let%bind next = peek_token parser in
  let%bind rest =
    if is_infix_token next then parse_infix parser else return []
  in
  return ((infix, expr) :: rest)

and parse_postfix (parser : t) ((initial, sp) : Ast.Expr.t Spanned.t) :
    Ast.Expr.t result =
  let%bind tok = peek_token parser in
  match tok with
  | Token.Open_paren ->
      let%bind args = parse_argument_list parser in
      return (Ast.Expr.Call ((initial, sp), args))
  | Token.Dot ->
      eat_token parser ;
      let%bind string = get_identifier parser in
      let name =
        Name.Name {string; kind = Name.Identifier; fixity = Name.Nonfix}
      in
      return (Ast.Expr.Record_access ((initial, sp), name))
  | _ -> failwith "function called incorrectly"

and parse_return_type (parser : t) : Type.any Type.t Spanned.t option result =
  match%bind maybe_get_specific parser Token.Arrow with
  | Some () ->
      let%bind ret_ty = spanned_bind (parse_type parser) in
      return (Some ret_ty)
  | None -> return None

and parse_value_type (parser : t) : Type.value Type.t result =
  let%bind tok = peek_token parser in
  match tok with
  | Token.Operator op when Nfc_string.equal op Ctxt_keyword.reference ->
      eat_token parser ;
      let%bind place = spanned_bind (parse_place_type parser) in
      return (Type.Reference place)
  | Token.Identifier _ ->
      let%bind id = get_identifier parser in
      return (Type.Named id)
  | Token.Keyword_func ->
      eat_token parser ;
      let%bind () = get_specific parser Token.Open_paren in
      let%bind params =
        parse_list parser ~f:parse_type ~sep:Token.Comma
          ~close:Token.Close_paren ~expected:Error.Expected.Type
      in
      let%bind () = get_specific parser Token.Close_paren in
      let%bind ret_ty = parse_return_type parser in
      return (Type.Function {params; ret_ty})
  | tok -> unexpected tok Error.Expected.Type

and parse_place_type (parser : t) : Type.place Type.t result =
  match%bind spanned_bind (peek_token parser) with
  | Token.Keyword_mut, sp ->
      let%bind ty = spanned_bind (parse_value_type parser) in
      return (Type.Place {mutability= Type.Mutable, sp; ty})
  | Token.Keyword_ref, sp ->
      let%bind ty = spanned_bind (parse_value_type parser) in
      return (Type.Place {mutability= Type.Immutable, sp; ty})
  | tok, _ -> unexpected tok Error.Expected.Place

and parse_type (parser : t) : Type.any Type.t result =
  let%bind tok = peek_token parser in
  if is_place_token tok then
    let%bind place_ty = parse_place_type parser in
    return (Type.Any place_ty)
  else
    match parse_value_type parser with
    | Result.Ok o, sp -> Result.Ok (Type.Any o), sp
    | Result.Error (Error.Unexpected_token (Error.Expected.Value_type, tok)), sp ->
        Result.Error (Error.Unexpected_token (Error.Expected.Type, tok)), sp
    | Result.Error e, sp -> Result.Error e, sp

and parse_data_members (parser : t) : Type.Data.members result =
  let%bind () = get_specific parser Token.Open_brace in
  let%bind members =
    let f parser =
      let%bind name = get_identifier parser in
      let%bind () = get_specific parser Token.Colon in
      let%bind ty = parse_value_type parser in
      return (name, ty)
    in
    parse_list parser ~f ~sep:Token.Semicolon ~close:Token.Close_brace
      ~expected:Error.Expected.Variable_decl
  in
  let%bind () = get_specific parser Token.Close_brace in
  return members

(* for simple types of the form <kind> type X { ... } *)
and parse_simple_type ~(kind : Type.Data.kind) (parser : t) :
    Type.Definition.t result =
  let%bind () = get_specific parser Token.Keyword_type in
  let%bind name = spanned_bind (get_identifier parser) in
  let%bind members = parse_data_members parser in
  let data = Type.Data.Data {kind; members} in
  let kind = Type.Definition.User_defined {data} in
  return (Type.Definition.Definition {kind; name})

and maybe_parse_type_annotation (parser : t) :
    Type.any Type.t Spanned.t option result =
  match%bind maybe_get_specific parser Token.Colon with
  | Some () ->
      let%bind ty = spanned_bind (parse_type parser) in
      return (Some ty)
  | None -> return None

and parse_parameter_list (parser : t) : Ast.Func.params result =
  let f parser =
    let%bind name = spanned_bind (get_name parser) in
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

and parse_block_no_open (parser : t) : Ast.Expr.Block.t result =
  let module B = Ast.Expr.Block in
  match%bind peek_token parser with
  | Token.Keyword_let ->
      eat_token parser ;
      let%bind stmt =
        let%bind mut_kw = maybe_get_specific parser Token.Keyword_mut in
        let is_mut = match mut_kw with Some () -> true | None -> false in
        let%bind name, name_sp = spanned_bind (get_name parser) in
        let name = (name, name_sp) in
        let%bind ty = maybe_parse_type_annotation parser in
        let%bind () = get_specific parser Ctxt_keyword.equal_tok in
        let%bind expr = spanned_bind (parse_expression parser) in
        let%bind (), semi_sp =
          spanned_bind (get_specific parser Token.Semicolon)
        in
        let full_sp = Spanned.Span.union name_sp semi_sp in
        return (Ast.Stmt.(Let {name; is_mut; ty; expr}), full_sp)
      in
      let%bind blk = parse_block_no_open parser in
      return (Ast.Expr.Block.with_stmt blk stmt)
  | _ -> (
      match%bind spanned_bind (maybe_parse_expression parser) with
      | Some e, sp -> (
          match%bind spanned_bind (next_token parser) with
          | Token.Close_brace, _ ->
              return (Ast.Expr.Block.Block {stmts = []; expr = Some (e, sp)})
          | Token.Semicolon, semi_sp ->
              let%bind blk = parse_block_no_open parser in
              let full_sp = Spanned.Span.union sp semi_sp in
              let stmt = (Ast.Stmt.Expression (e, sp), full_sp) in
              return (Ast.Expr.Block.with_stmt blk stmt)
          | tok, _ -> unexpected tok Error.Expected.Statement_end )
      | None, _ -> (
          match%bind next_token parser with
          | Token.Close_brace ->
              return (Ast.Expr.Block.Block {stmts = []; expr = None})
          | tok -> unexpected tok Error.Expected.Expression ) )

and parse_block (parser : t) : Ast.Expr.Block.t result =
  let%bind () = get_specific parser Token.Open_brace in
  parse_block_no_open parser

let parse_infix_group (parser : t) : Ast.Infix_group.t result =
  let module I = Ast.Infix_group in
  let rec helper name associativity precedence =
    match%bind next_token parser with
    | Token.Close_brace ->
        let associativity =
          match associativity with None -> I.Assoc_none | Some a -> a
        in
        return (I.Infix_group {name; associativity; precedence})
    | Token.Identifier id when Nfc_string.equal id Ctxt_keyword.precedence ->
        let%bind relation =
          match%bind next_token parser with
          | Token.Operator id when Nfc_string.equal id Ctxt_keyword.less ->
              let%bind other = spanned_bind (get_identifier parser) in
              return (I.Less other)
          | Token.Operator id when Nfc_string.equal id Ctxt_keyword.greater ->
              failwith "greater precedence not yet supported"
          | tok -> unexpected tok Error.Expected.Precedence
        in
        let%bind () = get_specific parser Token.Semicolon in
        helper name associativity (relation :: precedence)
    | Token.Identifier id when Nfc_string.equal id Ctxt_keyword.associativity
      ->
        let%bind () = get_specific parser Ctxt_keyword.equal_tok in
        let%bind associativity' =
          match%bind next_token parser with
          | Token.Identifier id
            when Nfc_string.equal id Ctxt_keyword.assoc_start ->
              return I.Assoc_start
          | Token.Identifier id when Nfc_string.equal id Ctxt_keyword.assoc_end
            ->
              return I.Assoc_end
          | Token.Identifier id
            when Nfc_string.equal id Ctxt_keyword.assoc_none ->
              return I.Assoc_none
          | tok -> unexpected tok Error.Expected.Associativity
        in
        let%bind () = get_specific parser Token.Semicolon in
        if Option.is_none associativity
        then helper name (Some associativity') precedence
        else
          let name, _ = name in
          return_err (Error.Associativity_defined_twice name)
    | tok -> unexpected tok Error.Expected.Infix_group_member
  in
  let%bind name = spanned_bind (get_identifier parser) in
  let%bind () = get_specific parser Token.Open_brace in
  helper name None []

let parse_infix_declaration (parser : t) : Ast.Infix_declaration.t result =
  let%bind () = get_specific parser Token.Open_paren in
  let%bind name = spanned_bind (get_infix_operator parser) in
  let%bind () = get_specific parser Token.Close_paren in
  let%bind () = get_specific parser Token.Colon in
  let%bind group = spanned_bind (get_identifier parser) in
  let%bind () = get_specific parser Token.Semicolon in
  return (Ast.Infix_declaration.Infix_declaration {name; group})

let parse_func (parser : t) : Ast.Func.t result =
  let%bind name = get_name parser in
  let%bind params = parse_parameter_list parser in
  let%bind ret_ty = parse_return_type parser in
  let%bind body = spanned_bind (parse_block parser) in
  return (Ast.Func.Func {name; params; ret_ty; body})

let parse_item (parser : t) : Item.t option result =
  match%bind next_token parser with
  | Token.Keyword_func ->
      let%bind func = parse_func parser in
      return (Some (Item.Func func))
  | Token.Keyword_infix -> (
      match%bind maybe_get_specific parser Token.Keyword_group with
      | Some () ->
          let%bind infix_group = parse_infix_group parser in
          return (Some (Item.Infix_group infix_group))
      | None ->
          let%bind infix_decl = parse_infix_declaration parser in
          return (Some (Item.Infix_declaration infix_decl)) )
  | Token.Keyword_record ->
      let%bind def = parse_simple_type ~kind:Type.Data.Record parser in
      return (Some (Item.Type_definition def))
  | Token.Keyword_variant ->
      let%bind def = parse_simple_type ~kind:Type.Data.Variant parser in
      return (Some (Item.Type_definition def))
  | Token.Keyword_type ->
      let%bind name = spanned_bind (get_identifier parser) in
      let%bind kind =
        match%bind maybe_get_specific parser Ctxt_keyword.equal_tok with
        | Some () ->
            let%bind ty = parse_value_type parser in
            let%bind () = get_specific parser Token.Semicolon in
            return (Type.Definition.Alias ty)
        | None ->
            let%bind () = get_specific parser Token.Open_brace in
            let%bind kind =
              match%bind next_token parser with
              | Token.Keyword_record -> return Type.Data.Record
              | Token.Keyword_variant -> return Type.Data.Variant
              | tok -> unexpected tok Error.Expected.Data
            in
            let%bind () = get_specific parser Token.Keyword_data in
            let%bind members = parse_data_members parser in
            let%bind () = get_specific parser Token.Close_brace in
            let data = Type.Data.Data {kind; members} in
            return (Type.Definition.User_defined {data})
      in
      let def = Type.Definition.Definition {name; kind} in
      return (Some (Item.Type_definition def))
  | Token.Eof -> return None
  | tok -> unexpected tok Error.Expected.Item_declarator

let parse_program (parser : t) : (Ast.t, Error.t) Spanned.Result.t =
  let rec helper parser =
    let%bind item, sp = spanned_bind (parse_item parser) in
    match item with
    | None ->
        return
          (Ast.Ast {funcs = []; infix_groups = []; infix_decls = []; types = []})
    | Some item -> (
        let%bind rest = helper parser in
        match item with
        | Item.Func func -> return (Ast.with_func rest (func, sp))
        | Item.Infix_group group ->
            return (Ast.with_infix_group rest (group, sp))
        | Item.Infix_declaration decl ->
            return (Ast.with_infix_decl rest (decl, sp))
        | Item.Type_definition def -> return (Ast.with_type rest (def, sp)) )
  in
  helper parser

let parse program =
  let lexer = Lexer.make program in
  let parser = Parser {lexer; peek = None} in
  parse_program parser