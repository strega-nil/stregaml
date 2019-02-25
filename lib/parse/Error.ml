module Expected = struct
  include Types.Error_Expected

  let to_string e ~lang =
    let module Keyword = Types.Token_Keyword in
    match e with
    | Specific tok -> Token.to_string tok ~lang
    | Item_declarator -> "an item declarator"
    | Operator -> "an operator identifier"
    | Identifier -> "an identifier"
    | Name ->
        "a name (either an identifier, or a paren-surrounded operator)"
    | Variable_decl -> "a variable declaration"
    | Value_type -> "a value type"
    | Place ->
        Printf.sprintf
          "`%s` or `%s`"
          (Lang.keyword_to_string ~lang Keyword.Ref)
          (Lang.keyword_to_string ~lang Keyword.Mut)
    | Type -> "the start of a type"
    | Data -> "either `record` or `variant`"
    | Associativity -> "either `start, `end`, or `none`"
    | Precedence -> "either `<` or `>`"
    | Infix_group_member -> "either `precedence` or `associativity`"
    | Infix_follow -> "either `group`, or an open parenthesis"
    | Expression -> "the start of an expression"
    | Expression_follow ->
        "an operator, semicolon, comma, dot, or closing brace (`}`, `)`)"
    | Statement_end -> "a closing brace or semicolon"
    | Path_expression ->
        "the continuation of a path, or an identifier or record literal"
    | Path -> "the continuation of a path, or an identifier"
    | Match_arm -> "either a match arm, or the end of the match block"
end

include Types.Error

let to_string e ~lang =
  match e with
  | Malformed_input s -> String.concat ["malformed utf-8: `"; s; "'"]
  | Malformed_number_literal -> "malformed number literal"
  | Identifier_operator_is_keyword tok ->
      Printf.sprintf
        "identifier operator must be a non-keyword identifier (found `%s`)"
        (Token.to_string ~lang tok)
  | Identifier_operator_start_without_ident None ->
      "`\\` followed by EOF; expected an identifier"
  | Identifier_operator_start_without_ident (Some ch) ->
      Printf.sprintf
        "`\\` followed by a character that cannot start an identifier: `%s` (`%d`)"
        (Nfc_string.uchar_to_string ch)
        (Uchar.to_scalar ch)
  | Associativity_defined_twice name ->
      Printf.sprintf
        "Attempted to define the associativity of infix group `%s` twice"
        (name :> string)
  | Reserved_token tok ->
      Printf.sprintf "reserved token: `%s`" (tok :> string)
  | Unrecognized_character ch ->
      Printf.sprintf "unrecognized character: `%s` (%d)"
        (Nfc_string.uchar_to_string ch)
        (Uchar.to_scalar ch)
  | Unclosed_comment -> "unclosed comment"
  | Unexpected_token (exp, tok) ->
      String.concat
        [ "expected: "
        ; Expected.to_string ~lang exp
        ; ", found: "
        ; Token.to_string ~lang tok ]
