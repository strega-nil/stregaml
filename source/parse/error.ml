module Expected = struct
  include Types.Error_Expected

  let to_string = function
    | Specific tok -> Token.to_string tok
    | Item_declarator -> "`func`, `type`, or `association`"
    | Operator -> "an operator identifier"
    | Identifier -> "an identifier"
    | Variable_decl -> "a variable declaration"
    | Type -> "the start of a type"
    | Data -> "either `record` or `variant`"
    | Association -> "either `:` or `=`"
    | Direction -> "either `left`, `right`, or `none`"
    | Precedence -> "either `<`, `=`, or `>`"
    | Expression -> "the start of an expression"
    | Expression_follow ->
        "an operator, semicolon, comma, dot, or closing brace (`}`, `)`)"
    | Statement_end -> "a closing brace or semicolon"
    | Path_expression ->
        "the continuation of a path, or an identifier or record literal"
end

include Types.Error

let to_string = function
  | Malformed_input s -> String.concat ["malformed utf-8: `"; s; "'"]
  | Malformed_number_literal -> "malformed number literal"
  | Operator_including_comment_token s ->
      Printf.sprintf "operator `%s` includes a sequence of comment characters"
        (s :> string)
  | Unrecognized_direction s ->
      Printf.sprintf
        {|unrecognized association direction `%s`
  valid association directions are: left, right, none|}
        (s :> string)
  | Reserved_token tok -> Printf.sprintf "reserved token: `%s`" (tok :> string)
  | Unrecognized_character ch ->
      Printf.sprintf "unrecognized character: `%s` (%d)"
        (Ident.uchar_to_string ch) (Uchar.to_scalar ch)
  | Unclosed_comment -> "unclosed comment"
  | Unexpected_token (exp, tok) ->
      String.concat
        ["expected: "; Expected.to_string exp; ", found: "; Token.to_string tok]
