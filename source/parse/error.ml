module Expected = struct
  type t =
    | Specific of Token.t
    | Item_declarator
    | Identifier
    | Variable_decl
    | Type
    | Data
    | Expression
    | Expression_follow
    | Path_expression

  let to_string = function
    | Specific tok -> Token.to_string tok
    | Item_declarator -> "`func`, `type`, or `alias`"
    | Identifier -> "an identifier"
    | Variable_decl -> "a variable declaration"
    | Type -> "the start of a type"
    | Data -> "either `record` or `variant`"
    | Expression -> "the start of an expression"
    | Expression_follow ->
        "an operator, semicolon, comma, dot, or closing brace (`}`, `)`)"
    | Path_expression ->
        "the continuation of a path, or an identifier or record literal"
end

type t =
  | Unclosed_comment
  | Operator_including_comment_token of string
  | Malformed_number_literal
  | Reserved_token of string
  | Unrecognized_character of char
  | Unexpected_token of (Expected.t * Token.t)

let to_string = function
  | Malformed_number_literal -> "malformed number literal"
  | Operator_including_comment_token s ->
      Printf.sprintf "operator `%s` includes a sequence of comment characters"
        s
  | Reserved_token tok -> Printf.sprintf "reserved token: `%s`" tok
  | Unrecognized_character ch ->
      Printf.sprintf "unrecognized character: `%c` (%d)" ch (Char.to_int ch)
  | Unclosed_comment -> "unclosed comment"
  | Unexpected_token (exp, tok) ->
      String.concat
        ["expected: "; Expected.to_string exp; ", found: "; Token.to_string tok]
