module Expected : sig
  type t =
    | Specific of Token.t
    | Item_declarator
    | Identifier
    | Variable_decl
    | Type
    | Data
    | Expression
    | Expression_follow
    | Statement_end
    | Path_expression

  val to_string : t -> string
end

type t =
  | Unclosed_comment
  | Operator_including_comment_token of string
  | Malformed_number_literal
  | Reserved_token of string
  | Unrecognized_character of char
  | Unexpected_token of (Expected.t * Token.t)

val to_string : t -> string
