include Types.Token

module Keyword = struct
  include Types.Token_Keyword
  module Contextual = Types.Token_Keyword_Contextual

  let equal lhs rhs =
    match (lhs, rhs) with
    | Match, Match -> true
    | If, If -> true
    | Else, Else -> true
    | Infix, Infix -> true
    | Group, Group -> true
    | Func, Func -> true
    | Type, Type -> true
    | Data, Data -> true
    | Record, Record -> true
    | Variant, Variant -> true
    | Alias, Alias -> true
    | Let, Let -> true
    | Mut, Mut -> true
    | Builtin, Builtin -> true
    | Underscore, Underscore -> true
    | _ -> false

  let to_string k ~lang = Lang.keyword_to_string k ~lang
end

module Attribute = struct
  include Types.Token_Attribute

  let equal lhs rhs =
    match (lhs, rhs) with Entrypoint, Entrypoint -> true

  let to_string a ~lang = Lang.attribute_to_string ~lang a
end

let equal lhs rhs =
  match (lhs, rhs) with
  | Open_paren, Open_paren -> true
  | Close_paren, Close_paren -> true
  | Open_brace, Open_brace -> true
  | Close_brace, Close_brace -> true
  | Open_square, Open_square -> true
  | Close_square, Close_square -> true
  | Semicolon, Semicolon -> true
  | Dot, Dot -> true
  | Comma, Comma -> true
  | Integer_literal i1, Integer_literal i2 -> i1 = i2
  | Assign, Assign -> true
  | Arrow, Arrow -> true
  | Thicc_arrow, Thicc_arrow -> true
  | Colon, Colon -> true
  | Double_colon, Double_colon -> true
  | Operator id1, Operator id2 -> Nfc_string.equal id1 id2
  | Identifier id1, Identifier id2 -> Nfc_string.equal id1 id2
  | Keyword k1, Keyword k2 -> Keyword.equal k1 k2
  | Eof, Eof -> true
  | _ -> false

let to_string t ~lang =
  match t with
  | Open_paren -> "open paren `(`"
  | Close_paren -> "close paren `)`"
  | Open_brace -> "open brace `{`"
  | Close_brace -> "close brace `}`"
  | Open_square -> "open square `[`"
  | Close_square -> "close square `]`"
  | Attribute -> "attribute `@`"
  | Semicolon -> "semicolon `;`"
  | Dot -> "dot `.`"
  | Comma -> "comma `,`"
  | Integer_literal i -> Printf.sprintf "int literal: `%d`" i
  | Assign -> "assign `<-`"
  | Arrow -> "arrow `->`"
  | Thicc_arrow -> "thick arrow `=>`"
  | Colon -> "colon `:`"
  | Double_colon -> "double colon `::`"
  | Operator ident -> Printf.sprintf "operator: `%s`" (ident :> string)
  | Identifier_operator ident ->
      Printf.sprintf "operator: `\\%s`" (ident :> string)
  | Identifier ident ->
      Printf.sprintf "identifier: `%s`" (ident :> string)
  | Keyword kw ->
      Printf.sprintf "keyword `%s`" (Keyword.to_string kw ~lang)
  | Eof -> "end of file"
