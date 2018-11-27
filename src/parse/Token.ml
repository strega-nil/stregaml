include Types.Token

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
  | Thick_arrow, Thick_arrow -> true
  | Colon, Colon -> true
  | Double_colon, Double_colon -> true
  | Operator id1, Operator id2 -> Nfc_string.equal id1 id2
  | Identifier id1, Identifier id2 -> Nfc_string.equal id1 id2
  | Keyword_true, Keyword_true -> true
  | Keyword_false, Keyword_false -> true
  | Keyword_match, Keyword_match -> true
  | Keyword_if, Keyword_if -> true
  | Keyword_else, Keyword_else -> true
  | Keyword_infix, Keyword_infix -> true
  | Keyword_group, Keyword_group -> true
  | Keyword_func, Keyword_func -> true
  | Keyword_type, Keyword_type -> true
  | Keyword_data, Keyword_data -> true
  | Keyword_record, Keyword_record -> true
  | Keyword_variant, Keyword_variant -> true
  | Keyword_alias, Keyword_alias -> true
  | Keyword_let, Keyword_let -> true
  | Keyword_mut, Keyword_mut -> true
  | Keyword_builtin, Keyword_builtin -> true
  | Keyword_underscore, Keyword_underscore -> true
  | Eof, Eof -> true
  | _ -> false

let to_string = function
  | Open_paren -> "open paren `(`"
  | Close_paren -> "close paren `)`"
  | Open_brace -> "open brace `{`"
  | Close_brace -> "close brace `}`"
  | Open_square -> "open square `[`"
  | Close_square -> "close square `]`"
  | Semicolon -> "semicolon `;`"
  | Dot -> "dot `.`"
  | Comma -> "comma `,`"
  | Integer_literal i -> Printf.sprintf "int literal: `%d`" i
  | Assign -> "assign `<-`"
  | Arrow -> "arrow `->`"
  | Thick_arrow -> "thick arrow `=>`"
  | Colon -> "colon `:`"
  | Double_colon -> "double colon `::`"
  | Operator ident -> Printf.sprintf "infix operator: `%s`" (ident :> string)
  | Identifier ident -> Printf.sprintf "identifier: `%s`" (ident :> string)
  | Keyword_true -> "keyword `true`"
  | Keyword_false -> "keyword `false`"
  | Keyword_match -> "keyword `match`"
  | Keyword_if -> "keyword `if`"
  | Keyword_else -> "keyword `else`"
  | Keyword_infix -> "keyword `infix`"
  | Keyword_prefix -> "keyword `prefix`"
  | Keyword_group -> "keyword `group`"
  | Keyword_func -> "keyword `func`"
  | Keyword_type -> "keyword `type`"
  | Keyword_data -> "keyword `data`"
  | Keyword_record -> "keyword `record`"
  | Keyword_variant -> "keyword `variant`"
  | Keyword_alias -> "keyword `alias`"
  | Keyword_let -> "keyword `let`"
  | Keyword_mut -> "keyword `mut`"
  | Keyword_builtin -> "keyword `__builtin`"
  | Keyword_underscore -> "keyword `_`"
  | Eof -> "end of file"
