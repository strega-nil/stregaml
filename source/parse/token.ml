module Keyword = struct
  type t = True | False | If | Else | Func | Type | Data | Alias | Underscore

  let equal lhs rhs =
    match (lhs, rhs) with
    | True, True -> true
    | False, False -> true
    | If, If -> true
    | Else, Else -> true
    | Func, Func -> true
    | Type, Type -> true
    | Data, Data -> true
    | Alias, Alias -> true
    | Underscore, Underscore -> true
    | _ -> false


  let to_string = function
    | True -> "true"
    | False -> "false"
    | If -> "if"
    | Else -> "else"
    | Func -> "func"
    | Type -> "type"
    | Data -> "data"
    | Alias -> "alias"
    | Underscore -> "_"
end

type t =
  | Open_paren
  | Close_paren
  | Open_brace
  | Close_brace
  | Open_record
  | Close_record
  | Keyword of Keyword.t
  | Identifier of string
  | Operator of string
  | Integer_literal of int
  | Arrow
  | Colon
  | Double_colon
  | Equals
  | Semicolon
  | Dot
  | Comma
  | Eof

let equal lhs rhs =
  match (lhs, rhs) with
  | Open_paren, Open_paren -> true
  | Close_paren, Close_paren -> true
  | Open_brace, Open_brace -> true
  | Close_brace, Close_brace -> true
  | Open_record, Open_record -> true
  | Close_record, Close_record -> true
  | Keyword kw1, Keyword kw2 -> Keyword.equal kw1 kw2
  | Identifier id1, Identifier id2 -> String.equal id1 id2
  | Operator op1, Operator op2 -> String.equal op1 op2
  | Integer_literal i1, Integer_literal i2 -> i1 = i2
  | Arrow, Arrow -> true
  | Colon, Colon -> true
  | Double_colon, Double_colon -> true
  | Equals, Equals -> true
  | Semicolon, Semicolon -> true
  | Dot, Dot -> true
  | Comma, Comma -> true
  | Eof, Eof -> true
  | _ -> false


let to_string = function
  | Open_paren -> "open paren `(`"
  | Close_paren -> "close paren `)`"
  | Open_brace -> "open brace `{`"
  | Close_brace -> "close brace `}`"
  | Open_record -> "open record `{|`"
  | Close_record -> "close record `|}`"
  | Keyword kw -> Printf.sprintf "keyword: `%s`" (Keyword.to_string kw)
  | Operator op -> Printf.sprintf "operator: `%s`" op
  | Identifier id -> Printf.sprintf "identifier: `%s`" id
  | Integer_literal i -> Printf.sprintf "int literal: `%d`" i
  | Arrow -> "arrow `->`"
  | Colon -> "colon `:`"
  | Equals -> "equals `=`"
  | Semicolon -> "semicolon `;`"
  | Double_colon -> "double colon `::`"
  | Comma -> "comma `,`"
  | Dot -> "dot `.`"
  | Eof -> "end of file"
