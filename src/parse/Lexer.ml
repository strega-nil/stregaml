open! Types.Pervasives

exception Bug_lexer of string

type t = Lexer : {decoder: Uutf.decoder; mutable peek: Uchar.t option} -> t

let decoder (Lexer r) = r.decoder

let make ch =
  let decoder =
    Uutf.decoder
      ~nln:(`NLF (Uchar.of_char '\n'))
      ~encoding:`UTF_8 (`Channel ch)
  in
  Lexer {decoder; peek= None}

(* the actual lexer functions *)
let ( =~ ) uch ch = Uchar.equal uch (Uchar.of_char ch)

let ( <=~ ) uch ch = Uchar.compare uch (Uchar.of_char ch) <= 0

let ( >=~ ) uch ch = Uchar.compare uch (Uchar.of_char ch) >= 0

let is_whitespace = Uucp.White.is_white_space

(*
  note: this will eventually support unicode numerals,
  but that will be added when we add
  non-english keywords and non-english error messages.
*)
let is_number_start ch =
  match Uchar.to_char ch with None -> false | Some ch -> Char.is_digit ch

let is_number_continue ch base =
  match base with
  | 2 -> ch =~ '0' || ch =~ '1'
  | 8 -> ch >=~ '0' && ch <=~ '7'
  | 10 -> ch >=~ '0' && ch <=~ '9'
  | 16 ->
      (ch >=~ '0' && ch <=~ '9')
      || (ch >=~ 'a' && ch <=~ 'f')
      || (ch >=~ 'A' && ch <=~ 'F')
  | base -> raise (Bug_lexer ("Invalid base: " ^ Int.to_string base))

(*
  <identifier-start> :=
    | <character _>
    | xid-start
  <identifier-continue> :=
    | <character ->
    | <character '>
    | xid-continue

  <identifier> = {<identifier-start> <identifier-continue>*} / <keyword>
*)
let is_ident_start ch = ch =~ '_' || Uucp.Id.is_xid_start ch

let is_ident_continue_no_prime ch = ch =~ '-' || Uucp.Id.is_xid_continue ch

let is_ident_continue ch = ch =~ '\'' || is_ident_continue_no_prime ch

(*
  <operator-start> :=
    | <unicode-symbol-math> (* includes =, <, > *)
    | (* common operators of other languages *)
      | <character :>
      | <character %>
      | <character +>
      | <character ->
      | <character *>
      | <character />
      | <character \>
      | <character ~>
      | <character &>
      | <character |>
      | <character ˆ>
      | <character !>
      | <character @>
  <operator-continue> :=
    | <operator-start>
    | <character '>

  <operator> :=
    | { <operator-start> <operator-continue>* } / <operator-keyword>
    | { <character \> <identifier> }
*)
let is_operator_start ch =
  let is_valid_ascii ch =
    let ch = Option.value ~default:(Char.unsafe_of_int 0) (Uchar.to_char ch) in
    match ch with
    | ':' -> true
    | '%' -> true
    | '+' -> true
    | '-' -> true
    | '*' -> true
    | '/' -> true
    | '~' -> true
    | '&' -> true
    | '|' -> true
    | '^' -> true
    | '!' -> true
    | '@' -> true
    | '=' -> true (* these three are included for optimization purposes *)
    | '<' -> true
    | '>' -> true
    | _ -> false
  in
  if is_valid_ascii ch then true
  else match Uucp.Gc.general_category ch with `Sm -> true | _ -> false

let is_operator_continue ch = ch =~ '\'' || is_operator_start ch

let current_span lex =
  let open Spanned.Span in
  let line = Uutf.decoder_line (decoder lex) in
  let col = Uutf.decoder_col (decoder lex) in
  Span
    {start_line= line; start_column= col; end_line= line; end_column= col + 1}

let peek_ch (Lexer r as lex) : Uchar.t option result =
  match r.peek with
  | Some uch -> (Ok (Some uch), current_span lex)
  | None -> (
    match Uutf.decode r.decoder with
    | `Await -> assert false
    | `Uchar uch ->
        let uch = Some uch in
        r.peek <- uch ;
        (Ok uch, current_span lex)
    | `End -> (Ok None, Spanned.Span.made_up)
    | `Malformed s -> (Error (Error.Malformed_input s), current_span lex) )

let next_ch (Lexer r as lex) =
  match peek_ch lex with
  | (Result.Ok _, _) as o ->
      r.peek <- None ;
      o
  | (Result.Error _, _) as e -> e

let eat_ch (Lexer lex) =
  match lex.peek with
  | Some _ -> lex.peek <- None
  | None ->
      failwith "internal error: lexer eat_ch without knowing the character"

let rec eat_whitespace lex =
  let%bind ch = peek_ch lex in
  match ch with
  | Some ch when is_whitespace ch -> eat_ch lex ; eat_whitespace lex
  | Some _ | None -> return ()

(* note:
  these three functions _do not_ do special checking on the first character
  to call these functions without guaranteeing that the first character is valid
  is an _error_
*)
let lex_ident lex =
  let rec helper lst =
    let%bind ch = peek_ch lex in
    match ch with
    | Some ch when is_ident_continue ch ->
        eat_ch lex ;
        helper (ch :: lst)
    | Some _ | None -> (
        let ident = Nfc_string.of_uchar_list (List.rev lst) in
        match (ident :> string) with
        | "" -> failwith "internal lexer error"
        | "true" -> return Token.Keyword_true
        | "false" -> return Token.Keyword_false
        | "match" -> return Token.Keyword_match
        | "if" -> return Token.Keyword_if
        | "else" -> return Token.Keyword_else
        | "infix" -> return Token.Keyword_infix
        | "prefix" -> return Token.Keyword_prefix
        | "group" -> return Token.Keyword_group
        | "func" -> return Token.Keyword_func
        | "type" -> return Token.Keyword_type
        | "data" -> return Token.Keyword_data
        | "record" -> return Token.Keyword_record
        | "alias" -> return Token.Keyword_alias
        | "let" -> return Token.Keyword_let
        | "mut" -> return Token.Keyword_mut
        | "__builtin" -> return Token.Keyword_builtin
        | "_" -> return Token.Keyword_underscore
        | "variant" -> return Token.Keyword_variant
        | "opaque" -> return_err (Error.Reserved_token ident)
        | "public" -> return_err (Error.Reserved_token ident)
        | _ -> return (Token.Identifier ident) )
  in
  helper []

let lex_number lex =
  let open! Char.O in
  let%bind fst =
    match%bind next_ch lex with
    | Some ch when is_number_start ch -> return ch
    | _ -> failwith "internal lexer error"
  in
  let%bind base, buff =
    if Uchar.equal fst (Uchar.of_char '0') then
      let%bind ch = peek_ch lex in
      match Option.bind ~f:Uchar.to_char ch with
      | Some 'x' ->
          eat_ch lex ;
          return (16, [])
      | Some 'o' ->
          eat_ch lex ;
          return (8, [])
      | Some 'b' ->
          eat_ch lex ;
          return (2, [])
      | Some _ | None -> return (10, [fst])
    else return (10, [fst])
  in
  let rec helper lst quote_allowed =
    let%bind ch = peek_ch lex in
    match ch with
    | Some ch when is_number_continue ch base ->
        eat_ch lex ;
        helper (ch :: lst) true
    | Some ch when ch =~ '\'' ->
        if quote_allowed then ( eat_ch lex ; helper lst false )
        else return_err Error.Malformed_number_literal
    | Some _ | None ->
        let char_to_int ch =
          match Uchar.to_char ch with
          | None -> raise (Bug_lexer "malformed number literal")
          | Some ch ->
              if ch >= '0' && ch <= '9' then Char.to_int ch - Char.to_int '0'
              else if ch >= 'a' && ch <= 'f' then
                Char.to_int ch - Char.to_int 'a' + 10
              else if ch >= 'A' && ch <= 'F' then
                Char.to_int ch - Char.to_int 'A' + 10
              else raise (Bug_lexer "malformed number literal")
        in
        (* TODO(ubsan): fix overflow *)
        let rec to_int pow acc = function
          | [] -> acc
          | ch :: xs ->
              let cur = char_to_int ch * pow in
              to_int (pow * base) (acc + cur) xs
        in
        return (Token.Integer_literal (to_int 1 0 lst))
  in
  helper buff true

let lex_operator lex =
  let rec helper lst =
    let%bind ch = peek_ch lex in
    match ch with
    | Some ch when is_operator_continue ch ->
        eat_ch lex ;
        helper (ch :: lst)
    | Some _ | None -> (
        let ident = Nfc_string.of_uchar_list (List.rev lst) in
        match (ident :> string) with
        | "" -> failwith "internal lexer error"
        | ":" -> return Token.Colon
        | "<-" -> return Token.Assign
        | "->" -> return Token.Arrow
        | "=>" -> return Token.Thicc_arrow
        | "::" -> return Token.Double_colon
        | _ -> return (Token.Operator ident) )
  in
  helper []

let rec next_token lex =
  let%bind () = eat_whitespace lex in
  let single_char_tok tok = eat_ch lex ; return tok in
  let%bind ch = peek_ch lex in
  match ch with
  | Some ch when ch =~ '(' -> single_char_tok Token.Open_paren
  | Some ch when ch =~ ')' -> single_char_tok Token.Close_paren
  | Some ch when ch =~ '{' -> single_char_tok Token.Open_brace
  | Some ch when ch =~ '}' -> single_char_tok Token.Close_brace
  | Some ch when ch =~ '[' -> single_char_tok Token.Open_square
  | Some ch when ch =~ ']' -> single_char_tok Token.Close_square
  | Some ch when ch =~ ';' -> single_char_tok Token.Semicolon
  | Some ch when ch =~ ',' -> single_char_tok Token.Comma
  | Some ch when ch =~ '.' -> single_char_tok Token.Dot
  | Some ch when ch =~ '#' -> (
      eat_ch lex ;
      let rec block_comment () =
        let rec eat_the_things () =
          let%bind ch = next_ch lex in
          match ch with
          | Some ch when ch =~ '}' -> return ()
          | Some ch when ch =~ '{' ->
              let%bind () = block_comment () in
              eat_the_things ()
          | Some _ -> eat_the_things ()
          | None -> return_err Error.Unclosed_comment
        in
        eat_the_things ()
      in
      let line_comment () =
        let rec eat_the_things () =
          let%bind ch = next_ch lex in
          match ch with
          | Some ch when ch =~ '\n' || ch =~ '\r' -> return ()
          | None -> return ()
          | Some _ -> eat_the_things ()
        in
        eat_the_things ()
      in
      match%bind peek_ch lex with
      | Some ch when ch =~ '{' ->
          eat_ch lex ;
          let%bind () = block_comment () in
          next_token lex
      | Some _ ->
          let%bind () = line_comment () in
          next_token lex
      | None -> return Token.Eof )
  | Some ch when ch =~ '\\' -> (
      eat_ch lex ;
      match%bind peek_ch lex with
      | Some ch when is_ident_start ch -> (
          let%bind ident = lex_ident lex in
          match ident with
          | Token.Identifier id -> return (Token.Identifier_operator id)
          | tok -> return_err (Error.Identifier_operator_is_keyword tok) )
      | ch -> return_err (Error.Identifier_operator_start_without_ident ch) )
  | Some ch when is_ident_start ch -> lex_ident lex
  | Some ch when is_operator_start ch -> lex_operator lex
  | Some ch when is_number_start ch -> lex_number lex
  | Some ch -> return_err (Error.Unrecognized_character ch)
  | None -> return Token.Eof
