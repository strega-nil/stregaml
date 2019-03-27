open! Types.Pervasives

module type Language = Types.Language

exception Bug_lexer of string

(*
  note: requires 2 characters of lookahead,
  since we want to support both:
    123,456
  and
    123, 456
  when lexing a number literal,
  a separator is treated as either a
  member or non-member of the literal,
  based on whether the character _after_
  the separator is a number-continue.

  In other words:
    lex(
      <a : number-start>
      <b : number-separator>
      <c : number-continue>
      ...
    )
    = <abc : number>
  while
    lex(
      <a : number-start>
      <b : number-separator>
      <c : not number-continue>
      ...
    )
    = <a : number> lex(<b><c>...)
*)
type peeked_char = Uchar.t Spanned.t

type t =
  | Lexer :
      { decoder : Uutf.decoder
      ; mutable peek : (peeked_char * peeked_char option) option
      ; lang : (module Language) }
      -> t

let decoder (Lexer r) = r.decoder

let lang (Lexer r) = r.lang

let make ch ~lang =
  let decoder =
    Uutf.decoder
      ~nln:(`NLF (Uchar.of_char '\n'))
      ~encoding:`UTF_8 (`Channel ch)
  in
  Lexer {decoder; peek = None; lang}

(* the actual lexer functions *)
let ( =~ ) uch ch = Uchar.equal uch (Uchar.of_char ch)

let ( <=~ ) uch ch = Uchar.compare uch (Uchar.of_char ch) <= 0

let ( >=~ ) uch ch = Uchar.compare uch (Uchar.of_char ch) >= 0

let is_whitespace = Uucp.White.is_white_space

(*
  we are going to wait for someone who knows what a
  person who uses non-western numerals to have Opinions
  about what to do for non-western numbers.

  <number-start> :=
    <character 0>
    <character 1>
    <character 2>
    <character 3>
    <character 4>
    <character 5>
    <character 6>
    <character 7>
    <character 8>
    <character 9>

  <number-continue 2> :=
    <character 0>
    <character 1>

  <number-continue 8> :=
    <number-continue 2>
    <character 2>
    <character 3>
    <character 4>
    <character 5>
    <character 6>
    <character 7>

  <number-continue 10> :=
    <number-continue 8>
    <character 8>
    <character 9>

  <number-continue 16> :=
    <number-continue 10>
    <character A>
    <character B>
    <character C>
    <character D>
    <character E>
    <character F>
    <character a>
    <character b>
    <character c>
    <character d>
    <character e>
    <character f>

  <number-separator> :=
    <character ,>
    <character .>
*)
let is_number_start ch =
  match Uchar.to_char ch with
  | None -> false
  | Some ch -> Char.is_digit ch

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

let is_ident_continue_no_prime ch =
  ch =~ '-' || Uucp.Id.is_xid_continue ch

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
    let ch =
      Option.value ~default:(Char.unsafe_of_int 0) (Uchar.to_char ch)
    in
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
    | '=' ->
        true (* these three are included for optimization purposes *)
    | '<' -> true
    | '>' -> true
    | _ -> false
  in
  if is_valid_ascii ch
  then true
  else
    match Uucp.Gc.general_category ch with `Sm -> true | _ -> false

let is_operator_continue ch = ch =~ '\'' || is_operator_start ch

let current_span lex =
  let open Spanned.Span in
  let line = Uutf.decoder_line (decoder lex) in
  let col = Uutf.decoder_col (decoder lex) in
  Span
    { start_line = line
    ; start_column = col
    ; end_line = line
    ; end_column = col + 1 }

let get_char_from_buffer (Lexer r as lex) : Uchar.t option result =
  match Uutf.decode r.decoder with
  | `Await -> assert false
  | `Uchar uch -> (Ok (Some uch), current_span lex)
  | `End -> (Ok None, Spanned.Span.made_up)
  | `Malformed s -> (Error (Error.Malformed_input s), current_span lex)

let peek_ch (Lexer r as lex) : Uchar.t option result =
  match r.peek with
  | Some ((fch, sp), _) -> (Ok (Some fch), sp)
  | None -> (
      match%bind spanned_bind (get_char_from_buffer lex) with
      | Some fch, sp ->
          r.peek <- Some ((fch, sp), None) ;
          (Ok (Some fch), sp)
      | None, sp -> (Ok None, sp) )

let peek_ch2 (Lexer r as lex) : Uchar.t option result =
  match r.peek with
  | Some (_, Some (sch, sp)) -> (Ok (Some sch), sp)
  | Some ((fch, sp1), None) -> (
      match%bind spanned_bind (get_char_from_buffer lex) with
      | Some sch, sp2 ->
          r.peek <- Some ((fch, sp1), Some (sch, sp2)) ;
          (Ok (Some sch), sp2)
      | None, sp2 -> (Ok None, sp2) )
  | None -> (
      match%bind spanned_bind (get_char_from_buffer lex) with
      | None, sp -> (Ok None, sp)
      | Some fch, sp1 -> (
          match%bind spanned_bind (get_char_from_buffer lex) with
          | Some sch, sp2 ->
              r.peek <- Some ((fch, sp1), Some (sch, sp2)) ;
              (Ok (Some sch), sp2)
          | None, sp2 ->
              r.peek <- Some ((fch, sp1), None) ;
              (Ok None, sp2) ) )

let eat_ch (Lexer r) =
  match r.peek with
  | Some (_, Some (sch, sp)) -> r.peek <- Some ((sch, sp), None)
  | Some (_, None) -> r.peek <- None
  | None ->
      failwith
        "internal error: lexer eat_ch without knowing the character"

let next_ch lex =
  match peek_ch lex with
  | (Result.Ok _, _) as o -> eat_ch lex ; o
  | (Result.Error _, _) as e -> e

(*
  note : in order to not get whitespace spans included in tokens,
  this returns a nothing span.
*)
let rec eat_whitespace lex =
  match peek_ch lex with
  | Ok (Some ch), _ when is_whitespace ch ->
      eat_ch lex ; eat_whitespace lex
  | Ok _, _ -> return ()
  | Error e, sp -> (Error e, sp)

(* note:
  these three functions _do not_ do special checking on the first
  character to call these functions without guaranteeing that the
  first character is valid is an _error_
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
        match Lang.keyword_of_string ~lang:(lang lex) ident with
        | Some kw -> return (Token.Keyword kw)
        | None -> return (Token.Identifier ident) )
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
    if Uchar.equal fst (Uchar.of_char '0')
    then
      let%bind ch = peek_ch lex in
      match Option.bind ~f:Uchar.to_char ch with
      | Some 'x' | Some 'X' ->
          eat_ch lex ;
          return (16, [])
      | Some 'o' | Some 'O' ->
          eat_ch lex ;
          return (8, [])
      | Some 'b' | Some 'B' ->
          eat_ch lex ;
          return (2, [])
      | Some _ | None -> return (10, [fst])
    else return (10, [fst])
  in
  let rec helper lst separator_allowed =
    let%bind after_ch = peek_ch2 lex in
    let%bind ch = peek_ch lex in
    let separator ch =
      match after_ch with
      | Some after_ch -> ch =~ ',' && is_number_continue after_ch base
      | None -> false
    in
    match ch with
    | Some ch when is_number_continue ch base ->
        eat_ch lex ;
        helper (ch :: lst) true
    | Some ch when separator ch ->
        if separator_allowed
        then ( eat_ch lex ; helper lst false )
        else return_err Error.Malformed_number_literal
    | Some _ | None ->
        let char_to_int ch =
          match Uchar.to_char ch with
          | None -> raise (Bug_lexer "malformed number literal")
          | Some ch ->
              if ch >= '0' && ch <= '9'
              then Char.to_int ch - Char.to_int '0'
              else if ch >= 'a' && ch <= 'f'
              then Char.to_int ch - Char.to_int 'a' + 10
              else if ch >= 'A' && ch <= 'F'
              then Char.to_int ch - Char.to_int 'A' + 10
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
        | "@" -> return Token.Attribute
        | ":" -> return Token.Colon
        | "<-" -> return Token.Assign
        | "->" -> return Token.Arrow
        | "=>" -> return Token.Thicc_arrow
        | "::" -> return Token.Double_colon
        | _ -> return (Token.Operator ident) )
  in
  helper []

let lex_identifier_operator lex =
  eat_ch lex ;
  match%bind peek_ch lex with
  | Some ch when is_ident_start ch -> (
      let%bind ident = lex_ident lex in
      match ident with
      | Token.Identifier id -> return (Token.Identifier_operator id)
      | tok -> return_err (Error.Identifier_operator_is_keyword tok) )
  | ch -> return_err (Error.Identifier_operator_start_without_ident ch)

let rec next_token lex =
  let%bind () = eat_whitespace lex in
  let single_char_tok tok sp = eat_ch lex ; (Ok tok, sp) in
  match peek_ch lex with
  | Ok None, _ -> return Token.Eof
  | Error e, sp -> (Error e, sp)
  | Ok (Some ch), sp ->
      if ch =~ '('
      then single_char_tok Token.Open_paren sp
      else if ch =~ ')'
      then single_char_tok Token.Close_paren sp
      else if ch =~ '{'
      then single_char_tok Token.Open_brace sp
      else if ch =~ '}'
      then single_char_tok Token.Close_brace sp
      else if ch =~ '['
      then single_char_tok Token.Open_square sp
      else if ch =~ ']'
      then single_char_tok Token.Close_square sp
      else if ch =~ ';'
      then single_char_tok Token.Semicolon sp
      else if ch =~ ','
      then single_char_tok Token.Comma sp
      else if ch =~ '.'
      then single_char_tok Token.Dot sp
      else if ch =~ '#'
      then lex_comment lex
      else if ch =~ '\\'
      then lex_identifier_operator lex
      else if is_ident_start ch
      then lex_ident lex
      else if is_operator_start ch
      then lex_operator lex
      else if is_number_start ch
      then lex_number lex
      else return_err (Error.Unrecognized_character ch)

and lex_comment lex =
  eat_ch lex ;
  let rec block_comment () =
    let rec eat_the_things () =
      match next_ch lex with
      | Ok (Some ch), _ when ch =~ '#' -> (
        match next_ch lex with
        | Ok (Some ch), _ when ch =~ '}' -> return ()
        | Ok (Some ch), _ when ch =~ '{' ->
            let%bind () = block_comment () in
            eat_the_things ()
        | Ok (Some _), _ -> eat_the_things ()
        | Ok None, _ -> return_err Error.Unclosed_comment
        | Error e, sp -> (Error e, sp) )
      | Ok (Some _), _ -> eat_the_things ()
      | Ok None, _ -> return_err Error.Unclosed_comment
      | Error e, sp -> (Error e, sp)
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
  match peek_ch lex with
  | Ok (Some ch), _ when ch =~ '{' -> (
      eat_ch lex ;
      match block_comment () with
      | Ok (), _ -> next_token lex
      | Error e, sp -> (Error e, sp) )
  | Ok (Some _), _ -> (
    match line_comment () with
    | Ok (), _ -> next_token lex
    | Error e, sp -> (Error e, sp) )
  | Ok None, _ -> return Token.Eof
  | Error e, sp -> (Error e, sp)
