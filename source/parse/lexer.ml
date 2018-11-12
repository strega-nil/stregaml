open! Types.Pervasives

exception Bug_lexer of string

type t = {decoder: Uutf.decoder; mutable peek: Uchar.t option}

let make ch =
  let decoder =
    Uutf.decoder
      ~nln:(`NLF (Uchar.of_char '\n'))
      ~encoding:`UTF_8 (`Channel ch)
  in
  {decoder; peek= None}

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

let is_ident_start ch = ch =~ '_' || Uucp.Id.is_xid_start ch

let is_ident_continue_no_prime ch = Uucp.Id.is_xid_continue ch

let is_ident_continue ch = ch =~ '\'' || is_ident_continue_no_prime ch

let is_operator_start ch =
  let is_non_math_operator = function
    | '-' | '*' | '/' | '%' | '&' | '!' | '?' | '@' | '\\' | '^' | ':' -> true
    | _ -> false
  in
  match Uucp.Gc.general_category ch with
  | `Sm -> true
  | _ -> (
    match Uchar.to_char ch with
    | None -> false
    | Some ch -> is_non_math_operator ch )

let is_operator_continue ch = is_operator_start ch

let current_span lex =
  let open Spanned.Span in
  let line = Uutf.decoder_line lex.decoder in
  let col = Uutf.decoder_col lex.decoder in
  {start_line= line; start_column= col; end_line= line; end_column= col + 1}

let peek_ch lex : Uchar.t option result =
  match lex.peek with
  | Some uch -> (Ok (Some uch), current_span lex)
  | None -> (
    match Uutf.decode lex.decoder with
    | `Await -> assert false
    | `Uchar uch ->
        let uch = Some uch in
        lex.peek <- uch ;
        (Ok uch, current_span lex)
    | `End -> (Ok None, Spanned.Span.made_up)
    | `Malformed s -> (Error (Error.Malformed_input s), current_span lex) )

let next_ch lex =
  match peek_ch lex with
  | (Result.Ok _, _) as o ->
      lex.peek <- None ;
      o
  | (Result.Error _, _) as e -> e

let eat_ch lex = lex.peek <- None

let rec eat_whitespace lex =
  let%bind ch = peek_ch lex in
  match ch with
  | Some ch when is_whitespace ch -> eat_ch lex ; eat_whitespace lex
  | Some _ | None -> return ()

let lex_ident fst sp lex =
  let rec helper lst =
    let%bind ch = peek_ch lex in
    match ch with
    | Some ch when is_ident_continue ch ->
        eat_ch lex ;
        helper (ch :: lst)
    | Some _ | None -> (
        let id = Ident.of_uchar_list (List.rev lst) in
        match Ident.to_string id with
        | "true" -> return Token.Keyword_true
        | "false" -> return Token.Keyword_false
        | "if" -> return Token.Keyword_if
        | "else" -> return Token.Keyword_else
        | "func" -> return Token.Keyword_func
        | "type" -> return Token.Keyword_type
        | "data" -> return Token.Keyword_data
        | "record" -> return Token.Keyword_record
        | "alias" -> return Token.Keyword_alias
        | "let" -> return Token.Keyword_let
        | "mut" -> return Token.Keyword_mut
        | "_" -> return Token.Keyword_underscore
        | "variant" -> return_err (Error.Reserved_token id)
        | "opaque" -> return_err (Error.Reserved_token id)
        | "public" -> return_err (Error.Reserved_token id)
        | _ -> return (Token.Identifier id) )
  in
  helper [fst]

let lex_number (fst : Uchar.t) sp lex =
  let open! Char.O in
  let%bind () = with_span sp in
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
    | Some ch when Uchar.equal ch (Uchar.of_char '\'') ->
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
  helper [fst] true

let rec next_token lex =
  let lex_operator fst sp lex =
    let rec block_comment () =
      let rec eat_the_things () =
        let%bind ch = next_ch lex in
        match ch with
        | Some ch when ch =~ '*' -> (
            let%bind ch = next_ch lex in
            match ch with
            | Some ch when ch =~ '/' -> return ()
            | _ -> eat_the_things () )
        | Some ch when ch =~ '/' -> (
            let%bind ch = next_ch lex in
            match ch with
            | Some ch when ch =~ '*' ->
                let%bind () = block_comment () in
                eat_the_things ()
            | _ -> eat_the_things () )
        | Some _ -> eat_the_things ()
        | None -> return_err Error.Unclosed_comment
      in
      eat_the_things ()
    in
    let line_comment () =
      let rec eat_the_things () =
        let%bind ch = next_ch lex in
        match ch with
        | Some ch when ch =~ '\n' -> return ()
        | None -> return ()
        | Some _ -> eat_the_things ()
      in
      eat_the_things ()
    in
    let includes_comment_token s =
      let is_invalid = function
        | '*', '/' -> true
        | '/', '/' -> true
        | '/', '*' -> true
        | _ -> false
      in
      let rec helper s idx =
        if String.length s > idx + 1 then
          if is_invalid (s.[idx], s.[idx + 1]) then true else helper s (idx + 1)
        else false
      in
      helper s 0
    in
    let rec helper lst =
      let%bind ch = peek_ch lex in
      match ch with
      | Some ch when is_operator_continue ch ->
          eat_ch lex ;
          helper (ch :: lst)
      | Some _ | None -> (
          let id = Ident.of_uchar_list (List.rev lst) in
          match Ident.to_string id with
          | "/*" ->
              let%bind () = block_comment () in
              next_token lex
          | "//" ->
              let%bind () = line_comment () in
              next_token lex
          | "=" -> return Token.Equals
          | ":" -> return Token.Colon
          | "<-" -> return Token.Assign
          | "->" -> return Token.Arrow
          | "::" -> return Token.Double_colon
          | "&" -> return Token.Reference
          | "|" -> return_err (Error.Reserved_token id)
          | "\\" -> return_err (Error.Reserved_token id)
          | op when includes_comment_token op ->
              return_err (Error.Operator_including_comment_token id)
          | _ -> return (Token.Operator id) )
    in
    helper [fst]
  in
  let%bind () = eat_whitespace lex in
  let%bind ch, sp = spanned_bind (next_ch lex) in
  match ch with
  | Some ch when ch =~ '(' -> return Token.Open_paren
  | Some ch when ch =~ ')' -> return Token.Close_paren
  | Some ch when ch =~ '{' -> return Token.Open_brace
  | Some ch when ch =~ '}' -> return Token.Close_brace
  | Some ch when ch =~ '[' ->
      return_err (Error.Reserved_token (Ident.of_string_unsafe "["))
  | Some ch when ch =~ ']' ->
      return_err (Error.Reserved_token (Ident.of_string_unsafe "]"))
  | Some ch when ch =~ ';' -> return Token.Semicolon
  | Some ch when ch =~ ',' -> return Token.Comma
  | Some ch when ch =~ '.' -> return Token.Dot
  | Some ch when is_ident_start ch -> lex_ident ch sp lex
  | Some ch when is_operator_start ch -> lex_operator ch sp lex
  | Some ch when is_number_start ch -> lex_number ch sp lex
  | Some ch -> return_err (Error.Unrecognized_character ch)
  | None -> return Token.Eof
