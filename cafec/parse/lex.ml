module Spanned = Cafec_spanned
open Spanned.Prelude
open Error.Monad_spanned
module Error = Error

exception Bug_lexer of string

type t =
  {buffer: string; mutable index: int; mutable line: int; mutable column: int}

let lexer s = {buffer= s; index= 0; line= 1; column= 1}

(* the actual lexer functions *)
let is_whitespace ch = ch = ' ' || ch = '\t' || ch = '\n' || ch = '\r'

let is_alpha ch = ch >= 'A' && ch <= 'Z' || ch >= 'a' && ch <= 'z'

let is_number_start ch = ch >= '0' && ch <= '9'

let is_number_continue ch = function
  | 2 -> ch = '0' || ch = '1'
  | 8 -> ch >= '0' && ch <= '7'
  | 10 -> ch >= '0' && ch <= '9'
  | 16 ->
      ch >= '0' && ch <= '9' || ch >= 'a' && ch <= 'f'
      || ch >= 'A' && ch <= 'F'
  | base -> raise (Bug_lexer ("Invalid base: " ^ string_of_int base))


let is_ident_start ch = is_alpha ch || ch = '_'

let is_ident_continue_no_prime ch = is_ident_start ch || is_number_start ch

let is_ident_continue ch = is_ident_continue_no_prime ch || ch = '\''

let is_operator_start = function
  | '!' | '#' | '$' | '%' | '&' | '*' | '+' | '-' | '/' | ':' | '<' | '='
   |'>' | '?' | '@' | '\\' | '^' | '|' | '~' ->
      true
  | _ -> false


let is_operator_continue ch = is_operator_start ch || ch = '.'

let current_span lex =
  { start_line= lex.line
  ; start_column= lex.column
  ; end_line= lex.line
  ; end_column= lex.column + 1 }


let peek_ch lex =
  if String.length lex.buffer <= lex.index then None
  else Some ((lex.buffer).[lex.index], current_span lex)


let next_ch lex =
  match peek_ch lex with
  | Some (ch, sp) ->
      lex.index <- lex.index + 1 ;
      if ch = '\n' then (
        lex.line <- lex.line + 1 ;
        lex.column <- 1 )
      else lex.column <- lex.column + 1 ;
      Some (ch, sp)
  | None -> None


let eat_ch lex = next_ch lex |> ignore

let rec eat_whitespace lex =
  match peek_ch lex with
  | Some (ch, _) when is_whitespace ch -> eat_ch lex ; eat_whitespace lex
  | Some _ | None -> ()


let lex_ident fst sp lex =
  let module S = String_buffer in
  let buff = S.with_capacity 16 in
  let rec helper idx sp =
    match peek_ch lex with
    | Some (ch, sp') when is_ident_continue ch ->
        eat_ch lex ;
        S.push ch buff ;
        helper (idx + 1) (Spanned.union sp sp')
    | Some _ | None ->
        let kw k = Ok (Token.Keyword k, sp) in
        match S.to_string buff with
        | "true" -> kw Token.Keyword_true
        | "false" -> kw Token.Keyword_false
        | "if" -> kw Token.Keyword_if
        | "else" -> kw Token.Keyword_else
        | "func" -> kw Token.Keyword_func
        | "type" -> kw Token.Keyword_type
        | "struct" -> kw Token.Keyword_struct
        | "_" -> kw Token.Keyword_underscore
        | "let" as res -> Error (Error.Reserved_token res, sp)
        | "variant" as res -> Error (Error.Reserved_token res, sp)
        | id -> Ok (Token.Identifier id, sp)
  in
  S.push fst buff ; helper 1 sp


let lex_number fst sp lex =
  let module S = String_buffer in
  let buff = S.with_capacity 22 in
  let base, idx, sp =
    if fst = '0' then
      match peek_ch lex with
      | Some ('x', sp') -> eat_ch lex ; (16, 0, Spanned.union sp sp')
      | Some ('o', sp') -> eat_ch lex ; (8, 0, Spanned.union sp sp')
      | Some ('b', sp') -> eat_ch lex ; (2, 0, Spanned.union sp sp')
      | Some _ | None -> S.push '0' buff ; (10, 1, sp)
    else ( S.push fst buff ; (10, 1, sp) )
  in
  let rec helper idx sp space_allowed =
    match peek_ch lex with
    | Some (ch, sp') when is_number_continue ch base ->
        eat_ch lex ;
        S.push ch buff ;
        helper (idx + 1) (Spanned.union sp sp') true
    | Some (' ', sp') ->
        eat_ch lex ;
        S.push ' ' buff ;
        helper idx (Spanned.union sp sp') false
    | Some (ch, sp')
      when space_allowed && (is_number_continue ch 10 || is_ident_continue ch) ->
        eat_ch lex ;
        S.push ch buff ;
        Error (Error.Malformed_number_literal, Spanned.union sp sp')
    | Some _ | None ->
        let char_to_int ch =
          if ch >= '0' && ch <= '9' then Char.code ch - Char.code '0'
          else if ch >= 'a' && ch <= 'f' then Char.code ch - Char.code 'a' + 10
          else if ch >= 'A' && ch <= 'F' then Char.code ch - Char.code 'A' + 10
          else assert false
        in
        (* TODO(ubsan): fix overflow *)
        let rec to_int idx acc =
          if idx < S.length buff then
            let ch = S.get idx buff in
            if ch = ' ' then to_int (idx + 1) acc
            else to_int (idx + 1) (acc * base + char_to_int ch)
          else acc
        in
        Ok (Token.Integer_literal (to_int 0 0), sp)
  in
  helper idx sp true


let rec next_token lex =
  let lex_operator fst sp lex =
    let rec block_comment sp =
      let rec eat_the_things () =
        match next_ch lex with
        | Some ('*', _) -> (
          match next_ch lex with
          | Some ('/', _) -> wrap ()
          | _ -> eat_the_things () )
        | Some ('/', sp') -> (
          match next_ch lex with
          | Some ('*', _) ->
              let%bind (), _ = block_comment sp' in
              eat_the_things ()
          | _ -> eat_the_things () )
        | Some _ -> eat_the_things ()
        | None ->
            Error (Error.Unclosed_comment, Spanned.union sp (current_span lex))
      in
      eat_the_things ()
    in
    let line_comment () =
      let rec eat_the_things () =
        match next_ch lex with
        | Some ('\n', _) | None -> ()
        | Some _ -> eat_the_things ()
      in
      eat_the_things ()
    in
    (* TODO(ubsan): pull this out into pred *)
    let includes_operator_token s =
      let is_invalid = function
        | '*', '/' -> true
        | '/', '/' -> true
        | '/', '*' -> true
        | _ -> false
      in
      let rec helper s idx =
        if String.length s > idx + 1 then
          if not (is_invalid (s.[idx], s.[idx + 1])) then false
          else helper s (idx + 1)
        else true
      in
      helper s 0
    in
    let module S = String_buffer in
    let buff = S.with_capacity 8 in
    let rec helper idx sp =
      match peek_ch lex with
      | Some (ch, sp') when is_operator_continue ch ->
          eat_ch lex ;
          S.push ch buff ;
          helper (idx + 1) (Spanned.union sp sp')
      | Some _ | None ->
        match S.to_string buff with
        | "/*" -> (
          match block_comment sp with
          | Ok ((), _) -> next_token lex
          | Error e -> Error e )
        | "//" -> line_comment () ; next_token lex
        | ":" -> Ok (Token.Colon, sp)
        | "=" -> Ok (Token.Equals, sp)
        | "->" -> Ok (Token.Arrow, sp)
        | "|" as res -> Error (Error.Reserved_token res, sp)
        | op when includes_operator_token op ->
            Error (Error.Operator_including_comment_token op, sp)
        | op -> Ok (Token.Operator op, sp)
    in
    S.push fst buff ; helper 1 sp
  in
  eat_whitespace lex ;
  match next_ch lex with
  | Some ('(', sp) -> Ok (Token.Open_paren, sp)
  | Some (')', sp) -> Ok (Token.Close_paren, sp)
  | Some ('{', sp) -> Ok (Token.Open_brace, sp)
  | Some ('}', sp) -> Ok (Token.Close_brace, sp)
  | Some ('[', sp) -> Error (Error.Reserved_token "[", sp)
  | Some (']', sp) -> Error (Error.Reserved_token "]", sp)
  | Some (';', sp) -> Ok (Token.Semicolon, sp)
  | Some (',', sp) -> Ok (Token.Comma, sp)
  | Some ('.', sp) -> Ok (Token.Dot, sp)
  | Some ('\'', sp) -> Error (Error.Reserved_token "'", sp)
  | Some (ch, sp) when is_ident_start ch -> lex_ident ch sp lex
  | Some (ch, sp) when is_operator_start ch -> lex_operator ch sp lex
  | Some (ch, sp) when is_number_start ch -> lex_number ch sp lex
  | Some (ch, sp) -> Error (Error.Unrecognized_character ch, sp)
  | None -> Ok (Token.Eof, current_span lex)
