module Error = Error
open Spanned.Result.Monad

exception Bug_lexer of string

type t =
  {buffer: string; mutable index: int; mutable line: int; mutable column: int}

let make s = {buffer= s; index= 0; line= 1; column= 1}

(* the actual lexer functions *)
let is_whitespace ch =
  let open! Char.O in ch = ' ' || ch = '\t' || ch = '\n' || ch = '\r'


let is_alpha ch =
  let open! Char.O in ch >= 'A' && ch <= 'Z' || ch >= 'a' && ch <= 'z'


let is_number_start ch = let open! Char.O in ch >= '0' && ch <= '9'

let is_number_continue ch base =
  let open! Char.O in
  match base with
  | 2 -> ch = '0' || ch = '1'
  | 8 -> ch >= '0' && ch <= '7'
  | 10 -> ch >= '0' && ch <= '9'
  | 16 ->
      ch >= '0' && ch <= '9' || ch >= 'a' && ch <= 'f'
      || ch >= 'A' && ch <= 'F'
  | base -> raise (Bug_lexer ("Invalid base: " ^ Int.to_string base))


let is_ident_start ch = is_alpha ch || Char.equal ch '_'

let is_ident_continue_no_prime ch = is_ident_start ch || is_number_start ch

let is_ident_continue ch = is_ident_continue_no_prime ch || Char.equal ch '\''

let is_operator_start = function
  | '!' | '#' | '$' | '%' | '&' | '*' | '+' | '-' | '/' | '<' | '=' | '>'
   |'?' | '@' | '\\' | '^' | '|' | '~' ->
      true
  | _ -> false


let is_operator_continue ch = is_operator_start ch || Char.equal ch '.'

let current_span lex =
  let open Spanned.Span in
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
      if Char.equal ch '\n' then (
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
  let buff = ref [fst] in
  let rec helper sp =
    match peek_ch lex with
    | Some (ch, sp') when is_ident_continue ch ->
        eat_ch lex ;
        buff := ch :: !buff ;
        helper (Spanned.Span.union sp sp')
    | Some _ | None ->
        let kw k = (Ok (Token.Keyword k), sp) in
        match String.of_char_list (List.rev !buff) with
        | "true" -> kw Token.Keyword.True
        | "false" -> kw Token.Keyword.False
        | "if" -> kw Token.Keyword.If
        | "else" -> kw Token.Keyword.Else
        | "func" -> kw Token.Keyword.Func
        | "type" -> kw Token.Keyword.Type
        | "data" -> kw Token.Keyword.Data
        | "alias" -> kw Token.Keyword.Alias
        | "_" -> kw Token.Keyword.Underscore
        | "let" as res -> (Error (Error.Reserved_token res), sp)
        | "variant" as res -> (Error (Error.Reserved_token res), sp)
        | id -> (Ok (Token.Identifier id), sp)
  in
  helper sp


let lex_number fst sp lex =
  let open! Char.O in
  let buff = ref [] in
  let base, sp =
    if fst = '0' then (
      match peek_ch lex with
      | Some ('x', sp') -> eat_ch lex ; (16, Spanned.Span.union sp sp')
      | Some ('o', sp') -> eat_ch lex ; (8, Spanned.Span.union sp sp')
      | Some ('b', sp') -> eat_ch lex ; (2, Spanned.Span.union sp sp')
      | Some _ | None ->
          buff := [fst] ;
          (10, sp) )
    else (
      buff := [fst] ;
      (10, sp) )
  in
  let rec helper sp quote_allowed =
    match peek_ch lex with
    | Some (ch, sp') when is_number_continue ch base ->
        eat_ch lex ;
        buff := ch :: !buff ;
        helper (Spanned.Span.union sp sp') true
    | Some ('\'', sp') ->
        if quote_allowed then (
          eat_ch lex ;
          helper (Spanned.Span.union sp sp') false )
        else (Error Error.Malformed_number_literal, Spanned.Span.union sp sp')
    | Some _ | None ->
        let char_to_int ch =
          if ch >= '0' && ch <= '9' then Char.to_int ch - Char.to_int '0'
          else if ch >= 'a' && ch <= 'f' then
            Char.to_int ch - Char.to_int 'a' + 10
          else if ch >= 'A' && ch <= 'F' then
            Char.to_int ch - Char.to_int 'A' + 10
          else assert false
        in
        (* TODO(ubsan): fix overflow *)
        let rec to_int pow acc = function
          | [] -> acc
          | ch :: xs ->
              let cur = char_to_int ch * pow in
              to_int (pow * base) (acc + cur) xs
        in
        (Ok (Token.Integer_literal (to_int 1 0 !buff)), sp)
  in
  helper sp true


let rec next_token lex =
  let lex_operator fst sp lex =
    let rec block_comment sp =
      let rec eat_the_things () =
        match next_ch lex with
        | Some ('*', _) -> (
          match next_ch lex with
          | Some ('/', _) -> return ()
          | _ -> eat_the_things () )
        | Some ('/', sp') -> (
          match next_ch lex with
          | Some ('*', _) ->
              let%bind () = block_comment sp' in
              eat_the_things ()
          | _ -> eat_the_things () )
        | Some _ -> eat_the_things ()
        | None ->
            ( Error Error.Unclosed_comment
            , Spanned.Span.union sp (current_span lex) )
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
    let includes_operator_token s =
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
    let buff = ref [fst] in
    let rec helper sp =
      match peek_ch lex with
      | Some (ch, sp') when is_operator_continue ch ->
          eat_ch lex ;
          buff := ch :: !buff ;
          helper (Spanned.Span.union sp sp')
      | Some _ | None ->
        match String.of_char_list (List.rev !buff) with
        | "/*" -> (
          match block_comment sp with
          | Ok (), _ -> next_token lex
          | Error e, sp -> (Error e, sp) )
        | "//" -> line_comment () ; next_token lex
        | "=" -> (Ok Token.Equals, sp)
        | "->" -> (Ok Token.Arrow, sp)
        | "|" as res -> (Error (Error.Reserved_token res), sp)
        | op when includes_operator_token op ->
            (Error (Error.Operator_including_comment_token op), sp)
        | op -> (Ok (Token.Operator op), sp)
    in
    helper sp
  in
  eat_whitespace lex ;
  match next_ch lex with
  | Some ('(', sp) -> (Ok Token.Open_paren, sp)
  | Some (')', sp) -> (Ok Token.Close_paren, sp)
  | Some ('{', sp) -> (
    match peek_ch lex with
    | Some ('|', sp') ->
        eat_ch lex ; (Ok Token.Open_record, Spanned.Span.union sp sp')
    | _ -> (Ok Token.Open_brace, sp) )
  | Some ('}', sp) -> (Ok Token.Close_brace, sp)
  | Some ('|', sp) -> (
    match peek_ch lex with
    | Some ('}', sp') ->
        eat_ch lex ; (Ok Token.Close_record, Spanned.Span.union sp sp')
    | _ -> lex_operator '|' sp lex )
  | Some ('[', sp) -> (Error (Error.Reserved_token "["), sp)
  | Some (']', sp) -> (Error (Error.Reserved_token "]"), sp)
  | Some (':', sp) -> (
    match peek_ch lex with
    | Some (':', sp') ->
        eat_ch lex ; (Ok Token.Double_colon, Spanned.Span.union sp sp')
    | _ -> (Ok Token.Colon, sp) )
  | Some (';', sp) -> (Ok Token.Semicolon, sp)
  | Some (',', sp) -> (Ok Token.Comma, sp)
  | Some ('.', sp) -> (Ok Token.Dot, sp)
  | Some ('\'', sp) -> (Error (Error.Reserved_token "'"), sp)
  | Some (ch, sp) when is_ident_start ch -> lex_ident ch sp lex
  | Some (ch, sp) when is_operator_start ch -> lex_operator ch sp lex
  | Some (ch, sp) when is_number_start ch -> lex_number ch sp lex
  | Some (ch, sp) -> (Error (Error.Unrecognized_character ch), sp)
  | None -> (Ok Token.Eof, current_span lex)
