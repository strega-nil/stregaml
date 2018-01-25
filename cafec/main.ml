module Parse = Cafec_parse
module Typed_ast = Cafec_typed_ast

type command_line_arguments = {filename: string; print_parse_ast: bool}

let parse_command_line () : (command_line_arguments, string) result =
  let print_parse_ast = ref false in
  let error = ref None in
  let filename = ref None in
  let get_filename name =
    match !filename with
    | Some _ -> error := Some "filename specified multiple times"
    | None -> filename := Some name
  in
  let cmd_options =
    let open Arg in
    [ ( "--print-parse-ast"
      , Set print_parse_ast
      , "print the (untyped) parsed ast" ) ]
  in
  Arg.parse cmd_options get_filename "cafec [filename]" ;
  match !error with
  | Some err -> Error err
  | None ->
    match !filename with
    | Some filename -> Ok {filename; print_parse_ast= !print_parse_ast}
    | None -> Error "no filename specified"


let main () =
  let args =
    match parse_command_line () with
    | Ok args -> args
    | Error e ->
        Printf.printf "error while parsing command line arguments: %s" e ;
        exit 1
  in
  let program =
    let file = open_in args.filename in
    let buffer = String_buffer.with_capacity (in_channel_length file) in
    (* yes this is ugly. I need to write a good I/O library, I think *)
    let rec loop () =
      String_buffer.push (input_char file) buffer ;
      loop ()
    in
    (try loop () with _ -> ()) close_in file ;
    String_buffer.to_string buffer
  in
  let unt_ast, _ =
    Parse.parse program
    |> Result.expect (fun e ->
           print_string "Error: " ;
           Parse.Error.print_spanned e ;
           print_newline () ;
           assert false )
  in
  Parse.Ast.print unt_ast ;
  let ty_ast, _ =
    Typed_ast.make unt_ast
    |> Result.expect (fun e ->
           print_string "Error: " ;
           Typed_ast.Error.print_spanned e ;
           print_newline () ;
           assert false )
  in
  Typed_ast.run ty_ast


let () = main ()
