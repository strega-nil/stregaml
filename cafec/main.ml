module Parse = Cafec_parse
module Typed_ast = Cafec_typed_ast
module Interpreter = Cafec_interpreter
module Out = Stdio.Out_channel
module In = Stdio.In_channel

type command_line_arguments = {filename: string; print_parse_ast: bool}

let parse_command_line () : (command_line_arguments, string) Result.t =
  let print_parse_ast = ref false in
  let error = ref None in
  let filename = ref None in
  let get_filename name =
    match !filename with
    | Some _ -> error := Some "filename specified multiple times"
    | None -> filename := Some name
  in
  let cmd_options =
    let open Caml.Arg in
    [ ( "--print-parse-ast"
      , Set print_parse_ast
      , "print the (untyped) parsed ast" ) ]
  in
  Caml.Arg.parse cmd_options get_filename "cafec [filename]" ;
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
        Out.fprintf Out.stderr "error while parsing command line arguments: %s"
          e ;
        Caml.exit 1
  in
  let program = In.with_file args.filename ~f:In.input_all in
  match Parse.parse program with
  | Error e, sp ->
      Out.output_string Out.stderr "Error: " ;
      Parse.Error.output_spanned Out.stderr (e, sp) ;
      Out.newline Out.stderr
  | Ok unt_ast, _ ->
      Parse.Ast.output Out.stdout unt_ast ;
      Out.flush Out.stdout ;
      match Typed_ast.make unt_ast with
      | Error (e, context), sp ->
          Out.output_string Out.stderr "Error: " ;
          Typed_ast.Error.output_spanned Out.stderr (e, sp) context ;
          Out.newline Out.stderr
      | Ok ty_ast, _ -> Interpreter.run ty_ast


let () = main ()
