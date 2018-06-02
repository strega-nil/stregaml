open Cafec
module Out = Stdio.Out_channel
module In = Stdio.In_channel

type command_line_arguments =
  {filename: string; print_parse_ast: bool; interpreted: bool}

let parse_command_line () : (command_line_arguments, string) Result.t =
  let print_parse_ast = ref false in
  let interpreted = ref false in
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
      , "print the (untyped) parsed ast" )
    ; ( "--interpret"
      , Set interpreted
      , "interpret, instead of using the LLVM backend" ) ]
  in
  Caml.Arg.parse cmd_options get_filename "cafec [filename]" ;
  match !error with
  | Some err -> Error err
  | None ->
    match !filename with
    | Some filename ->
        Ok
          { filename
          ; print_parse_ast= !print_parse_ast
          ; interpreted= !interpreted }
    | None -> Error "no filename specified"


let get_parse_ast args =
  let program = In.with_file args.filename ~f:In.input_all in
  match Parse.parse program with
  | Error e, sp ->
      Out.eprintf "Error: %s\n"
        (Spanned.to_string ~f:Parse.Error.to_string (e, sp)) ;
      Caml.exit 1
  | Ok parse_ast, _ ->
      if args.print_parse_ast then
        Out.print_endline (Parse.Ast.to_string parse_ast) ;
      parse_ast


let get_typed_ast args =
  let parse_ast = get_parse_ast args in
  match Typed_ast.make parse_ast with
  | Error (e, ctxt), sp ->
      let f e = Typed_ast.Error.to_string e ~ctxt in
      Out.eprintf "Error: %s\n" (Spanned.to_string ~f (e, sp)) ;
      Caml.exit 1
  | Ok ty_ast, _ -> ty_ast


let interpret ty_ast _args =
  let ctxt = Interpreter.make ty_ast in
  match Interpreter.get_function ctxt ~name:"main" with
  | Some f ->
      let v = Interpreter.call ctxt f [] in
      Out.output_string Out.stdout "main returned: " ;
      Out.output_string Out.stdout (Interpreter.Value.to_string v ctxt) ;
      Out.newline Out.stdout
  | None -> Out.output_string Out.stderr "no main was found\n"


(*let compile ty_ast _args = Llvm.output_object_file ~file:"foo.o" ty_ast*)

let main args =
  let typed_ast = get_typed_ast args in
  if args.interpreted then interpret typed_ast args else assert false


let () =
  let args =
    match parse_command_line () with
    | Ok args -> args
    | Error e ->
        Out.fprintf Out.stderr "error while parsing command line arguments: %s"
          e ;
        Caml.exit 1
  in
  main args
