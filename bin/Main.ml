open Cafec
module Out = Stdio.Out_channel
module In = Stdio.In_channel

type compile_style =
  | Typecheck
  | Interpret
  | Compile

type args =
  | Args :
      { filename : string
      ; print_parse_ast : bool
      ; style : compile_style }
      -> args

let args_filename (Args {filename; _}) = filename

let args_print_parse_ast (Args {print_parse_ast; _}) = print_parse_ast

let args_style (Args {style; _}) = style

let parse_command_line () : (args, string) Result.t =
  let print_parse_ast = ref false in
  let typeck_only = ref false in
  let interpret = ref false in
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
      , Set interpret
      , "interpret, instead of using the LLVM backend" )
    ; ( "--typecheck-only"
      , Set typeck_only
      , "only typecheck, don't interpret or compile" ) ]
  in
  Caml.Arg.parse cmd_options get_filename "cafec [filename]" ;
  match !error with
  | Some err -> Error err
  | None -> (
    match !filename with
    | Some filename ->
        if !typeck_only && !interpret
        then Error "both --typecheck-only and --interpret specified"
        else
          let style =
            if !typeck_only
            then Typecheck
            else if !interpret
            then Interpret
            else Compile
          in
          Ok (Args {filename; print_parse_ast = !print_parse_ast; style})
    | None -> Error "no filename specified" )

let get_parse_ast args =
  match In.with_file (args_filename args) ~f:Parse.parse with
  | Error e, sp ->
      Out.eprintf "Parse error: %s\n"
        (Spanned.to_string ~f:Parse.Error.to_string (e, sp)) ;
      Caml.exit 1
  | Ok parse_ast, _ ->
      if args_print_parse_ast args
      then Out.print_endline (Parse.Ast.to_string parse_ast) ;
      parse_ast

let get_typed_ast args =
  let parse_ast = get_parse_ast args in
  match Typed_ast.make parse_ast with
  | Error (e, ctxt), sp ->
      let f e = Typed_ast.Error.to_string e ~ctxt in
      Out.eprintf "Typing error: %s\n" (Spanned.to_string ~f (e, sp)) ;
      Caml.exit 1
  | Ok ty_ast, _ -> ty_ast

let interpret ty_ast _args =
  let ctxt = Interpreter.make ty_ast in
  let string = Nfc_string.of_string_unsafe "main" in
  let name =
    Name.Name {string; kind = Name.Identifier; fixity = Name.Nonfix}
  in
  match Interpreter.get_function ctxt ~name with
  | Some f ->
      let v = Interpreter.call ctxt f [] in
      Out.output_string Out.stdout "main returned: " ;
      Out.output_string Out.stdout (Interpreter.Value.to_string v ctxt) ;
      Out.newline Out.stdout
  | None -> Out.output_string Out.stderr "no main was found\n"

(*let compile ty_ast _args = Llvm.output_object_file ~file:"foo.o" ty_ast*)

let main args =
  let typed_ast = get_typed_ast args in
  match args_style args with
  | Typecheck -> ()
  | Interpret -> interpret typed_ast args
  | Compile -> failwith "compiler backend doesn't exist yet"

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
