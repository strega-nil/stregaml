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
      ; style : compile_style
      ; lang : (module Parse.Language)
      ; debug_lang : (module Parse.Language) }
      -> args

let args_filename (Args {filename; _}) = filename

let args_print_parse_ast (Args {print_parse_ast; _}) = print_parse_ast

let args_style (Args {style; _}) = style

let args_lang (Args {lang; _}) = lang

let args_debug_lang (Args {debug_lang; _}) = debug_lang

let parse_command_line () : (args, string) Result.t =
  let print_parse_ast = ref false in
  let typeck_only = ref false in
  let interpret = ref false in
  let error = ref None in
  let filename = ref None in
  let lang = ref None in
  let debug_lang = ref None in
  let get_filename name =
    match !filename with
    | Some _ -> error := Some "filename specified multiple times"
    | None -> filename := Some name
  in
  let get_lang name =
    match !lang with
    | Some _ -> error := Some "language specified multiple times"
    | None -> (
      match Languages.get_lang name with
      | Ok l -> lang := Some l
      | Error e -> error := Some e )
  in
  let get_debug_lang name =
    match !debug_lang with
    | Some _ -> error := Some "debug language specified multiple times"
    | None -> (
      match Languages.get_lang name with
      | Ok l -> debug_lang := Some l
      | Error e -> error := Some e )
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
      , "only typecheck, don't interpret or compile" )
    ; ( "--language"
      , String get_lang
      , "set the (human) language to use for the specified project" )
    ; ( "--debug-language"
      , String get_debug_lang
      , "set the language to use for debugging output" ) ]
  in
  Caml.Arg.parse cmd_options get_filename "cafec [filename]" ;
  match !error with
  | Some err -> Error err
  | None -> (
      let lang =
        match !lang with
        | Some lang -> lang
        | None -> (module Languages.English : Parse.Language)
      in
      let debug_lang =
        match !debug_lang with
        | Some lang -> lang
        | None -> (module Languages.English : Parse.Language)
      in
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
            Ok
              (Args
                 { filename
                 ; print_parse_ast = !print_parse_ast
                 ; style
                 ; lang
                 ; debug_lang })
      | None -> Error "no filename specified" )

let get_parse_ast args =
  let lang = args_lang args in
  let debug_lang = args_debug_lang args in
  match In.with_file (args_filename args) ~f:(Parse.parse ~lang) with
  | Error e, sp ->
      Out.eprintf "Parse error: %s\n"
        (Spanned.to_string ~f:(Parse.Error.to_string ~lang) (e, sp)) ;
      Caml.exit 1
  | Ok parse_ast, _ ->
      if args_print_parse_ast args
      then
        Out.print_endline
          (Parse.Ast.to_string ~lang:debug_lang parse_ast) ;
      parse_ast

let get_typed_ast args =
  let parse_ast = get_parse_ast args in
  match Typed_ast.make parse_ast with
  | Error (e, ctxt), sp ->
      let f e = Typed_ast.Error.to_string e ~ctxt in
      Out.eprintf "Typing error: %s\n" (Spanned.to_string ~f (e, sp)) ;
      Caml.exit 1
  | Ok ty_ast, _ -> ty_ast

let interpret ty_ast args =
  let ctxt = Interpreter.make ty_ast in
  match Interpreter.entrypoint ctxt with
  | Some f ->
      let v = Interpreter.call ctxt f [] in
      Out.output_string Out.stdout "entrypoint returned: " ;
      Out.output_string Out.stdout
        (Interpreter.Value.to_string ~lang:(args_debug_lang args) v
           ctxt) ;
      Out.newline Out.stdout
  | None -> Out.output_string Out.stderr "no entrypoint was found\n"

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
        Out.fprintf Out.stderr
          "error while parsing command line arguments: %s" e ;
        Caml.exit 1
  in
  main args
