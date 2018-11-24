open OUnit2

exception Parser_error of Cafec.Parse.Error.t

exception Type_error of (Cafec.Typed_ast.Error.t * Cafec.Typed_ast.Type.context)

exception Function_not_found of string

(* note: should add more info eventually to these *)
let () =
  Caml.Printexc.register_printer (function
    | Parser_error _ -> Some "Parser error"
    | Type_error _ -> Some "Type error"
    | Function_not_found s -> Some ("Function not found: " ^ s)
    | _ -> None )

let call program name args =
  match Cafec.Parse.parse program with
  | Error e, _ -> raise (Parser_error e)
  | Ok unt_ast, _ -> (
    match Cafec.Typed_ast.make unt_ast with
    | Error e, _ -> raise (Type_error e)
    | Ok ty_ast, _ -> (
        let ctxt = Cafec.Interpreter.make ty_ast in
        match Cafec.Interpreter.get_function ctxt ~name with
        | Some f -> Cafec.Interpreter.call ctxt f args
        | None -> raise (Function_not_found name) ) )

let _ =
  let module Value = Cafec.Interpreter.Value in
  let program =
    {|
type vec2 = struct { x0: int; x1: int };

func map(p: vec2, f: func(int) -> int): vec2 =
  vec2 { x0 = f(p.x0); x1 = f(p.x1) };

func double(i: int): int = MUL(i, 2);

func main(): vec2 =
  map(vec2 { x0 = 2014; x1 = 9 }, double); |}
  in
  let tests =
    [ ( "calling non-existent function"
      >:: fun _ ->
      match call program "foo" [] with
      | exception Function_not_found "foo" -> ()
      | exception _ -> assert_failure "threw the wrong exception"
      | _ -> assert_failure "should have thrown function not found" )
    ; ( "calling a function incorrectly"
      >:: fun _ ->
      match call program "main" [Value.Integer 0] with
      | exception Invalid_argument _ -> ()
      | exception _ -> assert_failure "threw the wrong exception"
      | _ -> assert_failure "should have thrown invalid argument" )
    ; ( "higher order function, struct"
      >:: fun _ ->
      let expected = Value.Struct [|Value.Integer 4028; Value.Integer 18|] in
      assert (Value.equal (call program "main" []) expected) ) ]
  in
  run_test_tt_main ("Tests" >::: tests)
