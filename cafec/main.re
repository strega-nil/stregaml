open Pred;

let program = {|
func fib(x: int) -> int {
  if (LESS_EQ(x, 1)) {
    x
  } else {
    ADD(fib(SUB(x, 1)), fib(SUB(x, 2)))
  }
};
func main() -> int {
  fib(7)
};
|};

let main = () => {
  let ast = switch (Parse.parse(program)) {
  | Ok((ast, _)) => Untyped_ast.print(ast) |> print_newline; Some(ast)
  | Err((e, sp)) => Parse.Error.print_spanned(e, sp) |> print_newline; None
  };
  switch ast {
  | Some(ast) =>
    Typed_ast.make(ast) |> ignore;
    Untyped_ast.run(ast);
  | None => ()
  }
};

main();
