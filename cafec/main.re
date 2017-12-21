let program = {|
func fib(x: i32) -> i32 {
  if (LESS_EQ(x, 1)) {
    x
  } else {
    ADD(fib(SUB(x, 1)), fib(SUB(x, 2)))
  }
};
func main() -> i32 {
  fib(5)
};
|};

let main = () => {
  let ast = Spanned.Prelude.(
    switch (Parse.parse(program)) {
    | SOk(ast, _) => Untyped_ast.print(ast) |> print_newline; Some(ast)
    | SErr(e, sp) => Parse.Error.print_spanned(e, sp) |> print_newline; None
    }
  );
  switch ast {
  | Some(ast) => Untyped_ast.run(ast)
  | None => ()
  }
};

main();
