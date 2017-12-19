let program = {|
func foo() {
  ()
};
func main() {
  foo((),)
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
