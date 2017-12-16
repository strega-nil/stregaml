let program = {|
func foo() {
  ()
};
func main() {
  foo()
};
|};

let main = () =>
  Spanned.Prelude.(
    switch (Parse.parse(program)) {
    | SOk(ast, _) => Untyped_ast.print(ast) |> print_newline
    | SErr(e, sp) => Parse.Error.print_spanned(e, sp) |> print_newline
    }
  );

main();
