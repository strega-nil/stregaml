open Pred;

let program = {|
func main() {
}
|};

let main = () => {
  open Spanned.Prelude;
  switch (Parse.parse(program)) {
  | SOk(ast, _) => Untyped_ast.print(ast)
  | SErr(e, sp) => Parse.Error.print_spanned(e, sp)
  };
};

main();
