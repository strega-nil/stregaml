open Pred;

module Parse = Cafec_parse;
module Typed_ast = Cafec_typed_ast;

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
  let (unt_ast, _) =
    Parse.parse(program)
    |> Result.expect((e) => {
      print_string("Error: ");
      Parse.Error.print_spanned(e);
      print_newline();
      assert false;
    });
  let (ty_ast, _) =
    Typed_ast.make(unt_ast)
    |> Result.expect((e) => {
      print_string("Error: ");
      Typed_ast.Error.print_spanned(e);
      print_newline();
      assert false;
    });
  Typed_ast.run(ty_ast)
};

main();
