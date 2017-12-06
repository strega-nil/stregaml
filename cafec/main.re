open Pred;

let main = () => {
  let lexer = Lex.lexer("func main() { }");
  let err = Lex.iter(lexer) |> Iter.for_each((tok) => Lex.print_token(tok) |> print_newline);
  switch err {
  | Lex.Error_end_of_file => ()
  | e =>
    print_string("error: ");
    Lex.print_error(e);
    print_newline();
  };
};

main();
