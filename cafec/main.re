open Pred.Prelude;

let main = () => {
  let lexer = Lex.lexer("func main() { }");
  let res = Lex.iter(lexer, (tok) => Lex.print_tok(tok) |> print_newline);
  switch res {
  | Ok () => ()
  | Err(e) => Lex.print_error(e) |> print_newline
  };
};

main();
