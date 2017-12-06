open Pred;

let main = () => {
  let lexer = Lex.lexer("func main() { }");
  let err =
    Lex.iter(lexer)
    |> Iter.for_each_break(
         (res) =>
           switch res {
           | Result.Ok(tok) =>
             Lex.print_token(tok);
             print_newline();
             None;
           | Result.Err(e) => Some(e)
           }
       );
  switch err {
  | Some(e) =>
    print_string("error: ");
    Lex.print_error(e);
    print_newline();
  | None => ()
  };
};

main();
