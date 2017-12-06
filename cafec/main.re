open Pred;

let main = () => {
  open Spanned.Prelude;
  let lexer = Lex.lexer({|
func main() {

}
|});
  let err =
    Lex.iter(lexer)
    |> Iter.for_each_break(
         (res) =>
           switch res {
           | SOk(tok, sp) =>
             Lex.print_spanned_token(tok, sp);
             print_newline();
             None;
           | SErr(e, sp) => Some((e, sp))
           }
       );
  switch err {
  | Some((e, sp)) =>
    print_string("error: ");
    Lex.print_spanned_error(e, sp);
    print_newline();
  | None => ()
  };
};

main();
