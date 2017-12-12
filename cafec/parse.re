open Pred;

open Spanned.Prelude;

module Lex = Parse__lex;

let parse = (program) => {
  let lexer = Lex.lexer(program);
  let err =
    Lex.iter(lexer)
    |> Iter.for_each_break(
         (res) =>
           switch res {
           | SOk(tok, sp) =>
             Token.print_spanned(tok, sp);
             print_newline();
             None;
           | SErr(e, sp) => Some((e, sp))
           }
       );
  switch err {
  | Some((e, sp)) =>
    print_string("error: ");
    Parser_error.print_spanned(e, sp);
    print_newline();
  | None => ()
  };
  unimplemented();
};
