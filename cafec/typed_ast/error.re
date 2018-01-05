open Spanned.Prelude;

type builder =
  | Multiple_function_definitions(string, span)
and t = spanned(builder);

module Monad_result = Pred.Result.Monad({type nonrec t = t;});

let print = (self) =>
  switch self {
  | Multiple_function_definitions(name, sp) =>
    Printf.printf("function %s (found from ", name);
    Spanned.print_span(sp);
    Printf.printf(") defined multiple times");
  };
