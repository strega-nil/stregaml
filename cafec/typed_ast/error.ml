module Spanned = Cafec_spanned

open Spanned.Prelude

type t =
| Multiple_function_definitions of (string * span)

module Monad_spanned = Spanned.Monad(struct type nonrec t = t end)

let print = function
| Multiple_function_definitions (name, sp) ->
  Printf.printf "function %s (found from " name;
  Spanned.print_span sp;
  print_string ") defined multiple times"

let print_spanned (self, sp) =
  match self with
  | Multiple_function_definitions(name, sp') ->
    Printf.printf "function %s (found from " name;
    Spanned.print_span sp';
    print_string ") defined multiple times (at ";
    Spanned.print_span sp;
    print_char ')'
