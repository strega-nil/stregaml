open Cafec_spanned.Prelude;

type t =
  | Multiple_function_definitions(string, span);

module Monad_spanned:
  Interfaces.Result_monad
  with type error = t
  and type t('a) = spanned_result('a, t);

let print: t => unit;
let print_spanned: spanned(t) => unit;
