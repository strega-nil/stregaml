module Error: (module type of Error);

open Cafec_spanned.Prelude;

type t;

let make: Cafec_parse.Ast.t => spanned_result(t, Error.t);

let run: t => unit;
