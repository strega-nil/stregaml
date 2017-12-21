open Spanned.Prelude;

type t;

let make: (Untyped_ast.t) => spanned_result(t, Error.t);
