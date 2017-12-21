open Spanned.Prelude;

module Error: (module type of Error);

let parse: string => spanned_result(Untyped_ast.t, Error.t);
