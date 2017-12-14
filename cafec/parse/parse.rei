open Spanned.Prelude;

module Error: (module type of Error);

let parse: string => spanned(Untyped_ast.t, Error.t);
