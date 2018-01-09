open Cafec_spanned.Prelude;

module Error: (module type of Error);

let parse: string => spanned_result(Cafec_untyped_ast.t, Error.t);
