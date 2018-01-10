open Cafec_spanned.Prelude;

module Ast: (module type of Ast);

module Error: (module type of Error);

let parse: string => spanned_result(Ast.t, Error.t);
