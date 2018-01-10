module Error: (module type of Error);

type t;

let make: Cafec_parse.Ast.t => result(t, Error.t);

let run: t => unit;
