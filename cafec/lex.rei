open Pred.Prelude;

type t;
type token;
type error;

let lexer: string => t;
let iter: (t, token => unit) => result(unit, error);

let print_tok: token => unit;
let print_error: error => unit;
