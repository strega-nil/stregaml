open Pred;

include (module type of Lexer_types);

type t;

let lexer: string => t;
let iter: t => iter(token, error);

let print_token: token => unit;
let print_error: error => unit;
