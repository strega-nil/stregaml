open Pred;

type t;
type token;
type error;

let lexer: string => t;
let iter: t => iter(result(token, error));

let print_token: token => unit;
let print_error: error => unit;
