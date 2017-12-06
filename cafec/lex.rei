open Pred;
open Spanned.Prelude;

include (module type of Lexer_types);

type t;

let lexer: string => t;
let iter: t => iter(spanned(token, error));

let print_token: token => unit;
let print_spanned_token: (token, span) => unit;
let print_error: error => unit;
let print_spanned_error: (error, span) => unit;
