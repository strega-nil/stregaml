open Pred;
open Spanned.Prelude;

type t;

let lexer: string => t;
let iter: t => iter(spanned(Token.t, Parser_error.t));
