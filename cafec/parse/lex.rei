open Pred;
open Spanned.Prelude;

type t;

let lexer: string => t;
let next_token: t => spanned(Token.t, Error.t);
