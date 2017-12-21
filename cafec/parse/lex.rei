open Spanned.Prelude;

type t;

let lexer: string => t;

let next_token: t => spanned_result(Token.t, Error.t);
