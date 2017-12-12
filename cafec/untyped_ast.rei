module Expr: {
  type t;
  let unit_literal: unit => t;
};

type t;

let make: unit => t;

let add_function: (t, string, Expr.t) => unit;

let print_ast: t => unit;
