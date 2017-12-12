open Pred;

module Expr = {
  type t = unit;
  let unit_literal = () => unimplemented();
};

type t = unit;

let make = () => unimplemented();

let add_function = (self, name, expr) => unimplemented();

let print_ast = (self) => unimplemented();
