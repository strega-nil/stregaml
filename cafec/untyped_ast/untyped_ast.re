open Pred;

module Expr = {
  type t = unit;
  let unit_literal = () => ();
};

module Type = {
  type t =
    | Named(string);
  let named = (name) => Named(name);
};

module Type_definition = {
  type t = unit;
};

module Function = {
  type t = {
    name: string,
    expr: Expr.t
  };
  let make = (name, expr) => {name, expr};
};

type t = {
  funcs: list(Function.t),
  tys: list(Type_definition.t)
};

let make = (funcs, tys) => {funcs, tys};

let print = (self) => {
  let rec helper = (funcs) =>
    switch funcs {
    | [x, ...xs] =>
      Printf.printf("func %s;\n", x.Function.name);
      helper(xs);
    | [] => ()
    };
  helper(self.funcs);
};
