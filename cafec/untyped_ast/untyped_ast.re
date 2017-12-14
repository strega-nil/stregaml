open Pred;

module Expr = {
  type t = unit;
  let unit_literal = () => ();
  let print = () => print_string("()");
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
      Printf.printf("func %s = ", x.Function.name);
      Expr.print(x.Function.expr);
      print_string(";\n");
      helper(xs);
    | [] => ()
    };
  helper(self.funcs);
};
