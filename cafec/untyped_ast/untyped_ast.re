let rec print_indent = (indent) =>
  switch indent {
  | n when n <= 0 => ()
  | n =>
    print_string("  ");
    print_indent(n - 1);
  };

module Expr = {
  type t = unit;
  let unit_literal = () => ();
  let print = ((), indent) => {
    print_indent(indent);
    print_string("()");
  };
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
  funcs: array(Function.t),
  tys: array(Type_definition.t)
};

let make = (funcs, tys) => {funcs, tys};

let print = (self) =>
  self.funcs
  |> Array.iter(
       (func) => {
         Printf.printf("func %s() {\n", func.Function.name);
         Expr.print(func.Function.expr, 1);
         print_string("\n};\n");
       }
     );
