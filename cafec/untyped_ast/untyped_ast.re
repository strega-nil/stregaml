let rec print_indent = (indent) =>
  switch indent {
  | n when n <= 0 => ()
  | n =>
    print_string("  ");
    print_indent(n - 1);
  };

module Expr = {
  type t = 
    | Unit_literal
    | Variable(string)
    | Call(t);
  let unit_literal = () => Unit_literal;
  let variable = (s) => Variable(s);
  let call = (e) => Call(e);

  let rec print = (e, indent) => {
    switch (e) {
    | Unit_literal => print_indent(indent); print_string("()")
    | Variable(s) => print_indent(indent); print_string(s)
    | Call(e) => print(e, indent); print_string("()")
    }
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
  module Prelude = {
    type t = {
      name: string,
      expr: Expr.t
    };
  };
  include Prelude;
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

type value =
  | Value_unit
  | Value_function(Function.t);

let run = (self) => {
  open Function.Prelude;

  let find = (arr, f) => {
    let rec helper = (idx) => {
      if (f(arr[idx])) { arr[idx] } else { helper(idx + 1) };
    };
    helper(0)
  };
  let rec call = (func) => {
    Printf.printf("calling %s\n", func.name);
    eval(func.expr)
  }
  and eval = (expr) => {
    switch expr {
    | Expr.Unit_literal => Value_unit
    | Expr.Variable(s) => Value_function(find(self.funcs, (f) => f.name == s))
    | Expr.Call(e) =>
      switch (eval(e)) {
      | Value_unit => assert false
      | Value_function(f) => call(f)
      }
    }
  };

  let main = find(self.funcs, (f) => f.name == "main");
  call(main) |> ignore;
  ()
};
