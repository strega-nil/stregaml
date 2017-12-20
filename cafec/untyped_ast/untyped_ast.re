open Pred;

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
    | Bool_literal(bool)
    | Integer_literal(int)
    | Variable(string)
    | Call(t, array(t));
  let unit_literal = () => Unit_literal;
  let bool_literal = (b) => Bool_literal(b);
  let integer_literal = (n) => Integer_literal(n);
  let variable = (s) => Variable(s);
  let call = (e, args) => Call(e, args);

  let rec print = (e, indent) => {
    let rec print_nonempty_args = (args, idx) => {
      print(args[idx], indent);
      if (Array.length(args) < idx + 1) {
        print_string(", ");
        print_nonempty_args(args, idx + 1);
      } else {
        ()
      }
    };
    switch (e) {
    | Unit_literal => print_string("()")
    | Bool_literal(true) => print_string("true")
    | Bool_literal(false) => print_string("false")
    | Integer_literal(n) => print_int(n)
    | Variable(s) => print_string(s)
    | Call(e, args) =>
      print(e, indent);
      if (Array.length(args) == 0) {
        print_string("()")
      } else {
        print_char('('); print_nonempty_args(args, 0); print_char(')');
      }
    }
  };
};

module Type = {
  type t =
    | Named(string);
  let named = (name) => Named(name);
  let print = (self) =>
    switch self {
    | Named(s) => print_string(s)
    };
};

module Type_definition = {
  type t = unit;
};

module Function = {
  module Prelude = {
    type t = {
      name: string,
      params: array((string, Type.t)),
      expr: Expr.t
    };
  };
  include Prelude;
  let make = (name, params, expr) => {name, params, expr};
  let print_parameter_list = (arr) => {
    let rec helper = (idx) => {
      let (name, ty) = arr[idx];
      print_string(name);
      print_string(": ");
      Type.print(ty);
      if (Array.length(arr) < idx + 1) {
        print_string(", ");
        helper(idx + 1);
      } else {
        ()
      }
    };
    if (Array.length(arr) == 0) {
      print_string("()");
    } else {
      print_char('('); helper(0); print_char(')');
    }
  };
  let print = (self) => {
    print_string("func ");
    print_string(self.name);
    print_parameter_list(self.params);
    print_string(" {\n");
    print_indent(1);
    Expr.print(self.expr, 1);
    print_string("\n};\n");
  };
};

type t = {
  funcs: array(Function.t),
  tys: array(Type_definition.t)
};

let make = (funcs, tys) => {funcs, tys};

let print = (self) => Array.iter(self.funcs) |> Iter.for_each(Function.print);

type value =
  | Value_unit
  | Value_bool(bool)
  | Value_integer(int)
  | Value_function(Function.t);

let run = (self) => {
  open Function.Prelude;

  let rec find_in_ctxt = (name, ctxt) => {
    switch ctxt {
    | [(name', val'), ..._] when name' == name => val'
    | [_, ...xs] => find_in_ctxt(name, xs)
    | [] =>
      {
        let funcs = self.funcs;
        let rec helper = (idx) => {
          if (funcs[idx].Function.name == name) {
            funcs[idx]
          } else {
            helper(idx + 1)
          };
        };
        Value_function(helper(0))
      }
    }
  };
  let rec call = (func, args, ctxt) => {
    Printf.printf("calling %s\n", func.name);
    let callee_ctxt = ref([]);
    Iter.zip(Array.iter(func.params), Array.iter(args))
    |> Iter.for_each(
      (((name, _), expr)) => callee_ctxt := [(name, eval(expr, ctxt)), ...callee_ctxt^]
    );
    eval(func.expr, callee_ctxt^)
  }
  and eval = (expr, ctxt) => {
    switch expr {
    | Expr.Unit_literal => Value_unit
    | Expr.Bool_literal(b) => Value_bool(b)
    | Expr.Integer_literal(n) => Value_integer(n)
    | Expr.Variable(s) => find_in_ctxt(s, ctxt)
    | Expr.Call(e, args) =>
      switch (eval(e, ctxt)) {
      | Value_unit => assert false
      | Value_bool(_) => assert false
      | Value_integer(_) => assert false
      | Value_function(f) => call(f, args, ctxt)
      }
    }
  };

  let main = switch(find_in_ctxt("main", [])) {
  | Value_function(f) => f
  | _ => assert false
  };
  switch (call(main, [||], [])) {
  | Value_integer(n) => Printf.printf("main returned %d\n", n)
  | Value_bool(true) => print_string("main returned true\n")
  | Value_bool(false) => print_string("main returned false\n")
  | _ => assert false
  }
};
