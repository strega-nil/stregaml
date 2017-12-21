open Pred;

open Spanned.Prelude;

let rec print_indent = (indent) =>
  switch indent {
  | n when n <= 0 => ()
  | n =>
    print_string("  ");
    print_indent(n - 1);
  };

module Expr = {
  type builder = 
    | Unit_literal
    | Bool_literal(bool)
    | Integer_literal(int)
    | If_else(t, t, t)
    | Variable(string)
    | Call(t, array(t))
  and t = spanned(builder);

  let unit_literal = () => Unit_literal;
  let bool_literal = (b) => Bool_literal(b);
  let integer_literal = (n) => Integer_literal(n);
  let if_else = (cond, thn, els) => If_else(cond, thn, els);
  let variable = (s) => Variable(s);
  let call = (e, args) => Call(e, args);

  let rec print = ((e, _), indent) => {
    let rec print_nonempty_args = (args, idx) => {
      print(args[idx], indent + 1);
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
    | If_else(cond, thn, els) =>
      print_string("if (");
      print(cond, indent + 1);
      print_string(") {\n");
      print_indent(indent + 1);
      print(thn, indent + 1);
      print_char('\n');
      print_indent(indent);
      print_string("} else {\n");
      print_indent(indent + 1);
      print(els, indent + 1);
      print_char('\n');
      print_indent(indent);
      print_char('}');
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
  type builder =
    | Named(string)
  and t = spanned(builder);

  let named = (name) => Named(name);
  let print = ((self, _)) =>
    switch self {
    | Named(s) => print_string(s)
    };
};

module Type_definition = {
  type t = unit;
};

module Function = {
  module Prelude = {
    type builder = {
      name: string,
      params: array((string, Type.t)),
      expr: Expr.t
    };
  };
  include Prelude;
  type t = spanned(builder);

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
  let print = ((self, _)) => {
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

type builtin =
  | Builtin_add
  | Builtin_sub
  | Builtin_less_eq;

type value =
  | Value_unit
  | Value_bool(bool)
  | Value_integer(int)
  | Value_function(Function.builder)
  | Value_builtin(builtin);

let run = (self) => {
  open Function.Prelude;

  let rec find_in_ctxt = (name, ctxt) => {
    switch ctxt {
    | [(name', val'), ..._] when name' == name => val'
    | [_, ...xs] => find_in_ctxt(name, xs)
    | [] =>
      {
        if (name == "LESS_EQ") {
          Value_builtin(Builtin_less_eq);
        } else if (name == "ADD") {
          Value_builtin(Builtin_add);
        } else if (name == "SUB") {
          Value_builtin(Builtin_sub);
        } else {
          let funcs = self.funcs;
          let rec helper = (idx) => {
            if (idx == Array.length(funcs)) {
              print_string("\ncouldn't find " ++ name);
              assert false;
            };
            let (func, _) = funcs[idx];
            if (func.Function.name == name) {
              func
            } else {
              helper(idx + 1)
            };
          };
          Value_function(helper(0))
        }
      }
    }
  };
  let rec call = (func, args, ctxt) => {
    let callee_ctxt = ref([]);
    Iter.zip(Array.iter(func.params), Array.iter(args))
    |> Iter.for_each(
      (((name, _), expr)) => callee_ctxt := [(name, eval(expr, ctxt)), ...callee_ctxt^]
    );
    eval(func.expr, callee_ctxt^)
  }
  and eval = ((expr, _), ctxt) => {
    switch expr {
    | Expr.Unit_literal => Value_unit
    | Expr.Bool_literal(b) => Value_bool(b)
    | Expr.Integer_literal(n) => Value_integer(n)
    | Expr.If_else(cond, thn, els) =>
      switch (eval(cond, ctxt)) {
      | Value_bool(true) => eval(thn, ctxt)
      | Value_bool(false) => eval(els, ctxt)
      | _ => assert false
      }
    | Expr.Variable(s) => find_in_ctxt(s, ctxt)
    | Expr.Call(e, args) =>
      switch (eval(e, ctxt)) {
      | Value_function(f) => call(f, args, ctxt)
      | Value_builtin(b) =>
        assert(Array.length(args) == 2);
        let (lhs, rhs) = switch (eval(args[0], ctxt), eval(args[1], ctxt)) {
        | (Value_integer(lhs), Value_integer(rhs)) => (lhs, rhs)
        | _ => assert false
        };
        switch b {
        | Builtin_add => Value_integer(lhs + rhs)
        | Builtin_sub => Value_integer(lhs - rhs)
        | Builtin_less_eq => Value_bool(lhs <= rhs)
        }
      | _ => assert false
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
