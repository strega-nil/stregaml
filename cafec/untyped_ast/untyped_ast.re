open Pred;

open Spanned.Prelude;

open Impl_;

module Expr = {
  type builder =
    | Unit_literal
    | Bool_literal(bool)
    | Integer_literal(int)
    | If_else(t, t, t)
    | Variable(string)
    | Call(t, list(t))
  and t = spanned(builder);

  let rec print = ((e, _), indent) => {
    let rec print_args = (args, start) =>
      switch args {
      | [arg, ...args] =>
        if (!start) {
          print_string(", ");
        };
        print(arg, indent + 1);
        print_args(args, false);
      | [] => ()
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
      print_char('('); print_args(args, true); print_char(')');
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

module Type_declaration = {
  type t;
};

module Function = {
  type builder = {
    name: string,
    params: list((string, Type.t)),
    ret_ty: option(Type.t),
    expr: Expr.t
  };
  type t = spanned(builder);

  let make = (name, params, ret_ty, expr) => {name, params, ret_ty, expr};
  let print = ((self, _)) => {
    let print_parameter_list = (lst) => {
      let rec helper = (lst, start) =>
        switch lst {
        | [(name, ty), ...xs] =>
          if (!start) {
            print_string(", ");
          };
          print_string(name);
          print_string(": ");
          Type.print(ty);
          helper(xs, false);
        | [] => ()
        };
      print_char('('); helper(lst, true); print_char(')');
    };
    print_string("func ");
    print_string(self.name);
    print_parameter_list(self.params);
    switch (self.ret_ty) {
    | Some(ty) => print_string(" -> "); Type.print(ty)
    | None => print_char(' ');
    };
    print_string(" {\n");
    print_indent(1);
    Expr.print(self.expr, 1);
    print_string("\n};\n");
  };
};

type t = {
  funcs: list(Function.t)
};

let make = (funcs) => {funcs: funcs};

let print = (self) => {
  let rec helper = (funcs) =>
    switch funcs {
    | [x, ...xs] => Function.print(x); helper(xs)
    | [] => ()
    };
  helper(self.funcs);
};

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
  let rec find_in_ctxt = (name, ctxt) => {
    switch ctxt {
    | [(name', v), ..._] when name' == name => v
    | [_, ...xs] => find_in_ctxt(name, xs)
    | [] =>
      if (name == "LESS_EQ") {
        Value_builtin(Builtin_less_eq);
      } else if (name == "ADD") {
        Value_builtin(Builtin_add);
      } else if (name == "SUB") {
        Value_builtin(Builtin_sub);
      } else {
        let rec helper = (funcs) => {
          switch funcs {
          | [] =>
            print_string("\ncouldn't find " ++ name);
            assert false;
          | [(func, _), ...xs] =>
            if (func.Function.name == name) {
              func
            } else {
              helper(xs)
            };
          }
        };
        Value_function(helper(self.funcs))
      }
    }
  };
  let rec call = (func, args, ctxt) => {
    let callee_ctxt = ref([]);
    Iter.zip(List.iter(func.Function.params), List.iter(args))
    |> Iter.for_each(
      (((name, _), expr)) =>
        callee_ctxt := [(name, eval(expr, ctxt)), ...callee_ctxt^]
    );
    eval(func.Function.expr, callee_ctxt^)
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
        let (a0, a1) = switch args {
        | [a0, a1] => (a0, a1)
        | _ => assert false
        };
        let (lhs, rhs) = switch (eval(a0, ctxt), eval(a1, ctxt)) {
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
  switch (call(main, [], [])) {
  | Value_integer(n) => Printf.printf("main returned %d\n", n)
  | Value_bool(true) => print_string("main returned true\n")
  | Value_bool(false) => print_string("main returned false\n")
  | _ => assert false
  }
};
