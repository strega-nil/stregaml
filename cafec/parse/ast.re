module Spanned = Cafec_spanned;

open Spanned.Prelude;

module Impl = {
  let rec print_indent = (indent) =>
    switch indent {
    | n when n <= 0 => ()
    | n =>
      print_string("  ");
      print_indent(n - 1);
    };
};
open Impl;

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
