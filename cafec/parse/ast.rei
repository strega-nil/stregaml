open Cafec_spanned.Prelude;

module Type_declaration: {
  type t;
};

module Type: {
  type builder =
  | Named(string)
  and t = spanned(builder);
};

module Expr: {
  type builder =
  | Unit_literal
  | Bool_literal(bool)
  | Integer_literal(int)
  | If_else(t, t, t)
  | Variable(string)
  | Call(t, list(t))
  and t = spanned(builder);
};

module Function: {
  type builder = {
    name: string,
    params: list((string, Type.t)),
    ret_ty: option(Type.t),
    expr: Expr.t
  } and t = spanned(builder);
};

type t = {
  funcs: list(Function.t)
};

let make: list(Function.t) => t;

let print: t => unit;
