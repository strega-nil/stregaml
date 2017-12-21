open Spanned.Prelude;

module Expr: {
  type builder;
  type t = spanned(builder);
  let unit_literal: unit => builder;
  let integer_literal: int => builder;
  let bool_literal: bool => builder;
  let if_else: (t, t, t) => builder;
  let variable: string => builder;
  let call: (t, array(t)) => builder;
};

module Type: {
  type builder;
  type t = spanned(builder);
  let named: string => builder;
};

module Type_definition: {
  type t;
  /*let alias: (string, Type.t) => t;*/
};

module Function: {
  type builder;
  type t = spanned(builder);
  let make: (string, array((string, Type.t)), option(Type.t), Expr.t) => builder;
};

type t;

let make: (array(Function.t), array(Type_definition.t)) => t;

let print: t => unit;

let run: t => unit;
