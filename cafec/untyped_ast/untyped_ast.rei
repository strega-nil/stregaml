module Expr: {
  type t;
  let unit_literal: unit => t;
  let integer_literal: int => t;
  let bool_literal: bool => t;
  let if_else: (t, t, t) => t;
  let variable: string => t;
  let call: t => array(t) => t;
};

module Type: {
  type t;
  let named: string => t;
};

module Type_definition: {
  type t;
  /*let alias: (string, Type.t) => t;*/
};

module Function: {
  type t;
  let make: (string, array((string, Type.t)), Expr.t) => t;
};

type t;

let make: (array(Function.t), array(Type_definition.t)) => t;

let print: t => unit;

let run: t => unit;
