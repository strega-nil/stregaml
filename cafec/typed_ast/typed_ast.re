open Pred;

open Spanned.Prelude;
open Result.Monad;

module Type = {
  module Ctxt: {
    type context;
    let make_context
      : list(Untyped_ast.Type_declaration.t) => result(context, Error.t);
  } = {
    type context = unit;
    let make_context = (_) => Ok();
  };
  include Ctxt;

  type builder =
    | Unit
    | Bool
    | Int
  and t = spanned(builder);

  let unit_ = (Unit, Spanned.made_up);
  let bool_ = (Bool, Spanned.made_up);
  let int_ = (Int, Spanned.made_up);

  /* NOTE(ubsan): this should *actually* be error handling */
  let make
    : (Untyped_ast.Type.t, context) => result(t, Error.t)
    = (unt_ty, _ctxt) =>
  {
    module T = Untyped_ast.Type;
    switch unt_ty {
    | (T.Named(name), _) =>
      if (name == "unit") {
        Ok(unit_)
      } else if (name == "bool") {
        Ok(bool_)
      } else if (name == "int") {
        Ok(int_)
      } else {
        assert false
      }
    }
  };
};

module rec Expr: {
  type builder =
    | Unit_literal
    | Bool_literal(bool)
    | Integer_literal(int)
    | If_else(t, t, t)
    | Call(t, list(t))
    | Global_function(int)
    | Parameter(int)
  and t = spanned(builder);

  let make
    : (Untyped_ast.Expr.t, Function.context, Type.context)
      => result(Expr.t, Error.t);
} = {
  type builder =
    | Unit_literal
    | Bool_literal(bool)
    | Integer_literal(int)
    | If_else(t, t, t)
    | Call(t, list(t))
    | Global_function(int)
    | Parameter(int)
  and t = spanned(builder);

  let make = (unt_expr, ctxt, ty_ctxt) => assert false;
} and Function : {
  type decl_builder = {
    params: list(Type.t),
    ret_ty: Type.t
  } and decl = spanned(decl_builder);
  type builder = {
    ty: decl,
    expr: Expr.t
  } and t = spanned(builder);

  type context;

  let find_in_context: (string, context) => option((int, decl));

  let make_context
    : (list(Untyped_ast.Function.t), Type.context) => result(context, Error.t);

  let make
    : (Untyped_ast.Function.t, context, Type.context) => result(t, Error.t);
} = {
  type decl_builder = {
    params: list(Type.t),
    ret_ty: Type.t
  } and decl = spanned(decl_builder);
  type builder = {
    ty: decl,
    expr: Expr.t
  } and t = spanned(builder);

  /* implemented this way, and not as just an alias, due to typeck bugs */
  type context =
    | Context(list((string, decl)));

  let find_in_context = (name, Context(ctxt)) => {
    let rec helper = (ctxt, idx) =>
      switch ctxt {
      | [(name', dcl), ..._] when name == name' => Some((idx, dcl))
      | [_, ...xs] => helper(xs, idx + 1)
      | [] => None
      };
    helper(ctxt, 0)
  };

  let make_context = (_funcs, _ty_ctxt) => {
    let module F = Untyped_ast.Function;

    assert false;
  };

  let make = ((unt_func, sp), ctxt, ty_ctxt) => {
    let module F = Untyped_ast.Function;

    let _decl = switch (find_in_context(unt_func.F.name, ctxt)) {
    | Some((_, decl)) => decl
    | None => assert false
    };
    let _expr = Expr.make(unt_func.F.expr, ctxt, ty_ctxt);

    assert false;
  };
};

type t = {
  funcs: list(Function.t)
};

let make = (unt_ast) => {
  let module U = Untyped_ast;
  Type.make_context([])
  >>= (ty_ctxt) => Function.make_context(unt_ast.U.funcs, ty_ctxt)
  >>= (func_ctxt) =>
  assert false;
};
