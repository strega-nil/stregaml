open Pred;

open Spanned.Prelude;
open Result.Monad;

module Type = {
  module Ctxt: {
    type context;
    let make_context
      : array(Untyped_ast.Type_declaration.t) => result(context, Error.t);
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
    | Call(t, array(t))
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
    | Call(t, array(t))
    | Global_function(int)
    | Parameter(int)
  and t = spanned(builder);

  let make = (unt_expr, ctxt, ty_ctxt) => assert false;
} and Function : {
  type decl_builder = {
    params: array(Type.t),
    ret_ty: Type.t
  } and decl = spanned(decl_builder);
  type builder = {
    ty: decl,
    expr: Expr.t
  } and t = spanned(builder);

  type context;

  let find_in_context: (string, context) => option((int, decl));

  let make_context
    : (array(Untyped_ast.Function.t), Type.context) => result(context, Error.t);

  let make
    : (Untyped_ast.Function.t, context, Type.context) => result(t, Error.t);
} = {
  type decl_builder = {
    params: array(Type.t),
    ret_ty: Type.t
  } and decl = spanned(decl_builder);
  type builder = {
    ty: decl,
    expr: Expr.t
  } and t = spanned(builder);

  /* implemented this way, and not as just an alias, due to typeck bugs */
  type context =
    | Context(array((string, decl)));

  let find_in_context = (name, Context(ctxt)) => {
    Array.iter(ctxt)
    |> Iter.enumerate
    |> Iter.for_each_break(
      ((i, (name', dcl))) => {
        if (name == name') {
          Some((i, dcl))
        } else {
          None
        }
      })
  };

  let make_context = (funcs, ty_ctxt) => {
    let module F = Untyped_ast.Function;

    let dummy = ("", ({params: [||], ret_ty: Type.unit_}, Spanned.made_up));
    let ret = Array.make(Array.length(funcs), dummy);
    let err =
      Array.iter(funcs)
      |> Iter.enumerate
      |> Iter.for_each_break(((i, (func, sp))) => {
        let {F.name, F.params, F.ret_ty, _} = func;
        let err =
          Iter.range(0, i)
          |> Iter.for_each_break(j => {
            let (decl, old_sp) = funcs[j];
            if (name == decl.F.name) {
              Some((Error.Multiple_function_definitions(name, old_sp), sp));
            } else {
              None
            }
          });
        switch err {
        | Some(err) => Some(err)
        | None => {
          let ret_ty = switch ret_ty {
          | Some(ty) =>
            switch (Type.make(ty, ty_ctxt)) {
            | Ok(ty) => Ok(ty)
            | Err((e, sp)) => Err((e, sp))
            }
          | None => Ok(Type.unit_)
          };
          switch ret_ty {
          | Err(e) => Some(e)
          | Ok(ret_ty) => {
            let (err, params) = {
              let params' = Array.make(Array.length(params), Type.unit_);
              let err =
                Array.iter(params)
                |> Iter.enumerate
                |> Iter.for_each_break(((j, (_name, param))) => {
                  switch (Type.make(param, ty_ctxt)) {
                  | Ok(ty) => params'[j] = ty; None
                  | Err((e, sp)) => Some((e, sp))
                  }
                });
              (err, params')
            };

            switch err {
            | Some(e) => Some(e)
            | None =>
              ret[i] = (name, ({params, ret_ty}, sp));
              None
            }
          }}
        }}
      });
    switch err {
    | Some((e, sp)) => Err((e, sp))
    | None => Ok(Context(ret))
    }
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
  funcs: array(Function.t)
};

let make = (unt_ast) => {
  let module U = Untyped_ast;
  Type.make_context([||])
  >>= (ty_ctxt) => Function.make_context(unt_ast.U.funcs, ty_ctxt)
  >>= (func_ctxt) =>
  assert false;
};
