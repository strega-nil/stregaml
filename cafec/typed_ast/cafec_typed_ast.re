module Spanned = Cafec_spanned;
module Untyped_ast = Cafec_untyped_ast;
open Spanned.Prelude;
open Error.Monad_result;

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

module Value {
  type builtin =
    | Builtin_less_eq
    | Builtin_add
    | Builtin_sub;
  type expr_builder =
    | Unit_literal
    | Bool_literal(bool)
    | Integer_literal(int)
    | If_else(expr, expr, expr)
    | Call(expr, list(expr))
    | Builtin(builtin)
    | Global_function(int)
    | Parameter(int)
  and expr = spanned(expr_builder);

  type decl_builder = {
    params: list((string, Type.t)),
    ret_ty: Type.t
  } and decl = spanned(decl_builder);
  type func_builder = {
    ty: decl,
    expr: expr
  } and func = spanned(func_builder);

  module Context = {
    type t =
      | Context(list((string, decl)));

    let find = (name, Context(ctxt)) => {
      let rec helper = (ctxt, idx) =>
        switch ctxt {
        | [(name', dcl), ..._] when name == name' => Some((dcl, idx))
        | [_, ...xs] => helper(xs, idx + 1)
        | [] => None
        };
      helper(ctxt, 0)
    };

    let make = (funcs, ty_ctxt) => {
      let module F = Untyped_ast.Function;

      let rec helper = (funcs) =>
        switch funcs {
        | [] => pure([])
        | [({F.name, F.params, F.ret_ty, _}, sp), ...funcs] =>
          let rec get_params = (params) =>
            switch params {
            | [] => pure([])
            | [(name, ty), ...params] =>
              let%bind ty = Type.make(ty, ty_ctxt);
              let%bind params = get_params(params);
              pure([(name, ty), ...params])
            };
          let%bind ret_ty = switch ret_ty {
          | None => pure(Type.unit_)
          | Some(ty) => Type.make(ty, ty_ctxt)
          };
          let%bind params = get_params(params);
          let dcl = ({params, ret_ty}, sp);
          let%bind tl = helper(funcs);
          pure([(name, dcl), ...tl])
        };
      let%bind inner = helper(funcs);
      pure(Context(inner));
    };
  };

  let find_in_parms = (name, lst) => {
    let rec helper = (name, lst, idx) =>
      switch lst {
      | [] => None
      | [(name', ty), ..._] when name' == name => Some((ty, idx))
      | [_, ...xs] =>
        helper(name, xs, idx + 1)
      };
    helper(name, lst, 0)
  };

  let rec make_expr = ((unt_expr, sp), decl, ctxt, ty_ctxt) => {
    let module E = Untyped_ast.Expr;
    switch unt_expr {
    | E.Unit_literal => Ok((Unit_literal, sp))
    | E.Bool_literal(b) => Ok((Bool_literal(b), sp))
    | E.Integer_literal(i) => Ok((Integer_literal(i), sp))
    | E.If_else(cond, thn, els) =>
      let%bind cond = make_expr(cond, decl, ctxt, ty_ctxt);
      let%bind thn = make_expr(thn, decl, ctxt, ty_ctxt);
      let%bind els = make_expr(els, decl, ctxt, ty_ctxt);
      Ok((If_else(cond, thn, els), sp))
    | E.Call(callee, args) =>
      let%bind callee = make_expr(callee, decl, ctxt, ty_ctxt);
      let rec helper = (lst) =>
        switch lst {
        | [x, ...xs] =>
          let%bind x = make_expr(x, decl, ctxt, ty_ctxt);
          let%bind xs = helper(xs);
          Ok([x, ...xs])
        | [] => Ok([])
        };
      let%bind args = helper(args);
      Ok((Call(callee, args), sp))
    | E.Variable(name) =>
      {
        let ({params, _}, _) = decl;
        switch (find_in_parms(name, params)) {
        | None =>
          switch (Context.find(name, ctxt)) {
          | None =>
            switch name {
            | "LESS_EQ" => Ok((Builtin(Builtin_less_eq), sp))
            | "ADD" => Ok((Builtin(Builtin_add), sp))
            | "SUB" => Ok((Builtin(Builtin_sub), sp))
            | _ => assert false
            }
          | Some((_dcl, idx)) => Ok((Global_function(idx), sp))
          }
        | Some((_ty, idx)) => Ok((Parameter(idx), sp))
        }
      }
    }
  };

  let make_func = ((unt_func, sp), ctxt, ty_ctxt) => {
    let module F = Untyped_ast.Function;

    let ty = switch (Context.find(unt_func.F.name, ctxt)) {
    | Some((decl, _)) => decl
    | None => assert false
    };
    let%bind expr = make_expr(unt_func.F.expr, ty, ctxt, ty_ctxt);
    Ok(({ty, expr}, sp))
  };
};

type t = {
  funcs: list(Value.func)
};

let make = (unt_ast) => {
  let module U = Untyped_ast;
  let%bind ty_ctxt = Type.make_context([]);
  let%bind func_ctxt = Value.Context.make(unt_ast.U.funcs, ty_ctxt);
  let rec helper = (funcs) =>
    switch funcs {
    | [func, ...funcs] =>
      let%bind func = Value.make_func(func, func_ctxt, ty_ctxt);
      let%bind funcs = helper(funcs);
      pure([func, ...funcs])
    | [] => pure([])
    };
  let%bind funcs = helper(unt_ast.U.funcs);
  pure({funcs: funcs})
};
