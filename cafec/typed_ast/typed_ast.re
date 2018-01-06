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

module rec Expr: {
  type builtin =
    | Builtin_less_eq
    | Builtin_add
    | Builtin_sub;
  type builder =
    | Unit_literal
    | Bool_literal(bool)
    | Integer_literal(int)
    | If_else(t, t, t)
    | Call(t, list(t))
    | Builtin(builtin)
    | Global_function(int)
    | Parameter(int)
  and t = spanned(builder);

  let make
    : (Untyped_ast.Expr.t, Function.decl, Function.context, Type.context)
      => result(Expr.t, Error.t);
} = {
  type builtin =
    | Builtin_less_eq
    | Builtin_add
    | Builtin_sub;
  type builder =
    | Unit_literal
    | Bool_literal(bool)
    | Integer_literal(int)
    | If_else(t, t, t)
    | Call(t, list(t))
    | Builtin(builtin)
    | Global_function(int)
    | Parameter(int)
  and t = spanned(builder);

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

  let rec make = ((unt_expr, sp), decl, ctxt, ty_ctxt) => {
    let module E = Untyped_ast.Expr;
    switch unt_expr {
    | E.Unit_literal => Ok((Unit_literal, sp))
    | E.Bool_literal(b) => Ok((Bool_literal(b), sp))
    | E.Integer_literal(i) => Ok((Integer_literal(i), sp))
    | E.If_else(cond, thn, els) =>
      make(cond, decl, ctxt, ty_ctxt)
      >>= (cond) => make(thn, decl, ctxt, ty_ctxt)
      >>= (thn) => make(els, decl, ctxt, ty_ctxt)
      >>= (els) => Ok((If_else(cond, thn, els), sp))
    | E.Call(callee, args) =>
      make(callee, decl, ctxt, ty_ctxt)
      >>= (callee) => {
        let rec helper = (lst) =>
          switch lst {
          | [x, ...xs] =>
            make(x, decl, ctxt, ty_ctxt)
            >>= (x) => helper(xs)
            >>= (xs) => Ok([x, ...xs])
          | [] => Ok([])
          };
        helper(args);
      } >>= (args) => Ok((Call(callee, args), sp))
    | E.Variable(name) =>
      {
        let ({Function.params, _}, _) = decl;
        switch (find_in_parms(name, params)) {
        | None =>
          switch (Function.find_in_context(name, ctxt)) {
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
} and Function : {
  type decl_builder = {
    params: list((string, Type.t)),
    ret_ty: Type.t
  } and decl = spanned(decl_builder);
  type builder = {
    ty: decl,
    expr: Expr.t
  } and t = spanned(builder);

  type context;

  let find_in_context: (string, context) => option((decl, int));

  let make_context
    : (list(Untyped_ast.Function.t), Type.context) => result(context, Error.t);

  let make
    : (Untyped_ast.Function.t, context, Type.context) => result(t, Error.t);
} = {
  type decl_builder = {
    params: list((string, Type.t)),
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
      | [(name', dcl), ..._] when name == name' => Some((dcl, idx))
      | [_, ...xs] => helper(xs, idx + 1)
      | [] => None
      };
    helper(ctxt, 0)
  };

  let make_context = (funcs, ty_ctxt) => {
    let module F = Untyped_ast.Function;

    let rec helper = (funcs) =>
      switch funcs {
      | [] => pure([])
      | [({F.name, F.params, F.ret_ty, _}, sp), ...funcs] =>
        let rec get_params = (params) =>
          switch params {
          | [] => pure([])
          | [(name, ty), ...params] =>
            Type.make(ty, ty_ctxt)
            >>= (ty) => get_params(params)
            >>= (params) => pure([(name, ty), ...params])
          };
        let ret_ty = switch ret_ty {
        | None => pure(Type.unit_)
        | Some(ty) => Type.make(ty, ty_ctxt)
        };
        ret_ty
        >>= (ret_ty) => get_params(params)
        >>= (params) => {
          let dcl = ({params, ret_ty}, sp);
          helper(funcs)
          >>= (tl) => pure([(name, dcl), ...tl])
        }
      };
    helper(funcs)
    >>= (inner) => pure(Context(inner));
  };

  let make = ((unt_func, sp), ctxt, ty_ctxt) => {
    let module F = Untyped_ast.Function;

    let ty = switch (find_in_context(unt_func.F.name, ctxt)) {
    | Some((decl, _)) => decl
    | None => assert false
    };
    Expr.make(unt_func.F.expr, ty, ctxt, ty_ctxt)
    >>= (expr) => Ok(({ty, expr}, sp))
  };
};

type t = {
  funcs: list(Function.t)
};

let make = (unt_ast) => {
  let module U = Untyped_ast;
  Type.make_context([])
  >>= (ty_ctxt) => Function.make_context(unt_ast.U.funcs, ty_ctxt)
  >>= (func_ctxt) => {
    let rec helper = (funcs) =>
      switch funcs {
      | [func, ...funcs] =>
        Function.make(func, func_ctxt, ty_ctxt)
        >>= (func) => helper(funcs)
        >>= (funcs) => pure([func, ...funcs])
      | [] => pure([])
      };
    helper(unt_ast.U.funcs)
    >>= (funcs) => pure({funcs: funcs})
  }
};
