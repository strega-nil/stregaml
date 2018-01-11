module Error = Error;

module Spanned = Cafec_spanned;
module Untyped_ast = Cafec_parse.Ast;
open Spanned.Prelude;
open Error.Monad_spanned;

module Type = {
  module Ctxt: {
    type context;
    let make_context
      : list(Untyped_ast.Type_declaration.t)
        => spanned_result(context, Error.t);
  } = {
    type context = unit;
    let make_context = (_) => pure();
  };
  include Ctxt;

  type t =
    | Unit
    | Bool
    | Int;

  let unit_ = Unit;
  let bool_ = Bool;
  let int_ = Int;

  /* NOTE(ubsan): this should *actually* be error handling */
  let make
    : (Untyped_ast.Type.t, context) => spanned_result(t, Error.t)
    = (unt_ty, _ctxt) =>
  {
    module T = Untyped_ast.Type;
    switch unt_ty {
    | (T.Named(name), _) =>
      if (name == "unit") {
        pure(unit_)
      } else if (name == "bool") {
        pure(bool_)
      } else if (name == "int") {
        pure(int_)
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
  type expr =
    | Unit_literal
    | Bool_literal(bool)
    | Integer_literal(int)
    | If_else(expr, expr, expr)
    | Call(expr, list(expr))
    | Builtin(builtin)
    | Global_function(int)
    | Parameter(int);

  type decl = {
    params: list((string, Type.t)),
    ret_ty: Type.t
  };
  type func = {
    ty: spanned(decl),
    expr: spanned(expr)
  };

  module Context = {
    type t =
      | Context(list((string, spanned(decl))));

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
          pure([x, ...xs])
        | [] => pure([])
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
    switch (make_expr(unt_func.F.expr, ty, ctxt, ty_ctxt)) {
    | Ok(expr) => Ok(({ty, expr}, sp))
    | Error(e) => Error(e)
    }
  };
};

type t = {
  funcs: list(Value.func)
};

let make = unt_ast => {
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

let run = (self) => ();

/*
type builtin =
  | Builtin_add
  | Builtin_sub
  | Builtin_less_eq;

type value =
  | Value_unit
  | Value_bool(bool)
  | Value_integer(int)
  | Value_function(int)
  | Value_builtin(builtin);

let run = (self) => {
  let rec call = (func, args, ctxt) => {
    let callee_ctxt = ref([]);
    Iter.zip(List.iter(func.Value.params), List.iter(args))
    |> Iter.for_each(
      (((name, _), expr)) =>
        callee_ctxt := [(name, eval(expr, ctxt)), ...callee_ctxt^]
    );
    eval(func.Value.expr, callee_ctxt^)
  }
  and eval = ((expr, _), ctxt) => {
    switch expr {
    | Value.Unit_literal => Value_unit
    | Value.Bool_literal(b) => Value_bool(b)
    | Value.Integer_literal(n) => Value_integer(n)
    | Value.If_else(cond, thn, els) =>
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
*/
