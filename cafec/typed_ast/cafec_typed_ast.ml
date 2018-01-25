module Error = Error

module Spanned = Cafec_spanned
module Untyped_ast = Cafec_parse.Ast
open Spanned.Prelude
open Error.Monad_spanned

module Type = struct
  module Ctxt: sig
    type context
    val make_context
      : Untyped_ast.Type_declaration.t list
      -> (context, Error.t) spanned_result
  end = struct
    type context = unit
    let make_context _ = wrap ()
  end
  include Ctxt

  type t =
  | Unit
  | Bool
  | Int

  (* NOTE(ubsan): this should *actually* be error handling *)
  let make
    (unt_ty: Untyped_ast.Type.t)
    (_ctxt: context)
    : (t, Error.t) spanned_result
  =
    let module T = Untyped_ast.Type in
    match unt_ty with
    | (T.Named name, _) ->
      if name = "unit" then
        wrap Unit
      else if name = "bool" then
        wrap Bool
      else if name = "int" then
        wrap Int
      else
        assert false
end

module Value = struct
  type builtin =
  | Builtin_less_eq
  | Builtin_add
  | Builtin_sub
  type expr =
  | Unit_literal
  | Bool_literal of bool
  | Integer_literal of int
  | If_else of (expr * expr * expr)
  | Call of (expr * expr list)
  | Builtin of builtin
  | Global_function of int
  | Parameter of int

  type decl = {
    params: (string * Type.t) list;
    ret_ty: Type.t;
  }
  type func = {
    ty: decl spanned;
    expr: expr spanned;
  }

  module Context = struct
    type t =
    | Context of (string * decl spanned) list

    let find name (Context ctxt) =
      let rec helper ctxt idx =
        match ctxt with
        | ((name', dcl) :: _) when name = name' -> Some (dcl, idx)
        | (_ :: xs) -> helper xs (idx + 1)
        | [] -> None
      in
      helper ctxt 0

    let make funcs ty_ctxt =
      let module F = Untyped_ast.Function in

      let rec helper funcs =
        match funcs with
        | [] -> wrap []
        | (({F.name; F.params; F.ret_ty; _}, sp) :: funcs) ->
          let rec get_params = function
          | [] -> wrap []
          | ((name, ty) :: params) ->
            let%bind ty = Type.make ty ty_ctxt in
            let%bind params = get_params params in
            wrap ((name, ty) :: params)
          in
          let%bind ret_ty = match ret_ty with
          | None -> wrap Type.Unit
          | Some ty -> Type.make ty ty_ctxt
          in
          let%bind params = get_params params in
          let dcl = ({params; ret_ty}, sp) in
          let%bind tl = helper funcs in
          wrap ((name, dcl) :: tl)
      in
      let%bind inner = helper funcs in
      wrap (Context inner)
  end

  let find_in_parms name lst =
    let rec helper name lst idx =
      match lst with
      | [] -> None
      | ((name', ty) :: _) when name' = name -> Some (ty, idx)
      | (_ :: xs) -> helper name xs (idx + 1)
    in
    helper name lst 0

  let rec make_expr (unt_expr, sp) decl ctxt ty_ctxt =
    let module E = Untyped_ast.Expr in
    match unt_expr with
    | E.Unit_literal -> Ok (Unit_literal, sp)
    | E.Bool_literal b -> Ok (Bool_literal b, sp)
    | E.Integer_literal i -> Ok (Integer_literal i, sp)
    | E.If_else (cond, thn, els) ->
      let%bind cond = make_expr cond decl ctxt ty_ctxt in
      let%bind thn = make_expr thn decl ctxt ty_ctxt in
      let%bind els = make_expr els decl ctxt ty_ctxt in
      Ok (If_else (cond, thn, els), sp)
    | E.Call (callee, args) ->
      let%bind callee = make_expr callee decl ctxt ty_ctxt in
      let rec helper = function
      | [] -> wrap []
      | (x :: xs) ->
        let%bind x = make_expr x decl ctxt ty_ctxt in
        let%bind xs = helper xs in
        wrap (x :: xs)
      in
      let%bind args = helper args in
      Ok (Call (callee, args), sp)
    | E.Variable name ->
      let ({params; _}, _) = decl in
      (match find_in_parms name params with
      | None ->
        (match  Context.find name ctxt with
        | None ->
          (match name with
          | "LESS_EQ" -> Ok (Builtin Builtin_less_eq, sp)
          | "ADD" -> Ok (Builtin Builtin_add, sp)
          | "SUB" -> Ok (Builtin Builtin_sub, sp)
          | _ -> assert false)
        | Some (_dcl, idx) -> Ok (Global_function idx, sp))
      | Some (_ty, idx) -> Ok (Parameter idx, sp))

  let make_func (unt_func, sp) ctxt ty_ctxt =
    let module F = Untyped_ast.Function in

    let ty = match Context.find unt_func.F.name ctxt with
    | Some (decl, _) -> decl
    | None -> assert false
    in
    match make_expr unt_func.F.expr ty ctxt ty_ctxt with
    | Ok expr -> Ok ({ty; expr}, sp)
    | Error e -> Error e
end

type t = {
  funcs: Value.func list;
}

let make unt_ast =
  let module U = Untyped_ast in
  let%bind ty_ctxt = Type.make_context [] in
  let%bind func_ctxt = Value.Context.make unt_ast.U.funcs ty_ctxt in
  let rec helper = function
  | (func :: funcs) ->
    let%bind func = Value.make_func func func_ctxt ty_ctxt in
    let%bind funcs = helper funcs in
    wrap (func :: funcs)
  | [] -> wrap []
  in
  let%bind funcs = helper unt_ast.U.funcs in
  wrap {funcs}

let run _self = ()

(*
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
*)
