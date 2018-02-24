module Error = Error
module Spanned = Cafec_spanned
module Untyped_ast = Cafec_parse.Ast
open Spanned.Prelude
open Error.Monad_spanned

let get_type _ctxt unt_ty =
  let module T = Untyped_ast.Type in
  let unt_ty, sp = unt_ty in
  let%bind (), _ = with_span sp in
  match unt_ty with T.Named name ->
    (match name with
    | "unit" -> wrap Type.Unit
    | "bool" -> wrap Type.Bool
    | "int" -> wrap Type.Int
    | _ -> wrap_err (Error.Type_not_found unt_ty))

type builtin = Builtin_less_eq | Builtin_add | Builtin_sub

type decl = {params: (string * Type.t) list; ret_ty: Type.t}

(* TODO(ubsan): add spans *)
type expr =
  | Unit_literal
  | Bool_literal of bool
  | Integer_literal of int
  | If_else of (expr * expr * expr)
  | Call of (expr * expr list)
  | Builtin of builtin
  | Global_function of func
  | Parameter of int

and func = {ty: decl spanned; expr: expr spanned}

type t =
  { types: (string * Type.t spanned) list
  ; funcs: (string * func spanned) list
  ; main: func option }

let find_function_by_name {funcs; _} name =
  let rec helper = function
    | (name', func) :: _ when name = name' -> Some func
    | _ :: xs -> helper xs
    | [] -> None
  in
  helper funcs

let type_of_expr e =
  let e, sp = e in
  let%bind (), _ = with_span sp in
  match e with
  | Unit_literal -> wrap Type.Unit
  | Bool_literal _ -> wrap Type.Bool
  | Integer_literal _ -> wrap Type.Int
  | If_else (_, _e1, _e2) -> assert false
    (*let t1 = type_of_expr e1 in
    let t2 = type_of_expr e2 in*)
  | Call _ -> assert false
  | Builtin _ -> assert false
  | Global_function _ -> assert false
  | Parameter _ -> assert false

let find_parameter name lst =
  let rec helper name lst idx =
    match lst with
    | [] -> None
    | (name', ty) :: _ when name' = name -> Some (ty, idx)
    | _ :: xs -> helper name xs (idx + 1)
  in
  helper name lst 0


let rec type_expression decl ast unt_expr =
  let module E = Untyped_ast.Expr in
  match%bind Ok unt_expr with
  | E.Unit_literal, _ -> wrap Unit_literal
  | E.Bool_literal b, _ -> wrap (Bool_literal b)
  | E.Integer_literal i, _ -> wrap (Integer_literal i)
  | E.If_else (cond, thn, els), _ ->
      let%bind cond, _ = type_expression decl ast cond in
      let%bind thn, _ = type_expression decl ast thn in
      let%bind els, _ = type_expression decl ast els in
      wrap (If_else (cond, thn, els))
  | E.Call (callee, args), _ ->
      let%bind callee, _ = type_expression decl ast callee in
      let rec helper = function
        | [] -> wrap []
        | x :: xs ->
            let%bind x, _ = type_expression decl ast x in
            let%bind xs, _ = helper xs in
            wrap (x :: xs)
      in
      let%bind args, _ = helper args in
      wrap (Call (callee, args))
  | E.Variable name, _ ->
      let {params; _} = decl in
      match find_parameter name params with
      | None -> (
        match find_function_by_name ast name with
        | None -> (
          match name with
          | "LESS_EQ" -> wrap (Builtin Builtin_less_eq)
          | "ADD" -> wrap (Builtin Builtin_add)
          | "SUB" -> wrap (Builtin Builtin_sub)
          | _ -> wrap_err (Error.Name_not_found name))
        | Some (e, _) -> wrap (Global_function e) )
      | Some (_ty, idx) -> wrap (Parameter idx)


let add_function unt_func (ast: t) : (t, Error.t) spanned_result =
  let module F = Untyped_ast.Function in
  let {F.name; _}, _ = unt_func in
  let%bind func =
    let%bind ty, ty_sp =
      let {F.params; F.ret_ty; _}, _ = unt_func in
      let rec get_params = function
        | [] -> wrap []
        | (name, ty) :: params ->
            let%bind ty, _ = get_type ast.types ty in
            let%bind params, _ = get_params params in
            wrap ((name, ty) :: params)
      in
      let%bind ret_ty, _ =
        match ret_ty with
        | None -> wrap Type.Unit
        | Some ty -> get_type ast.types ty
      in
      let%bind params, _ = get_params params in
      wrap {params; ret_ty}
    in
    let unt_func, _ = unt_func in
    match type_expression ty ast unt_func.F.expr with
    | Ok expr ->
      let%bind te, _ = type_of_expr expr in
      if te = ty.ret_ty then
        wrap {ty= (ty, ty_sp); expr}
      else
        wrap_err Error.(
          Return_type_mismatch
            {func_name= name; expected= ty.ret_ty; found= te})
    | Error e -> Error e
  in
  let ast_with_f = {ast with funcs= (name, func) :: ast.funcs} in
  if name = "main" then
    let func, _ = func in
    if ast_with_f.main = None then wrap {ast_with_f with main= Some func}
    else assert false
  else wrap ast_with_f


let make unt_ast =
  let module U = Untyped_ast in
  let empty_ast = wrap {funcs= []; types= []; main= None} in
  let rec helper ast = function
    | (unt_func: U.Function.t) :: funcs ->
        let%bind ast, _ = ast in
        let new_ast = add_function unt_func ast in
        helper new_ast funcs
    | [] -> ast
  in
  helper empty_ast unt_ast.U.funcs


type value =
  | Value_unit
  | Value_bool of bool
  | Value_integer of int
  | Value_function of func
  | Value_builtin of builtin

let run self =
  let rec eval args ctxt = function
    | Unit_literal -> Value_unit
    | Bool_literal b -> Value_bool b
    | Integer_literal n -> Value_integer n
    | If_else (cond, thn, els) -> (
      match eval args ctxt cond with
      | Value_bool true -> eval args ctxt thn
      | Value_bool false -> eval args ctxt els
      | _ -> assert false )
    | Parameter i -> List.nth_exn i args
    | Call (e, args') -> (
      match eval args ctxt e with
      | Value_function func ->
          let expr, _ = func.expr in
          let args' = List.map (fun e -> eval args ctxt e) args' in
          eval args' ctxt expr
      | Value_builtin b ->
          let lhs, rhs =
            match args' with [lhs; rhs] -> (lhs, rhs) | _ -> assert false
          in
          let lhs =
            match eval args ctxt lhs with
            | Value_integer v -> v
            | _ -> assert false
          in
          let rhs =
            match eval args ctxt rhs with
            | Value_integer v -> v
            | _ -> assert false
          in
          let ret =
            match b with
            | Builtin_add -> Value_integer (lhs + rhs)
            | Builtin_sub -> Value_integer (lhs - rhs)
            | Builtin_less_eq -> Value_bool (lhs <= rhs)
          in
          ret
      | _ -> assert false )
    | Builtin b -> Value_builtin b
    | Global_function i -> Value_function i
  in
  match self.main with
  | None -> print_endline "main not defined"
  | Some main ->
      let main_expr, _ = main.expr in
      match eval [] self.funcs main_expr with
      | Value_integer n -> Printf.printf "main returned %d\n" n
      | Value_bool true -> print_endline "main returned true"
      | Value_bool false -> print_endline "main returned false"
      | _ -> assert false
