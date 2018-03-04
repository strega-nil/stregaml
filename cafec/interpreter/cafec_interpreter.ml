module Ast = Cafec_typed_ast
module Expr = Ast.Expr

type value =
  | Value_unit
  | Value_bool of bool
  | Value_integer of int
  | Value_function of Expr.function_index
  | Value_builtin of Expr.builtin

let run ctxt =
  let rec eval args ctxt = function
    | Expr.Unit_literal -> Value_unit
    | Expr.Bool_literal b -> Value_bool b
    | Expr.Integer_literal n -> Value_integer n
    | Expr.If_else ((cond, _), (thn, _), (els, _)) -> (
      match eval args ctxt cond with
      | Value_bool true -> eval args ctxt thn
      | Value_bool false -> eval args ctxt els
      | _ -> assert false )
    | Expr.Parameter i -> List.nth_exn i args
    | Expr.Call ((e, _), args') -> (
      match eval args ctxt e with
      | Value_function func ->
          let {Ast.expr; Ast.name; _} = Ast.function_by_index ctxt func in
          let args' = List.map (fun (e, _) -> eval args ctxt e) args' in
          eval args' ctxt expr
      | Value_builtin b ->
          let (lhs, _), (rhs, _) =
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
            | Expr.Builtin_add -> Value_integer (lhs + rhs)
            | Expr.Builtin_sub -> Value_integer (lhs - rhs)
            | Expr.Builtin_less_eq -> Value_bool (lhs <= rhs)
          in
          ret
      | _ -> assert false )
    | Expr.Builtin b -> Value_builtin b
    | Expr.Global_function i -> Value_function i
  in
  match Ast.function_index_by_name ctxt "main" with
  | None -> print_endline "main not defined"
  | Some idx ->
      let {Ast.expr= main_expr; _} = Ast.function_by_index ctxt idx in
      match eval [] ctxt main_expr with
      | Value_integer n -> Printf.printf "main returned %d\n" n
      | Value_bool true -> print_endline "main returned true"
      | Value_bool false -> print_endline "main returned false"
      | _ -> assert false
