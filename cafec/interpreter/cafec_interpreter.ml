module Ast = Cafec_typed_ast
module Expr = Ast.Expr

(*
  NOTE(ubsan): NEVER modify these arrays.
  They should be immutable,
  but ocaml doesn't have immutable arrays
*)
type context = {funcs: (string * Expr.t) array}

type value =
  | Value_unit
  | Value_bool of bool
  | Value_integer of int
  | Value_function of int
  | Value_struct of value array
  | Value_builtin of Expr.builtin

let build_context ast =
  let seq = ref (Ast.function_seq ast) in
  let helper _ =
    match !seq () with
    | Seq.Nil -> assert false
    | Seq.Cons ((({Ast.fname; _}, _), (expr, _)), xs) ->
        seq := xs ;
        (fname, expr)
  in
  let ret = {funcs= Array.init (Ast.number_of_functions ast) helper} in
  match !seq () with Seq.Cons _ -> assert false | Seq.Nil -> ret


let function_expression_by_index ctxt idx =
  let _, expr = (ctxt.funcs).(idx) in
  expr


let function_index_by_name ctxt find =
  match Array.find (fun (name, _) -> name = find) ctxt.funcs with
  | None -> None
  | Some (n, _) -> Some n


let run ast =
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
          let expr = function_expression_by_index ctxt func in
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
            | Expr.Builtin_mul -> Value_integer (lhs * rhs)
            | Expr.Builtin_less_eq -> Value_bool (lhs <= rhs)
          in
          ret
      | _ -> assert false )
    | Expr.Builtin b -> Value_builtin b
    | Expr.Global_function i -> Value_function i
    | Expr.Struct_literal (_, members) ->
        let len = List.length members in
        let arr = Array.make len Value_unit in
        let f (i, (e, _)) = arr.(i) <- eval args ctxt e in
        Seq.for_each f (List.to_seq members) ;
        Value_struct arr
    | Expr.Struct_access ((e, _), idx) ->
        let v = eval args ctxt e in
        match v with
        | Value_struct members -> members.(idx)
        | _ -> assert false
  in
  let ctxt = build_context ast in
  match function_index_by_name ctxt "main" with
  | None -> print_endline "main not defined"
  | Some idx ->
      let main_expr = function_expression_by_index ctxt idx in
      match eval [] ctxt main_expr with
      | Value_integer n -> Printf.printf "main returned %d\n" n
      | Value_bool true -> print_endline "main returned true"
      | Value_bool false -> print_endline "main returned false"
      | _ -> assert false
