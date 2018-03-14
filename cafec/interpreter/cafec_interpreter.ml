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
    match Sequence.next !seq with
    | None -> assert false
    | Some ((({Ast.fname; _}, _), (expr, _)), rest) ->
        seq := rest ;
        (fname, expr)
  in
  {funcs= Array.init (Ast.number_of_functions ast) ~f:helper}


let function_expression_by_index ctxt idx =
  let _, expr = (ctxt.funcs).(idx) in
  expr


let function_index_by_name ctxt find =
  match
    Array.findi ctxt.funcs ~f:(fun _ (name, _) -> String.equal name find)
  with
  | None -> None
  | Some (n, _) -> Some n


module Out = Stdio.Out_channel

let rec output_value f ctxt = function
  | Value_unit -> Out.output_string f "()"
  | Value_integer n -> Out.fprintf f "%d" n
  | Value_bool true -> Out.output_string f "true"
  | Value_bool false -> Out.output_string f "false"
  | Value_struct arr ->
      Out.output_string f "{" ;
      if not (Array.is_empty arr) then
        Array.iter arr ~f:(fun el ->
            Out.output_string f "; " ; output_value f ctxt el ) ;
      Out.output_string f " }"
  | Value_function n ->
      let name, _ = (ctxt.funcs).(n) in
      Out.fprintf f "<function %s>" name
  | Value_builtin Expr.Builtin_add -> Out.output_string f "<builtin add>"
  | Value_builtin Expr.Builtin_sub -> Out.output_string f "<builtin sub>"
  | Value_builtin Expr.Builtin_mul -> Out.output_string f "<builtin mul>"
  | Value_builtin Expr.Builtin_less_eq ->
      Out.output_string f "<builtin less_eq>"


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
    | Expr.Parameter i -> List.nth_exn args i
    | Expr.Call ((e, _), args') -> (
      match eval args ctxt e with
      | Value_function func ->
          let expr = function_expression_by_index ctxt func in
          let args' = List.map args' ~f:(fun (e, _) -> eval args ctxt e) in
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
      | v ->
          Out.output_string Out.stderr "\n\nattempted to call a non-function: " ;
          output_value Out.stderr ctxt v ;
          assert false )
    | Expr.Builtin b -> Value_builtin b
    | Expr.Global_function i -> Value_function i
    | Expr.Struct_literal (_, members) ->
        let len = List.length members in
        let arr = Array.create ~len Value_unit in
        let f (i, (e, _)) = arr.(i) <- eval args ctxt e in
        List.iter ~f members ; Value_struct arr
    | Expr.Struct_access ((e, _), idx) ->
        let v = eval args ctxt e in
        match v with
        | Value_struct members -> members.(idx)
        | _ -> assert false
  in
  let ctxt = build_context ast in
  match function_index_by_name ctxt "main" with
  | None -> Out.output_string Out.stdout "main not defined\n"
  | Some idx ->
      let main_expr = function_expression_by_index ctxt idx in
      Out.output_string Out.stdout "main returned " ;
      output_value Out.stdout ctxt (eval [] ctxt main_expr) ;
      Out.newline Out.stdout
