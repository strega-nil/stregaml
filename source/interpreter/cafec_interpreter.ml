module Ast = Cafec_typed_ast
module Expr = Ast.Expr

(*
  NOTE(ubsan): NEVER modify these arrays.
  They should be immutable,
  but ocaml doesn't have immutable arrays
*)
type t = {funcs: (string * Expr.t) array}

module Value = struct
  type ctxt = t

  type function_index = int

  type t =
    | Unit
    | Bool of bool
    | Integer of int
    | Function of int
    | Struct of t array
    | Builtin of Expr.Builtin.t

  let rec equal lhs rhs =
    match (lhs, rhs) with
    | Unit, Unit -> true
    | Bool b1, Bool b2 -> Bool.equal b1 b2
    | Integer i1, Integer i2 -> i1 = i2
    | Function f1, Function f2 -> f1 = f2
    | Struct lhs, Struct rhs -> Array.equal ~equal lhs rhs
    | Builtin b1, Builtin b2 -> Expr.Builtin.equal b1 b2
    | _ -> false


  let rec output f v ctxt =
    let module Out = Stdio.Out_channel in
    match v with
    | Unit -> Out.output_string f "()"
    | Integer n -> Out.fprintf f "%d" n
    | Bool true -> Out.output_string f "true"
    | Bool false -> Out.output_string f "false"
    | Struct arr ->
        let rec output_rest seq =
          match Sequence.next seq with
          | Some (x, xs) ->
              Out.output_string f "; " ; output f x ctxt ; output_rest xs
          | None -> ()
        in
        Out.output_string f "{" ;
        let () =
          match Sequence.next (Array.to_sequence_mutable arr) with
          | Some (x, xs) ->
              Out.output_char f ' ' ; output f x ctxt ; output_rest xs
          | None -> ()
        in
        Out.output_string f " }"
    | Function n ->
        let name, _ = (ctxt.funcs).(n) in
        Out.fprintf f "<function %s>" name
    | Builtin Expr.Builtin.Add -> Out.output_string f "<builtin add>"
    | Builtin Expr.Builtin.Sub -> Out.output_string f "<builtin sub>"
    | Builtin Expr.Builtin.Mul -> Out.output_string f "<builtin mul>"
    | Builtin Expr.Builtin.Less_eq -> Out.output_string f "<builtin less_eq>"
end

let make ast =
  let seq = ref (Ast.function_seq ast) in
  let helper _ =
    match Sequence.next !seq with
    | None -> assert false
    | Some ((({Ast.fname; _}, _), (expr, _)), rest) ->
        seq := rest ;
        (fname, expr)
  in
  {funcs= Array.init (Ast.number_of_functions ast) ~f:helper}


let get_function ctxt ~name =
  match
    Array.findi ctxt.funcs ~f:(fun _ (name', _) -> String.equal name name')
  with
  | None -> None
  | Some (n, _) -> Some n


let call ctxt idx args =
  let rec eval ctxt args = function
    | Expr.Unit_literal -> Value.Unit
    | Expr.Bool_literal b -> Value.Bool b
    | Expr.Integer_literal n -> Value.Integer n
    | Expr.If_else ((cond, _), (thn, _), (els, _)) -> (
      match eval ctxt args cond with
      | Value.Bool true -> eval ctxt args thn
      | Value.Bool false -> eval ctxt args els
      | _ -> assert false )
    | Expr.Parameter i -> List.nth_exn args i
    | Expr.Call ((e, _), args') -> (
      match eval ctxt args e with
      | Value.Function func ->
          let _, expr = (ctxt.funcs).(func) in
          let args' = List.map args' ~f:(fun (e, _) -> eval ctxt args e) in
          eval ctxt args' expr
      | Value.Builtin b ->
          let (lhs, _), (rhs, _) =
            match args' with [lhs; rhs] -> (lhs, rhs) | _ -> assert false
          in
          let lhs =
            match eval ctxt args lhs with
            | Value.Integer v -> v
            | _ -> assert false
          in
          let rhs =
            match eval ctxt args rhs with
            | Value.Integer v -> v
            | _ -> assert false
          in
          let ret =
            match b with
            | Expr.Builtin.Add -> Value.Integer (lhs + rhs)
            | Expr.Builtin.Sub -> Value.Integer (lhs - rhs)
            | Expr.Builtin.Mul -> Value.Integer (lhs * rhs)
            | Expr.Builtin.Less_eq -> Value.Bool (lhs <= rhs)
          in
          ret
      | _ -> assert false )
    | Expr.Builtin b -> Value.Builtin b
    | Expr.Global_function i -> Value.Function i
    | Expr.Struct_literal (_, members) ->
        let len = List.length members in
        let arr = Array.create ~len Value.Unit in
        let f (i, (e, _)) = arr.(i) <- eval ctxt args e in
        List.iter ~f members ; Value.Struct arr
    | Expr.Struct_access ((e, _), idx) ->
        let v = eval ctxt args e in
        match v with
        | Value.Struct members -> members.(idx)
        | _ -> assert false
  in
  let _, expr = (ctxt.funcs).(idx) in
  eval ctxt args expr
