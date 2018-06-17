module Ast = Cafec_typed_ast
module Expr = Ast.Expr

(*
  NOTE(ubsan): NEVER modify these arrays.
  They should be immutable,
  but ocaml doesn't have immutable arrays
*)
type t = {funcs: (string * Expr.t) array}

module Value = struct
  type function_index = int

  type t =
    | Unit
    | Bool of bool
    | Integer of int
    | Function of int
    | Record of (string * t) list
    | Builtin of Expr.Builtin.t

  let rec equal lhs rhs =
    match (lhs, rhs) with
    | Unit, Unit -> true
    | Bool b1, Bool b2 -> Bool.equal b1 b2
    | Integer i1, Integer i2 -> i1 = i2
    | Function f1, Function f2 -> f1 = f2
    | Record lhs, Record rhs ->
        let equal (name1, e1) (name2, e2) =
          String.equal name1 name2 && equal e1 e2
        in
        List.equal ~equal lhs rhs
    | Builtin b1, Builtin b2 -> Expr.Builtin.equal b1 b2
    | _ -> false


  let rec to_string v ctxt =
    match v with
    | Unit -> "()"
    | Integer n -> Int.to_string n
    | Bool true -> "true"
    | Bool false -> "false"
    | Record members ->
        let members =
          let f (name, e) = String.concat [name; " = "; to_string e ctxt] in
          String.concat ~sep:"; " (List.map ~f members)
        in
        String.concat ["{ "; members; " }"]
    | Function n ->
        let name, _ = (ctxt.funcs).(n) in
        Printf.sprintf "<function %s>" name
    | Builtin Expr.Builtin.Add -> "<builtin add>"
    | Builtin Expr.Builtin.Sub -> "<builtin sub>"
    | Builtin Expr.Builtin.Mul -> "<builtin mul>"
    | Builtin Expr.Builtin.Less_eq -> "<builtin less_eq>"
end

let make ast =
  let seq = ref (Ast.function_seq ast) in
  let helper _ =
    match Sequence.next !seq with
    | None -> assert false
    | Some ((({Ast.Function_declaration.name; _}, _), (expr, _)), rest) ->
        seq := rest ;
        (name, expr)
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
    | Expr.Record_literal {members; _} ->
        let members =
          List.map members ~f:(fun ((name, (e, _)), _) ->
              (name, eval ctxt args e) )
        in
        Value.Record members
    | Expr.Record_access ((e, _), member) ->
        let v = eval ctxt args e in
        match v with
        | Value.Record members ->
            let f (name, _) = String.equal name member in
            let _, e = List.find_exn ~f members in
            e
        | _ -> assert false
  in
  let _, expr = (ctxt.funcs).(idx) in
  eval ctxt args expr
