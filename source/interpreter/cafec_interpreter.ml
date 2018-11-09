module Ast = Cafec_typed_ast
module Expr = Ast.Expr
module Stmt = Ast.Stmt
module Type = Ast.Type

(*
  NOTE(ubsan): NEVER modify these arrays.
  They should be immutable,
  but ocaml doesn't have immutable arrays
*)
type t = {funcs: (string * Expr.block) array}

module Value = struct
  include Types.Value

  type function_index = Types.Function_index.t

  let function_index_of_int = Types.Function_index.of_int

  let rec clone imm =
    match imm with
    | Unit | Bool _ | Integer _ | Function _ | Builtin _ | Reference _ -> imm
    | Record r ->
        let rec helper = function
          | (name, x) :: xs -> (name, ref (clone !x)) :: helper xs
          | [] -> []
        in
        Record (helper r)

  let rec to_string v ctxt =
    match v with
    | Unit -> "()"
    | Integer n -> Int.to_string n
    | Bool true -> "true"
    | Bool false -> "false"
    | Reference place ->
        let Types.Expr_result.({is_mut; value; _}) = place in
        let pointer = if is_mut then "&mut " else "&" in
        String.concat [pointer; "{"; to_string !value ctxt; "}"]
    | Record members ->
        let members =
          let f (name, e) = String.concat [name; " = "; to_string !e ctxt] in
          String.concat ~sep:"; " (List.map ~f members)
        in
        String.concat ["{ "; members; " }"]
    | Function n ->
        let name, _ = ctxt.funcs.((n :> int)) in
        Printf.sprintf "<function %s>" name
    | Builtin Expr.Builtin.Add -> "<builtin add>"
    | Builtin Expr.Builtin.Sub -> "<builtin sub>"
    | Builtin Expr.Builtin.Mul -> "<builtin mul>"
    | Builtin Expr.Builtin.Less_eq -> "<builtin less_eq>"
end

module Expr_result = struct
  include Types.Expr_result

  let to_value = function
    | Value value -> value
    | Place {value; _} -> Value.clone !value

  let reference ~is_mut = function
    | Value _ -> assert false
    | Place place ->
        let is_mut = place.is_mut && is_mut in
        let place = {place with is_mut} in
        Value (Value.Reference place)

  let deref r =
    let r = to_value r in
    match r with Value.Reference p -> Place p | _ -> assert false

  let assign place imm =
    assert place.is_mut ;
    assert place.header.in_scope ;
    place.value := imm
end

module Object = struct
  type t = {header: Expr_result.object_header; is_mut: bool; value: Value.t ref}

  let place {header; is_mut; value} =
    {Expr_result.header; Expr_result.is_mut; Expr_result.value}

  let obj ~is_mut value =
    let value = ref value in
    let header = Expr_result.{in_scope= true} in
    {header; is_mut; value}

  let drop obj =
    assert obj.header.Expr_result.in_scope ;
    (obj.header).Expr_result.in_scope <- true
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
  | Some (n, _) -> Some (Value.function_index_of_int n)

let is_mut = function Type.Immutable -> false | Type.Mutable -> true

let call ctxt (idx : Value.function_index) (args : Value.t list) =
  let rec eval_block ctxt locals Expr.({stmts; expr}) =
    let rec helper locals = function
      | [] -> locals
      | (stmt, _) :: xs -> (
        match stmt with
        | Stmt.Expression e ->
            let _ = eval ctxt locals e in
            helper locals xs
        | Stmt.Let {expr= expr, _; binding= Ast.Binding.({mutability; _}); _}
          ->
            let v = Expr_result.to_value (eval ctxt locals expr) in
            let locals = Object.obj ~is_mut:(is_mut mutability) v :: locals in
            helper locals xs )
    in
    let locals = helper locals stmts in
    match expr with
    | Some (e, _) -> Expr_result.to_value (eval ctxt locals e)
    | None -> Value.Unit
  and eval ctxt locals e =
    let Expr.({variant; _}) = e in
    match variant with
    | Expr.Unit_literal -> Expr_result.Value Value.Unit
    | Expr.Bool_literal b -> Expr_result.Value (Value.Bool b)
    | Expr.Integer_literal n -> Expr_result.Value (Value.Integer n)
    | Expr.If_else {cond= cond, _; thn= thn, _; els= els, _} -> (
      match Expr_result.to_value (eval ctxt locals cond) with
      | Value.Bool true -> Expr_result.Value (eval_block ctxt locals thn)
      | Value.Bool false -> Expr_result.Value (eval_block ctxt locals els)
      | _ -> assert false )
    | Expr.Local Expr.Local.({index; _}) ->
        Expr_result.Place (Object.place (List.nth_exn locals index))
    | Expr.Call ((e, _), args) -> (
      match Expr_result.to_value (eval ctxt locals e) with
      | Value.Function func ->
          let _, expr = ctxt.funcs.((func :> int)) in
          let ready_arg (arg, _) =
            let imm = Expr_result.to_value (eval ctxt locals arg) in
            (* we should actually be figuring out whether these should be mut *)
            Object.obj ~is_mut:false imm
          in
          let args = List.map args ~f:ready_arg in
          let ret = eval_block ctxt args expr in
          List.iter ~f:Object.drop args ;
          Expr_result.Value ret
      | Value.Builtin b ->
          let (lhs, _), (rhs, _) =
            match args with [lhs; rhs] -> (lhs, rhs) | _ -> assert false
          in
          let lhs =
            match Expr_result.to_value (eval ctxt locals lhs) with
            | Value.Integer v -> v
            | _ -> assert false
          in
          let rhs =
            match Expr_result.to_value (eval ctxt locals rhs) with
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
          Expr_result.Value ret
      | _ -> assert false )
    | Expr.Builtin b -> Expr_result.Value (Value.Builtin b)
    | Expr.Block (b, _) -> Expr_result.Value (eval_block ctxt locals b)
    | Expr.Global_function i ->
        Expr_result.Value (Value.Function (Value.function_index_of_int i))
    | Expr.Reference {mutability; place= place, _} ->
        let place = eval ctxt locals place in
        Expr_result.reference ~is_mut:(is_mut mutability) place
    | Expr.Dereference (value, _) ->
        let value = eval ctxt locals value in
        Expr_result.deref value
    | Expr.Record_literal {members; _} ->
        let members =
          let f ((name, (e, _)), _) =
            let immediate = ref (Expr_result.to_value (eval ctxt locals e)) in
            (name, immediate)
          in
          List.map members ~f
        in
        Expr_result.Value (Value.Record members)
    | Expr.Record_access ((e, _), member) -> (
        let find_member (name, _) = String.equal name member in
        let v = eval ctxt locals e in
        match v with
        | Expr_result.Value (Value.Record members) ->
            let _, e = List.find_exn ~f:find_member members in
            Expr_result.Value !e
        | Expr_result.Place Expr_result.({value; is_mut; header}) -> (
          match !value with
          | Value.Record members ->
              let _, value = List.find_exn ~f:find_member members in
              Expr_result.Place Expr_result.{value; is_mut; header}
          | _ -> assert false )
        | _ -> assert false )
    | Expr.Assign {dest= dest, _; source= source, _} -> (
        let dest = eval ctxt locals dest in
        let source = Expr_result.to_value (eval ctxt locals source) in
        match dest with
        | Expr_result.Value _ -> assert false
        | Expr_result.Place place ->
            Expr_result.assign place source ;
            Expr_result.Value Value.Unit )
  in
  let _, blk = ctxt.funcs.((idx :> int)) in
  let args = List.map ~f:(Object.obj ~is_mut:false) args in
  eval_block ctxt args blk
