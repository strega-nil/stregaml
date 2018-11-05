module Ast = Cafec_typed_ast
module Expr = Ast.Expr
module Stmt = Ast.Stmt

(*
  NOTE(ubsan): NEVER modify these arrays.
  They should be immutable,
  but ocaml doesn't have immutable arrays
*)
type t = {funcs: (string * Expr.block) array}

module Immediate = struct
  include Types.Immediate

  type function_index = Types.Function_index.t

  let function_index_of_int = Types.Function_index.of_int

  let rec clone imm =
    match imm with
    | Unit | Bool _ | Integer _ | Function _ | Builtin _ -> imm
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

module Value = struct
  include Types.Value

  let to_immediate = function
    | Immediate imm -> imm
    | Object {immediate; _} -> Immediate.clone !immediate

  let obj ~is_mut immediate =
    let immediate = ref immediate in
    let header = {is_mut; in_scope = true} in
    {header; immediate}

  let assign obj imm =
    assert obj.header.is_mut ;
    assert obj.header.in_scope ;
    obj.immediate := imm

  let drop obj =
    assert obj.header.in_scope ;
    obj.header.in_scope <- true
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
  | Some (n, _) -> Some (Immediate.function_index_of_int n)

let is_mut = function
  | Ast.Value_type.Immutable -> false
  | Ast.Value_type.Mutable -> true

let call ctxt (idx: Immediate.function_index) (args: Immediate.t list) =
  let rec eval_block ctxt locals Expr.({stmts; expr}) =
    let rec helper locals = function
      | [] -> locals
      | (stmt, _) :: xs -> (
        match stmt with
        | Stmt.Expression e ->
            let _ = eval ctxt locals e in
            helper locals xs
        | Stmt.Let {expr= expr, _; binding= Ast.Binding.{mutability; _}; _} ->
            let v = Value.to_immediate (eval ctxt locals expr) in
            let locals = (Value.obj ~is_mut:(is_mut mutability) v) :: locals in
            helper locals xs )
    in
    let locals = helper locals stmts in
    match expr with
    | Some (e, _) -> Value.to_immediate (eval ctxt locals e)
    | None -> Immediate.Unit
  and eval ctxt locals e =
    let Expr.({variant; _}) = e in
    match variant with
    | Expr.Unit_literal -> Value.Immediate Immediate.Unit
    | Expr.Bool_literal b -> Value.Immediate (Immediate.Bool b)
    | Expr.Integer_literal n -> Value.Immediate (Immediate.Integer n)
    | Expr.If_else {cond= cond, _; thn= thn, _; els= els, _} -> (
      match Value.to_immediate (eval ctxt locals cond) with
      | Immediate.Bool true -> Value.Immediate (eval_block ctxt locals thn)
      | Immediate.Bool false -> Value.Immediate (eval_block ctxt locals els)
      | _ -> assert false )
    | Expr.Local Expr.Local.({index; _}) ->
        Value.Object (List.nth_exn locals index)
    | Expr.Call ((e, _), args) -> (
      match Value.to_immediate (eval ctxt locals e) with
      | Immediate.Function func ->
          let _, expr = ctxt.funcs.((func :> int)) in
          let ready_arg (arg, _) =
            let imm = Value.to_immediate (eval ctxt locals arg) in
            (* we should actually be figuring out whether these should be mut *)
            Value.obj ~is_mut:false imm
          in
          let args = List.map args ~f:ready_arg in
          let ret = (eval_block ctxt args expr) in
          List.iter ~f:Value.drop args ;
          Value.Immediate ret
      | Immediate.Builtin b ->
          let (lhs, _), (rhs, _) =
            match args with [lhs; rhs] -> (lhs, rhs) | _ -> assert false
          in
          let lhs =
            match Value.to_immediate (eval ctxt locals lhs) with
            | Immediate.Integer v -> v
            | _ -> assert false
          in
          let rhs =
            match Value.to_immediate (eval ctxt locals rhs) with
            | Immediate.Integer v -> v
            | _ -> assert false
          in
          let ret =
            match b with
            | Expr.Builtin.Add -> Immediate.Integer (lhs + rhs)
            | Expr.Builtin.Sub -> Immediate.Integer (lhs - rhs)
            | Expr.Builtin.Mul -> Immediate.Integer (lhs * rhs)
            | Expr.Builtin.Less_eq -> Immediate.Bool (lhs <= rhs)
          in
          Value.Immediate ret
      | _ -> assert false )
    | Expr.Builtin b -> Value.Immediate (Immediate.Builtin b)
    | Expr.Block (b, _) -> Value.Immediate (eval_block ctxt locals b)
    | Expr.Global_function i -> Value.Immediate (Immediate.Function (Immediate.function_index_of_int i))
    | Expr.Record_literal {members; _} ->
        let members =
          let f ((name, (e, _)), _) =
            let immediate = ref (Value.to_immediate (eval ctxt locals e)) in
            (name, immediate)
          in
          List.map members ~f
        in
        Value.Immediate (Immediate.Record members)
    | Expr.Record_access ((e, _), member) -> (
        let find_member (name, _) = String.equal name member in
        let v = eval ctxt locals e in
        match v with
        | Value.Immediate (Immediate.Record members) ->
            let _, e = List.find_exn ~f:find_member members in
            Value.Immediate !e
        | Value.Object Value.{immediate; header} -> (
            match !immediate with
            | Immediate.Record members ->
              let _, immediate = List.find_exn ~f:find_member members in
              Value.Object Value.{immediate; header}
            | _ -> assert false )
        | _ -> assert false )
    | Expr.Assign {dest= dest, _; source= source, _} ->
        let dest = eval ctxt locals dest in
        let source = Value.to_immediate (eval ctxt locals source) in
        match dest with
        | Value.Immediate _ -> assert false
        | Value.Object obj ->
            Value.assign obj source ;
            Value.Immediate Immediate.Unit
  in
  let _, blk = ctxt.funcs.((idx :> int)) in
  let args = List.map ~f:(Value.obj ~is_mut:false) args in
  eval_block ctxt args blk
