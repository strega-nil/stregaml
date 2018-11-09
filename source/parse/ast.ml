module Spanned = Cafec_containers.Spanned

let indent_to_string indent = String.make (indent * 2) ' '

module Type = struct
  include Types.Ast_type

  let rec to_string = function
    | Named s -> s
    | Pointer {is_mut; pointee= ty, _} ->
        let ptr = if is_mut then "&mut " else "&" in
        ptr ^ to_string ty
    | Function {params; ret_ty} ->
        let f (x, _) = to_string x in
        let ret_ty =
          match ret_ty with Some x -> ") -> " ^ f x | None -> ")"
        in
        let params = String.concat ~sep:", " (List.map params ~f) in
        String.concat ["func("; params; ret_ty]

  module Data = struct
    include Types.Ast_type_data

    let to_string (Record members) =
      let f ((name, ty), _) =
        String.concat ["\n    "; name; ": "; to_string ty; ";"]
      in
      let members = String.concat (List.map members ~f) in
      String.concat ["record {"; members; "\n  }"]
  end

  module Definition = Types.Ast_type_definition
end

module Implementation_stmt_expr = struct
  let rec expr_to_string e ~indent =
    let open Types.Ast_expr in
    match e with
    | Unit_literal -> "()"
    | Bool_literal true -> "true"
    | Bool_literal false -> "false"
    | Integer_literal n -> Int.to_string n
    | If_else {cond= cond, _; thn= thn, _; els= els, _} ->
        String.concat
          [ "if ("
          ; expr_to_string cond ~indent:(indent + 1)
          ; ") "
          ; block_to_string thn ~indent
          ; " else "
          ; block_to_string els ~indent ]
    | Variable {path; name} -> String.concat ~sep:"::" (path @ [name])
    | Block (blk, _) -> block_to_string blk ~indent
    | Call ((e, _), args) ->
        let args =
          let f (x, _) = expr_to_string x ~indent:(indent + 1) in
          String.concat ~sep:", " (List.map args ~f)
        in
        String.concat [expr_to_string ~indent e; "("; args; ")"]
    | Assign {source= source, _; dest= dest, _} ->
        String.concat
          [ "ASSIGN("
          ; expr_to_string dest ~indent:(indent + 1)
          ; ", "
          ; expr_to_string source ~indent:(indent + 1)
          ; ")" ]
    | Record_literal {ty= ty, _; members} ->
        let members =
          let f ((name, (expr, _)), _) =
            String.concat
              [name; " = "; expr_to_string expr ~indent:(indent + 1)]
          in
          String.concat ~sep:"; " (List.map ~f members)
        in
        String.concat [Type.to_string ty; "::{ "; members; " }"]
    | Record_access ((e, _), member) ->
        String.concat [expr_to_string ~indent:(indent + 1) e; "."; member]

  and block_to_string Types.Ast_expr.({stmts; expr}) ~indent =
    let stmts =
      let f (s, _) =
        String.concat
          [ indent_to_string (indent + 1)
          ; stmt_to_string s ~indent:(indent + 1)
          ; ";\n" ]
      in
      String.concat (List.map stmts ~f)
    in
    let expr =
      match expr with
      | None -> ""
      | Some (e, _) ->
          indent_to_string (indent + 1) ^ expr_to_string e ~indent:(indent + 1)
    in
    String.concat ["{\n"; stmts; expr; indent_to_string indent; "\n}"]

  and stmt_to_string self ~indent =
    let open Types.Ast_stmt in
    match self with
    | Expression (e, _) -> expr_to_string ~indent e
    | Let {name; is_mut; ty; expr} ->
        let name, _ = name in
        let ty =
          match ty with Some (ty, _) -> ": " ^ Type.to_string ty | None -> ""
        in
        let mut = if is_mut then "mut " else "" in
        let expr, _ = expr in
        String.concat
          ["let "; mut; name; ty; " = "; expr_to_string expr ~indent]
end

module Stmt = struct
  include Types.Ast_stmt

  let to_string = Implementation_stmt_expr.stmt_to_string
end

module Expr = struct
  include Types.Ast_expr

  let to_string = Implementation_stmt_expr.expr_to_string

  let block_to_string = Implementation_stmt_expr.block_to_string
end

module Func = struct
  include Types.Ast_func

  let to_string self =
    let parameters =
      let f (((name, _), (ty, _)), _) =
        String.concat [name; ": "; Type.to_string ty]
      in
      String.concat ~sep:", " (List.map ~f self.params)
    in
    let ret_ty =
      match self.ret_ty with
      | Some (ty, _) -> " -> " ^ Type.to_string ty
      | None -> ""
    in
    String.concat
      [ "func "
      ; self.name
      ; "("
      ; parameters
      ; ")"
      ; ret_ty
      ; " "
      ; (let body, _ = self.body in
         Expr.block_to_string ~indent:0 body)
      ; "\n" ]
end

include Types.Ast

let to_string self =
  let types =
    let f (Type.Definition.({name= name, _; kind}), _) =
      match kind with
      | Type.Definition.Alias data ->
          String.concat ["alias "; name; " = "; Type.to_string data; ";"]
      | Type.Definition.User_defined {data} ->
          String.concat
            [ "type "
            ; name
            ; " {\n"
            ; "  data = "
            ; Type.Data.to_string data
            ; ";\n}" ]
    in
    String.concat ~sep:"\n" (List.map ~f self.types)
  in
  let funcs =
    let f (func, _) = Func.to_string func in
    String.concat ~sep:"\n" (List.map ~f self.funcs)
  in
  String.concat [types; "\n\n"; funcs]
