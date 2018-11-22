module Spanned = Cafec_containers.Spanned

let indent_to_string indent = String.make (indent * 2) ' '

module Type = struct
  include Types.Ast_Type

  let rec to_string = function
    | Named id -> (id :> string)
    | Reference {is_mut; pointee= ty, _} ->
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
    include Types.Ast_Type_Data

    let to_string (Record members) =
      let f (((name : Nfc_string.t), ty), _) =
        String.concat ["\n    "; (name :> string); ": "; to_string ty; ";"]
      in
      let members = String.concat (List.map members ~f) in
      String.concat ["record {"; members; "\n  }"]
  end

  module Definition = Types.Ast_Type_Definition
end

module Implementation_stmt_expr = struct
  let infix_to_string = function
    | Types.Ast_Expr.Infix_assign -> "<-"
    | Types.Ast_Expr.Infix_name id -> (id :> string)

  let rec expr_to_string ?(parens = false) e ~indent =
    let open Types.Ast_Expr in
    let open_paren parens = if parens then "(" else "" in
    let close_paren parens = if parens then ")" else "" in
    let arg_list args =
      let f (x, _) = expr_to_string x ~indent:(indent + 1) in
      String.concat ~sep:", " (List.map args ~f)
    in
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
    | Name {path; name} ->
        let name = (path :> string list) @ [Name.to_string name] in
        String.concat ~sep:"::" name
    | Block (blk, _) -> block_to_string blk ~indent
    | Builtin ((name, _), args) ->
        let args = arg_list args in
        String.concat ["__builtin("; (name :> string); ")("; args; ")"]
    | Prefix_operator ((name, _), (expr, _)) ->
        String.concat
          [ open_paren parens
          ; (name :> string)
          ; expr_to_string ~indent expr
          ; close_paren parens ]
    | Infix_list ((first, _), rest) ->
        let f ((op, _), (expr, _)) =
          let expr = expr_to_string expr ~parens:true ~indent:(indent + 1) in
          String.concat [" "; infix_to_string op; " "; expr]
        in
        let first = expr_to_string first ~parens:true ~indent:(indent + 1) in
        String.concat (first :: List.map ~f rest)
    | Call ((e, _), args) ->
        let args = arg_list args in
        String.concat [expr_to_string ~indent e; "("; args; ")"]
    | Reference {is_mut; place= place, _} ->
        let name = if is_mut then "&mut " else "&" in
        let place = expr_to_string place ~parens:true ~indent:(indent + 1) in
        String.concat [name; place]
    | Dereference (value, _) ->
        let place = expr_to_string value ~parens:true ~indent:(indent + 1) in
        String.concat [open_paren parens; "*"; place; close_paren parens]
    | Record_literal {ty= ty, _; members} ->
        let members =
          let f (((name : Nfc_string.t), (expr, _)), _) =
            String.concat
              [ (name :> string)
              ; " = "
              ; expr_to_string expr ~indent:(indent + 1) ]
          in
          String.concat ~sep:"; " (List.map ~f members)
        in
        String.concat [Type.to_string ty; "::{ "; members; " }"]
    | Record_access ((e, _), member) ->
        let record = expr_to_string e ~parens:true ~indent:(indent + 1) in
        let member = (member :> string) in
        String.concat
          [open_paren parens; record; "."; member; close_paren parens]

  and block_to_string Types.Ast_Expr.({stmts; expr}) ~indent =
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
    String.concat ["{\n"; stmts; expr; "\n"; indent_to_string indent; "}"]

  and stmt_to_string self ~indent =
    let open Types.Ast_Stmt in
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
          [ "let "
          ; mut
          ; Name.to_string name
          ; ty
          ; " = "
          ; expr_to_string expr ~indent ]
end

module Stmt = struct
  include Types.Ast_Stmt

  let to_string = Implementation_stmt_expr.stmt_to_string
end

module Expr = struct
  include Types.Ast_Expr

  let to_string = Implementation_stmt_expr.expr_to_string ~parens:false

  let block_to_string = Implementation_stmt_expr.block_to_string
end

module Func = struct
  include Types.Ast_Func

  let to_string self =
    let parameters =
      let f ((((name : Name.t), _), (ty, _)), _) =
        String.concat [Name.to_string name; ": "; Type.to_string ty]
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
      ; Name.to_string self.name
      ; "("
      ; parameters
      ; ")"
      ; ret_ty
      ; " "
      ; (let body, _ = self.body in
         Expr.block_to_string ~indent:0 body)
      ; "\n" ]
end

module Infix_group = struct
  include Types.Ast_Infix_group

  let to_string {name= name, _; associativity; precedence} =
    let associativity =
      match associativity with Assoc_start -> "start" | Assoc_none -> "none"
    in
    let precedence =
      let f (Less (id, _)) = "precedence < " ^ (id :> string) in
      String.concat ~sep:"\n  " (List.map precedence ~f)
    in
    Printf.sprintf {|infix group %s {
  associativity = %s;
  %s
}|}
      (name :> string)
      associativity precedence
end

module Infix_declaration = struct
  include Types.Ast_Infix_declaration

  let to_string {name= name, _; group= group, _} =
    String.concat ["infix ("; (name :> string); "): "; (group :> string); ";"]
end

include Types.Ast

let to_string self =
  let types =
    let f (Type.Definition.({name= name, _; kind}), _) =
      match kind with
      | Type.Definition.Alias data ->
          String.concat
            ["type "; (name :> string); " = "; Type.to_string data; ";"]
      | Type.Definition.User_defined {data} ->
          String.concat
            [ "type "
            ; (name :> string)
            ; " {\n"
            ; "  data = "
            ; Type.Data.to_string data
            ; ";\n}" ]
    in
    String.concat ~sep:"\n" (List.map ~f self.types)
  in
  let infix_groups =
    let f (infix_group, _) = Infix_group.to_string infix_group in
    String.concat ~sep:"\n" (List.map ~f self.infix_groups)
  in
  let infix_decls =
    let f (infix_decl, _) = Infix_declaration.to_string infix_decl in
    String.concat ~sep:"\n\n" (List.map ~f self.infix_decls)
  in
  let funcs =
    let f (func, _) = Func.to_string func in
    String.concat ~sep:"\n" (List.map ~f self.funcs)
  in
  String.concat ~sep:"\n\n" [types; infix_groups; infix_decls; funcs]
