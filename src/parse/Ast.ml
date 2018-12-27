let indent_to_string indent = String.make (indent * 2) ' '

let infix_name_string = function
  | Name.Name {string; kind= Name.Operator; _} -> (string :> string)
  | Name.Name {string; kind= Name.Identifier; _} -> "\\" ^ (string :> string)

let infix_to_string = function
  | Types.Ast_Expr.Infix_assign -> "<-"
  | Types.Ast_Expr.Infix_name name -> infix_name_string name

module Implementation_stmt_expr = struct
  let qualified_to_string (Types.Ast_Expr.Qualified {path; name= name, _}) =
    let f ((id : Nfc_string.t), _) = (id :> string) in
    let path = List.map ~f path in
    let name = path @ [Name.to_ident_string name] in
    String.concat ~sep:"::" name

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
    | Match {cond= cond, _; arms} ->
        let f ((pat, _), (block, _)) =
          let (Pattern {constructor= const, _; binding= binding, _}) = pat in
          String.concat
            [ indent_to_string (indent + 1)
            ; qualified_to_string const
            ; "("
            ; Name.to_ident_string binding
            ; ") => "
            ; block_to_string ~indent:(indent + 1) block
            ; "\n" ]
        in
        let arms = String.concat (List.map ~f arms) in
        String.concat
          [ "match ("
          ; expr_to_string cond ~indent:(indent + 1)
          ; ") {\n"
          ; arms
          ; indent_to_string indent
          ; "}" ]
    | If_else {cond= cond, _; thn= thn, _; els= els, _} ->
        String.concat
          [ "if ("
          ; expr_to_string cond ~indent:(indent + 1)
          ; ") "
          ; block_to_string thn ~indent
          ; " else "
          ; block_to_string els ~indent ]
    | Name name -> qualified_to_string name
    | Block (blk, _) -> block_to_string blk ~indent
    | Builtin ((name, _), args) ->
        let args = arg_list args in
        String.concat ["__builtin("; (name :> string); ")("; args; ")"]
    | Prefix_operator ((name, _), (expr, _)) ->
        let name =
          match name with
          | Name.Name {string; kind= Name.Operator; _} -> (string :> string)
          | Name.Name {string; kind= Name.Identifier; _} ->
              "\\" ^ (string :> string) ^ " "
        in
        String.concat
          [ open_paren parens
          ; name
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
    | Place {mutability= mut, _; expr= expr, _} ->
        String.concat
          [ Type.mutability_to_string mut
          ; " "
          ; expr_to_string expr ~parens:true ~indent:(indent + 1) ]
    | Reference (place, _) ->
        let place = expr_to_string place ~parens:true ~indent:(indent + 1) in
        String.concat [open_paren parens; "&"; place; close_paren parens]
    | Dereference (expr, _) ->
        let expr = expr_to_string expr ~parens:true ~indent:(indent + 1) in
        String.concat [open_paren parens; "*"; expr; close_paren parens]
    | Record_literal {ty= ty, _; members} ->
        let members =
          let f ((name, (expr, _)), _) =
            String.concat
              [ Name.to_ident_string name
              ; " = "
              ; expr_to_string expr ~indent:(indent + 1) ]
          in
          String.concat ~sep:"; " (List.map ~f members)
        in
        String.concat [Type.to_string ty; "::{ "; members; " }"]
    | Record_access ((e, _), name) ->
        let record = expr_to_string e ~parens:true ~indent:(indent + 1) in
        let name = Name.to_ident_string name in
        String.concat [open_paren parens; record; "."; name; close_paren parens]

  and block_to_string (Types.Ast_Expr_Block.Block {stmts; expr}) ~indent =
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
    | Let {name= name, _; is_mut; ty; expr} ->
        let ty =
          match ty with Some (ty, _) -> ": " ^ Type.to_string ty | None -> ""
        in
        let mut = if is_mut then "mut " else "" in
        let expr, _ = expr in
        String.concat
          [ "let "
          ; mut
          ; Name.to_ident_string name
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

  module Block = struct
    include Types.Ast_Expr_Block

    let to_string = Implementation_stmt_expr.block_to_string

    let stmts (Block r) = r.stmts

    let expr (Block r) = r.expr

    let with_stmt (Block r) stmt = Block {r with stmts= stmt :: r.stmts}
  end

  let to_string = Implementation_stmt_expr.expr_to_string ~parens:false

  let qualified_path (Qualified r) = r.path

  let qualified_name (Qualified r) = r.name

  let pattern_constructor (Pattern r) = r.constructor

  let pattern_binding (Pattern r) = r.binding
end

module Func = struct
  include Types.Ast_Func

  let name (Func r) = Name.erase r.name

  let params (Func r) = r.params

  let ret_ty (Func r) = r.ret_ty

  let body (Func r) = r.body

  let to_string self =
    let parameters =
      let f (((name, _), (ty, _)), _) =
        String.concat [Name.to_ident_string name; ": "; Type.to_string ty]
      in
      String.concat ~sep:", " (List.map ~f (params self))
    in
    let ret_ty =
      match ret_ty self with
      | Some (ty, _) -> " -> " ^ Type.to_string ty
      | None -> ""
    in
    String.concat
      [ "func "
      ; Name.to_ident_string (name self)
      ; "("
      ; parameters
      ; ")"
      ; ret_ty
      ; " "
      ; (let body, _ = body self in
         Expr.Block.to_string ~indent:0 body)
      ; "\n" ]
end

module Infix_group = struct
  include Types.Ast_Infix_group

  let to_string (Infix_group {name= name, _; associativity; precedence}) =
    let associativity =
      match associativity with
      | Assoc_start -> "start"
      | Assoc_end -> "end"
      | Assoc_none -> "none"
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

  let to_string (Infix_declaration {name= name, _; group= group, _}) =
    String.concat
      ["infix ("; infix_name_string name; "): "; (group :> string); ";"]
end

include Types.Ast

let funcs (Ast r) = r.funcs

let infix_decls (Ast r) = r.infix_decls

let infix_groups (Ast r) = r.infix_groups

let types (Ast r) = r.types

let with_func (Ast r) func = Ast {r with funcs= func :: r.funcs}

let with_infix_decl (Ast r) infix_decl =
  Ast {r with infix_decls= infix_decl :: r.infix_decls}

let with_infix_group (Ast r) infix_group =
  Ast {r with infix_groups= infix_group :: r.infix_groups}

let with_type (Ast r) type_ = Ast {r with types= type_ :: r.types}

let to_string self =
  let types =
    let f (Type.Definition.Definition {name= name, _; kind}, _) =
      match kind with
      | Type.Definition.Alias data ->
          String.concat
            ["type "; (name :> string); " = "; Type.to_string data; ";"]
      | Type.Definition.User_defined {data} ->
          String.concat
            [ "type "
            ; (name :> string)
            ; " {\n"
            ; "  "
            ; Type.Data.to_string ~name:"data" data
            ; ";\n}" ]
    in
    if List.is_empty (types self) then ""
    else String.concat ~sep:"\n" (List.map ~f (types self)) ^ "\n\n"
  in
  let infix_groups =
    let f (infix_group, _) = Infix_group.to_string infix_group in
    if List.is_empty (infix_groups self) then ""
    else String.concat ~sep:"\n" (List.map ~f (infix_groups self)) ^ "\n\n"
  in
  let infix_decls =
    let f (infix_decl, _) = Infix_declaration.to_string infix_decl in
    if List.is_empty (infix_decls self) then ""
    else String.concat ~sep:"\n" (List.map ~f (infix_decls self)) ^ "\n\n"
  in
  let funcs =
    let f (func, _) = Func.to_string func in
    String.concat ~sep:"\n" (List.map ~f (funcs self))
  in
  String.concat [types; infix_groups; infix_decls; funcs]
