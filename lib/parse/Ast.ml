module Att = Token.Attribute
module Kw = Token.Keyword
module Ckw = Token.Keyword.Contextual

let indent_to_string indent = String.make (indent * 2) ' '

let infix_name_string = function
  | Name.Name {string; kind = Name.Operator; _} -> (string :> string)
  | Name.Name {string; kind = Name.Identifier; _} ->
      "\\" ^ (string :> string)

let infix_to_string = function
  | Types.Ast_Expr.Infix_assign -> "<-"
  | Types.Ast_Expr.Infix_name name -> infix_name_string name

module Attribute = struct
  include Types.Ast_Attribute

  let to_string att ~lang =
    match att with Entrypoint ->
      Lang.attribute_to_string ~lang Att.Entrypoint

  let spanned_list_to_string attributes ~lang =
    if List.is_empty attributes
    then ""
    else
      let atts =
        String.concat ~sep:", "
          (List.map
             ~f:(function att, _ -> to_string ~lang att)
             attributes)
      in
      "@[" ^ atts ^ "]\n"
end

module Implementation_stmt_expr = struct
  let qualified_to_string
      (Types.Ast_Expr.Qualified {path; name = name, _}) =
    let f ((id : Nfc_string.t), _) = (id :> string) in
    let path = List.map ~f path in
    let name = path @ [Name.to_ident_string name] in
    String.concat ~sep:"::" name

  let rec expr_to_string ?(parens = false) e ~indent ~lang =
    let open Types.Ast_Expr in
    let open_paren parens = if parens then "(" else "" in
    let close_paren parens = if parens then ")" else "" in
    let arg_list args =
      let f (x, _) = expr_to_string x ~indent:(indent + 1) ~lang in
      String.concat ~sep:", " (List.map args ~f)
    in
    match e with
    | Integer_literal n -> Int.to_string n
    | Tuple_literal xs ->
        let args = arg_list xs in
        String.concat ["("; args; ")"]
    | Match {cond = cond, _; arms} ->
        let f ((pat, _), (block, _)) =
          let (Pattern {constructor; binding}) = pat in
          let const, _ = constructor in
          let binding =
            match binding with
            | Some (binding, _) ->
                String.concat ["("; Name.to_ident_string binding; ")"]
            | None -> ""
          in
          String.concat
            [ indent_to_string (indent + 1)
            ; qualified_to_string const
            ; binding
            ; " => "
            ; block_to_string ~indent:(indent + 1) block ~lang
            ; "\n" ]
        in
        let arms = String.concat (List.map ~f arms) in
        String.concat
          [ Lang.keyword_to_string ~lang Kw.Match
          ; " ("
          ; expr_to_string cond ~indent:(indent + 1) ~lang
          ; ") {\n"
          ; arms
          ; indent_to_string indent
          ; "}" ]
    | Name name -> qualified_to_string name
    | Block (blk, _) -> block_to_string blk ~indent ~lang
    | Builtin {name; type_arguments} ->
        let name, _ = name in
        let type_arguments =
          if List.is_empty type_arguments
          then ""
          else
            let f (ty, _) = Type.to_string ty ~lang in
            let inner =
              String.concat ~sep:", " (List.map ~f type_arguments)
            in
            String.concat ["["; inner; "]"]
        in
        String.concat
          [ Lang.keyword_to_string ~lang Kw.Builtin
          ; "("
          ; Lang.builtin_name_to_string name ~lang
          ; type_arguments
          ; ")" ]
    | Prefix_operator ((name, _), (expr, _)) ->
        let name =
          match name with
          | Name.Name {string; kind = Name.Operator; _} ->
              (string :> string)
          | Name.Name {string; kind = Name.Identifier; _} ->
              "\\" ^ (string :> string) ^ " "
        in
        String.concat
          [ open_paren parens
          ; name
          ; expr_to_string ~indent expr ~lang
          ; close_paren parens ]
    | Infix_list ((first, _), rest) ->
        let f ((op, _), (expr, _)) =
          let expr =
            expr_to_string expr ~parens:true ~indent:(indent + 1) ~lang
          in
          String.concat [" "; infix_to_string op; " "; expr]
        in
        let first =
          expr_to_string first ~parens:true ~indent:(indent + 1) ~lang
        in
        String.concat (first :: List.map ~f rest)
    | Call ((e, _), args) ->
        let args = arg_list args in
        String.concat [expr_to_string ~indent e ~lang; "("; args; ")"]
    | Place {mutability = mut, _; expr = expr, _} ->
        String.concat
          [ Type.mutability_to_string ~lang mut
          ; " "
          ; expr_to_string expr ~parens:true ~indent:(indent + 1) ~lang
          ]
    | Reference (place, _) ->
        let place =
          expr_to_string place ~parens:true ~indent:(indent + 1) ~lang
        in
        String.concat
          [open_paren parens; "&"; place; close_paren parens]
    | Dereference (expr, _) ->
        let expr =
          expr_to_string expr ~parens:true ~indent:(indent + 1) ~lang
        in
        String.concat [open_paren parens; "*"; expr; close_paren parens]
    | Record_literal {ty = ty, _; fields} ->
        let fields =
          let f ((name, (expr, _)), _) =
            String.concat
              [ Name.to_ident_string name
              ; " = "
              ; expr_to_string expr ~indent:(indent + 1) ~lang ]
          in
          String.concat ~sep:"; " (List.map ~f fields)
        in
        String.concat [Type.to_string ~lang ty; "::{ "; fields; " }"]
    | Record_access ((e, _), name) ->
        let record =
          expr_to_string e ~parens:true ~indent:(indent + 1) ~lang
        in
        let name = Name.to_ident_string name in
        String.concat
          [open_paren parens; record; "."; name; close_paren parens]

  and block_to_string (Types.Ast_Expr_Block.Block {stmts; expr})
      ~indent ~lang =
    let stmts =
      let f (s, _) =
        String.concat
          [ indent_to_string (indent + 1)
          ; stmt_to_string s ~indent:(indent + 1) ~lang
          ; ";\n" ]
      in
      String.concat (List.map stmts ~f)
    in
    let expr =
      match expr with
      | None -> ""
      | Some (e, _) ->
          indent_to_string (indent + 1)
          ^ expr_to_string e ~indent:(indent + 1) ~lang
    in
    String.concat
      ["{\n"; stmts; expr; "\n"; indent_to_string indent; "}"]

  and stmt_to_string self ~indent ~lang =
    let open Types.Ast_Stmt in
    match self with
    | Expression (e, _) -> expr_to_string ~indent e ~lang
    | Let {name = name, _; is_mut; ty; expr} ->
        let ty =
          match ty with
          | Some (ty, _) -> ": " ^ Type.to_string ~lang ty
          | None -> ""
        in
        let mut =
          if is_mut
          then Lang.keyword_to_string ~lang Kw.Mut ^ " "
          else ""
        in
        let expr, _ = expr in
        String.concat
          [ Lang.keyword_to_string ~lang Kw.Let
          ; " "
          ; mut
          ; Name.to_ident_string name
          ; ty
          ; " = "
          ; expr_to_string expr ~indent ~lang ]
end

module Stmt = struct
  include Types.Ast_Stmt

  let to_string = Implementation_stmt_expr.stmt_to_string
end

module Expr = struct
  module Block = struct
    include Types.Ast_Expr_Block

    let to_string = Implementation_stmt_expr.block_to_string

    let stmts (Block r) = r.stmts

    let expr (Block r) = r.expr

    let with_stmt (Block r) stmt =
      Block {r with stmts = stmt :: r.stmts}
  end

  include Types.Ast_Expr

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

  let attributes (Func r) = r.attributes

  let to_string self ~lang =
    let parameters =
      let f (((name, _), (ty, _)), _) =
        String.concat
          [Name.to_ident_string name; ": "; Type.to_string ~lang ty]
      in
      String.concat ~sep:", " (List.map ~f (params self))
    in
    let ret_ty =
      match ret_ty self with
      | Some (ty, _) -> " -> " ^ Type.to_string ~lang ty
      | None -> ""
    in
    String.concat
      [ Attribute.spanned_list_to_string ~lang (attributes self)
      ; Lang.keyword_to_string ~lang Kw.Func
      ; " "
      ; Name.to_ident_string (name self)
      ; "("
      ; parameters
      ; ")"
      ; ret_ty
      ; " "
      ; (let body, _ = body self in
         Expr.Block.to_string ~indent:0 body ~lang)
      ; "\n" ]
end

module Infix_group = struct
  include Types.Ast_Infix_group

  let to_string grp ~lang =
    let (Infix_group r) = grp in
    let name, _ = r.name in
    let associativity =
      let ckw =
        match r.associativity with
        | Assoc_start -> Ckw.Start
        | Assoc_end -> Ckw.End
        | Assoc_none -> Ckw.None
      in
      Lang.contextual_keyword_to_string ~lang ckw
    in
    let precedence =
      let f (Less (id, _)) =
        String.concat
          [ Lang.contextual_keyword_to_string ~lang Ckw.Precedence
          ; " < "
          ; (id :> string) ]
      in
      String.concat ~sep:"\n  " (List.map r.precedence ~f)
    in
    String.concat
      [ Attribute.spanned_list_to_string ~lang r.attributes
      ; Lang.keyword_to_string ~lang Kw.Infix
      ; " "
      ; Lang.keyword_to_string ~lang Kw.Group
      ; " "
      ; (name :> string)
      ; " {\n  "
      ; Lang.contextual_keyword_to_string ~lang Ckw.Associativity
      ; " = "
      ; associativity
      ; ";\n  "
      ; precedence
      ; "\n};" ]
end

module Infix_declaration = struct
  include Types.Ast_Infix_declaration

  let to_string decl ~lang =
    let (Infix_declaration r) = decl in
    let name, _ = r.name in
    let group, _ = r.group in
    String.concat
      [ Attribute.spanned_list_to_string ~lang r.attributes
      ; Lang.keyword_to_string ~lang Kw.Infix
      ; " ("
      ; infix_name_string name
      ; "): "
      ; (group :> string)
      ; ";" ]
end

include Types.Ast

let funcs (Ast r) = r.funcs

let infix_decls (Ast r) = r.infix_decls

let infix_groups (Ast r) = r.infix_groups

let types (Ast r) = r.types

let with_func (Ast r) func = Ast {r with funcs = func :: r.funcs}

let with_infix_decl (Ast r) infix_decl =
  Ast {r with infix_decls = infix_decl :: r.infix_decls}

let with_infix_group (Ast r) infix_group =
  Ast {r with infix_groups = infix_group :: r.infix_groups}

let with_type (Ast r) type_ = Ast {r with types = type_ :: r.types}

let to_string self ~lang =
  let types =
    let f (ty, _) =
      let (Type.Definition.Definition r) = ty in
      let name, _ = r.name in
      let attributes =
        Attribute.spanned_list_to_string ~lang r.attributes
      in
      match r.kind with
      | Type.Definition.Alias data ->
          String.concat
            [ attributes
            ; Lang.keyword_to_string ~lang Kw.Type
            ; " "
            ; (name :> string)
            ; " = "
            ; Type.to_string ~lang data
            ; ";" ]
      | Type.Definition.User_defined {data} ->
          String.concat
            [ attributes
            ; Lang.keyword_to_string ~lang Kw.Type
            ; " "
            ; (name :> string)
            ; " {\n"
            ; "  "
            ; Type.Data.to_string
                ~name:(Lang.keyword_to_string ~lang Kw.Data)
                ~lang data
            ; ";\n}" ]
    in
    if List.is_empty (types self)
    then ""
    else String.concat ~sep:"\n" (List.map ~f (types self)) ^ "\n\n"
  in
  let infix_groups =
    let f (infix_group, _) = Infix_group.to_string ~lang infix_group in
    if List.is_empty (infix_groups self)
    then ""
    else
      String.concat ~sep:"\n" (List.map ~f (infix_groups self))
      ^ "\n\n"
  in
  let infix_decls =
    let f (infix_decl, _) =
      Infix_declaration.to_string ~lang infix_decl
    in
    if List.is_empty (infix_decls self)
    then ""
    else
      String.concat ~sep:"\n" (List.map ~f (infix_decls self)) ^ "\n\n"
  in
  let funcs =
    let f (func, _) = Func.to_string ~lang func in
    String.concat ~sep:"\n" (List.map ~f (funcs self))
  in
  String.concat [types; infix_groups; infix_decls; funcs]
