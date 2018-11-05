module rec Error_expected : sig
  type t =
    | Specific of Token.t
    | Item_declarator
    | Identifier
    | Variable_decl
    | Type
    | Data
    | Expression
    | Expression_follow
    | Statement_end
    | Path_expression
end =
  Error_expected

and Error : sig
  type t =
    | Unclosed_comment
    | Operator_including_comment_token of string
    | Malformed_number_literal
    | Reserved_token of string
    | Unrecognized_character of char
    | Unexpected_token of (Error_expected.t * Token.t)
end =
  Error

and Token : sig
  type t =
    | Open_paren
    | Close_paren
    | Open_brace
    | Close_brace
    | Keyword of Token_keyword.t
    | Identifier of string
    | Operator of string
    | Integer_literal of int
    | Assign
    | Arrow
    | Colon
    | Double_colon
    | Equals
    | Semicolon
    | Dot
    | Comma
    | Eof
end =
  Token

and Token_keyword : sig
  type t =
    | True
    | False
    | If
    | Else
    | Func
    | Type
    | Data
    | Record
    | Alias
    | Let
    | Mut
    | Underscore
end =
  Token_keyword

and Ast_type : sig
  type t =
    | Named of string
    | Function of t Spanned.t list * t Spanned.t option
end =
  Ast_type

and Ast_type_data : sig
  type t = Record of (string * Ast_type.t) Spanned.t list
end =
  Ast_type_data

and Ast_type_definition : sig
  type kind = Alias of Ast_type.t | User_defined of {data: Ast_type_data.t}

  type t = {name: string; kind: kind}
end =
  Ast_type_definition

and Ast_expr : sig
  type block = {stmts: Ast_stmt.t Spanned.t list; expr: t Spanned.t option}

  and t =
    | Unit_literal
    | Bool_literal of bool
    | Integer_literal of int
    | If_else of {cond: t Spanned.t; thn: block Spanned.t; els: block Spanned.t}
    | Variable of {path: string list; name: string}
    | Block of block Spanned.t
    | Call of t Spanned.t * t Spanned.t list
    | Assign of {dest: Ast_expr.t Spanned.t; source: Ast_expr.t Spanned.t}
    | Record_literal of
        { ty: Ast_type.t Spanned.t
        ; members: (string * t Spanned.t) Spanned.t list }
    | Record_access of t Spanned.t * string
end =
  Ast_expr

and Ast_expr_operator : sig end = Ast_expr_operator

and Ast_stmt : sig
  type t =
    | Expression of Ast_expr.t Spanned.t
    | Let of
        { name: string Spanned.t
        ; is_mut: bool
        ; ty: Ast_type.t Spanned.t option
        ; expr: Ast_expr.t Spanned.t }
end =
  Ast_stmt

and Ast_func : sig
  type t =
    { name: string
    ; params: (string Spanned.t * Ast_type.t Spanned.t) Spanned.t list
    ; ret_ty: Ast_type.t Spanned.t option
    ; body: Ast_expr.block Spanned.t }
end =
  Ast_func

and Ast : sig
  type t =
    { funcs: Ast_func.t Spanned.t list
    ; types: Ast_type_definition.t Spanned.t list }
end =
  Ast
