module rec Error_expected : sig
  type t =
    | Specific of Token.t
    | Item_declarator
    | Identifier
    | Real_operator
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
    | Malformed_input of string
    | Unclosed_comment
    | Operator_including_comment_token of Ident.t
    | Malformed_number_literal
    | Reserved_token of Ident.t
    | Unrecognized_character of Uchar.t
    | Unexpected_token of (Error_expected.t * Token.t)
end =
  Error

and Token : sig
  type t =
    | Open_paren
    | Close_paren
    | Open_brace
    | Close_brace
    | Open_square
    | Close_square
    | Semicolon
    | Dot
    | Comma
    | Integer_literal of int
    | Assign
    | Arrow
    | Equals
    | Colon
    | Double_colon
    | Operator of Ident.t
    | Identifier of Ident.t
    | Keyword_true
    | Keyword_false
    | Keyword_if
    | Keyword_else
    | Keyword_func
    | Keyword_type
    | Keyword_data
    | Keyword_record
    | Keyword_alias
    | Keyword_let
    | Keyword_mut
    | Keyword_builtin
    | Keyword_underscore
    | Eof
end =
  Token

and Ast_type : sig
  type t =
    | Named of Ident.t
    | Reference of {is_mut: bool; pointee: t Spanned.t}
    | Function of {params: t Spanned.t list; ret_ty: t Spanned.t option}
end =
  Ast_type

and Ast_type_data : sig
  type t = Record of (Ident.t * Ast_type.t) Spanned.t list
end =
  Ast_type_data

and Ast_type_definition : sig
  type kind = Alias of Ast_type.t | User_defined of {data: Ast_type_data.t}

  type t = {name: Ident.t Spanned.t; kind: kind}
end =
  Ast_type_definition

and Ast_expr : sig
  type block = {stmts: Ast_stmt.t Spanned.t list; expr: t Spanned.t option}

  and t =
    | Unit_literal
    | Bool_literal of bool
    | Integer_literal of int
    | If_else of {cond: t Spanned.t; thn: block Spanned.t; els: block Spanned.t}
    | Variable of {path: Ident.t list; name: Ident.t}
    | Block of block Spanned.t
    | Builtin of Ident.t Spanned.t * t Spanned.t list
    | Call of t Spanned.t * t Spanned.t list
    | Assign of {dest: Ast_expr.t Spanned.t; source: Ast_expr.t Spanned.t}
    | Reference of {is_mut: bool; place: Ast_expr.t Spanned.t}
    | Dereference of Ast_expr.t Spanned.t
    | Record_literal of
        { ty: Ast_type.t Spanned.t
        ; members: (Ident.t * t Spanned.t) Spanned.t list }
    | Record_access of t Spanned.t * Ident.t
end =
  Ast_expr

and Ast_expr_operator : sig end = Ast_expr_operator

and Ast_stmt : sig
  type t =
    | Expression of Ast_expr.t Spanned.t
    | Let of
        { name: Ident.t Spanned.t
        ; is_mut: bool
        ; ty: Ast_type.t Spanned.t option
        ; expr: Ast_expr.t Spanned.t }
end =
  Ast_stmt

and Ast_func : sig
  type t =
    { name: Ident.t
    ; params: (Ident.t Spanned.t * Ast_type.t Spanned.t) Spanned.t list
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

module Pervasives = struct
  include Spanned.Result.Monad

  type error = Error.t

  type 'a result = ('a, error) Spanned.Result.t
end
