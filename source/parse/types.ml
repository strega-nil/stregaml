module rec Error_Expected : sig
  type t =
    | Specific of Token.t
    | Item_declarator
    | Identifier
    | Operator
    | Variable_decl
    | Type
    | Data
    | Association
    | Direction
    | Precedence
    | Expression
    | Expression_follow
    | Statement_end
    | Path_expression
end =
  Error_Expected

and Error : sig
  type t =
    | Malformed_input of string
    | Unclosed_comment
    | Operator_including_comment_token of Ident.t
    | Malformed_number_literal
    | Unrecognized_direction of Ident.t
    | Reserved_token of Ident.t
    | Unrecognized_character of Uchar.t
    | Unexpected_token of (Error_Expected.t * Token.t)
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
    | Colon
    | Double_colon
    | Operator of Ident.t
    | Identifier of Ident.t
    | Keyword_true
    | Keyword_false
    | Keyword_if
    | Keyword_else
    | Keyword_association
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

and Ast_Type : sig
  type t =
    | Named of Ident.t
    | Reference of {is_mut: bool; pointee: t Spanned.t}
    | Function of {params: t Spanned.t list; ret_ty: t Spanned.t option}
end =
  Ast_Type

and Ast_Type_Data : sig
  type t = Record of (Ident.t * Ast_Type.t) Spanned.t list
end =
  Ast_Type_Data

and Ast_Type_Definition : sig
  type kind = Alias of Ast_Type.t | User_defined of {data: Ast_Type_Data.t}

  type t = {name: Ident.t Spanned.t; kind: kind}
end =
  Ast_Type_Definition

and Ast_Expr : sig
  type infix = Infix_assign | Infix_name of Ident.t

  type block = {stmts: Ast_Stmt.t Spanned.t list; expr: t Spanned.t option}

  and t =
    | Unit_literal
    | Bool_literal of bool
    | Integer_literal of int
    | If_else of {cond: t Spanned.t; thn: block Spanned.t; els: block Spanned.t}
    | Name of {path: Ident.t list; name: Ident.t}
    | Block of block Spanned.t
    | Builtin of Ident.t Spanned.t * t Spanned.t list
    | Call of t Spanned.t * t Spanned.t list
    | Infix_list of t Spanned.t * (infix Spanned.t * t Spanned.t) list
    | Reference of {is_mut: bool; place: Ast_Expr.t Spanned.t}
    | Dereference of Ast_Expr.t Spanned.t
    | Record_literal of
        { ty: Ast_Type.t Spanned.t
        ; members: (Ident.t * t Spanned.t) Spanned.t list }
    | Record_access of t Spanned.t * Ident.t
end =
  Ast_Expr

and Ast_Stmt : sig
  type t =
    | Expression of Ast_Expr.t Spanned.t
    | Let of
        { name: Ident.t Spanned.t
        ; is_mut: bool
        ; ty: Ast_Type.t Spanned.t option
        ; expr: Ast_Expr.t Spanned.t }
end =
  Ast_Stmt

and Ast_Func : sig
  type t =
    { name: Ident.t
    ; params: (Ident.t Spanned.t * Ast_Type.t Spanned.t) Spanned.t list
    ; ret_ty: Ast_Type.t Spanned.t option
    ; body: Ast_Expr.block Spanned.t }
end =
  Ast_Func

and Ast_Association : sig
  type kind =
    | Direction_left
    | Direction_none
    | Equal of Ident.t Spanned.t
    | Less of Ident.t Spanned.t
    | Greater of Ident.t Spanned.t

  type t = {name: Ident.t Spanned.t; kind: kind}
end =
  Ast_Association

and Ast : sig
  type t =
    { funcs: Ast_Func.t Spanned.t list
    ; assocs: Ast_Association.t Spanned.t list
    ; types: Ast_Type_Definition.t Spanned.t list }
end =
  Ast

module Pervasives = struct
  include Spanned.Result.Monad

  type error = Error.t

  type 'a result = ('a, error) Spanned.Result.t
end
