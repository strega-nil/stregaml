module rec Error_Expected : sig
  type t =
    | Specific of Token.t
    | Item_declarator
    | Identifier
    | Operator
    | Name
    | Variable_decl
    | Type
    | Data
    | Associativity
    | Precedence
    | Infix_group_member
    | Infix_follow
    | Expression
    | Expression_follow
    | Statement_end
    | Path_expression
    | Path
    | Match_arm
end =
  Error_Expected

and Error : sig
  type t =
    | Malformed_input of string
    | Unclosed_comment
    | Operator_including_comment_token of Nfc_string.t
    | Identifier_operator_is_keyword of Token.t
    | Malformed_number_literal
    | Associativity_defined_twice of Nfc_string.t
    | Reserved_token of Nfc_string.t
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
    | Thicc_arrow
    | Colon
    | Double_colon
    | Operator of Nfc_string.t
    | Identifier_operator of Nfc_string.t
    | Identifier of Nfc_string.t
    | Keyword_true
    | Keyword_false
    | Keyword_match
    | Keyword_if
    | Keyword_else
    | Keyword_infix
    | Keyword_prefix
    | Keyword_group
    | Keyword_func
    | Keyword_type
    | Keyword_data
    | Keyword_record
    | Keyword_variant
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
    | Named of Nfc_string.t
    | Reference of {is_mut: bool; pointee: t Spanned.t}
    | Function of {params: t Spanned.t list; ret_ty: t Spanned.t option}
end =
  Ast_Type

and Ast_Type_Data : sig
  type t =
    | Record of (Nfc_string.t * Ast_Type.t) Spanned.t list
    | Variant of (Nfc_string.t * Ast_Type.t) Spanned.t list
end =
  Ast_Type_Data

and Ast_Type_Definition : sig
  type kind = Alias of Ast_Type.t | User_defined of {data: Ast_Type_Data.t}

  type t = {name: Nfc_string.t Spanned.t; kind: kind}
end =
  Ast_Type_Definition

and Ast_Expr : sig
  type infix = Infix_assign | Infix_name of Name.t

  type qualified_name =
    {path: Nfc_string.t Spanned.t list; name: Name.t Spanned.t}

  type pattern =
    {constructor: qualified_name Spanned.t; binding: Name.t Spanned.t}

  type block = {stmts: Ast_Stmt.t Spanned.t list; expr: t Spanned.t option}

  and t =
    | Unit_literal
    | Bool_literal of bool
    | Integer_literal of int
    | Match of
        { cond: t Spanned.t
        ; arms: (pattern Spanned.t * block Spanned.t) list }
    | If_else of {cond: t Spanned.t; thn: block Spanned.t; els: block Spanned.t}
    | Name of qualified_name
    | Block of block Spanned.t
    | Builtin of Nfc_string.t Spanned.t * t Spanned.t list
    | Call of t Spanned.t * t Spanned.t list
    | Prefix_operator of Name.t Spanned.t * t Spanned.t
    | Infix_list of t Spanned.t * (infix Spanned.t * t Spanned.t) list
    | Reference of {is_mut: bool; place: Ast_Expr.t Spanned.t}
    | Dereference of Ast_Expr.t Spanned.t
    | Record_literal of
        { ty: Ast_Type.t Spanned.t
        ; members: (Nfc_string.t * t Spanned.t) Spanned.t list }
    | Record_access of t Spanned.t * Nfc_string.t
end =
  Ast_Expr

and Ast_Stmt : sig
  type t =
    | Expression of Ast_Expr.t Spanned.t
    | Let of
        { name: Name.t Spanned.t
        ; is_mut: bool
        ; ty: Ast_Type.t Spanned.t option
        ; expr: Ast_Expr.t Spanned.t }
end =
  Ast_Stmt

and Ast_Func : sig
  type t =
    { name: Name.t
    ; params: (Name.t Spanned.t * Ast_Type.t Spanned.t) Spanned.t list
    ; ret_ty: Ast_Type.t Spanned.t option
    ; body: Ast_Expr.block Spanned.t }
end =
  Ast_Func

and Ast_Infix_group : sig
  type associativity = Assoc_start | Assoc_end | Assoc_none

  type order = Less of Nfc_string.t Spanned.t

  type t =
    { name: Nfc_string.t Spanned.t
    ; associativity: associativity
    ; precedence: order list }
end =
  Ast_Infix_group

and Ast_Infix_declaration : sig
  type t = {name: Name.t Spanned.t; group: Nfc_string.t Spanned.t}
end =
  Ast_Infix_declaration

and Ast : sig
  type t =
    { funcs: Ast_Func.t Spanned.t list
    ; infix_decls: Ast_Infix_declaration.t Spanned.t list
    ; infix_groups: Ast_Infix_group.t Spanned.t list
    ; types: Ast_Type_Definition.t Spanned.t list }
end =
  Ast

module Pervasives = struct
  include Spanned.Result.Monad

  type error = Error.t

  type 'a result = ('a, error) Spanned.Result.t
end
