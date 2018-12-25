module rec Error_Expected : sig
  type t =
    | Specific : Token.t -> t
    | Item_declarator : t
    | Identifier : t
    | Operator : t
    | Name : t
    | Variable_decl : t
    | Type : t
    | Data : t
    | Associativity : t
    | Precedence : t
    | Infix_group_member : t
    | Infix_follow : t
    | Expression : t
    | Expression_follow : t
    | Statement_end : t
    | Path_expression : t
    | Path : t
    | Match_arm : t
end =
  Error_Expected

and Error : sig
  type t =
    | Malformed_input : string -> t
    | Unclosed_comment : t
    | Identifier_operator_is_keyword : Token.t -> t
    | Identifier_operator_start_without_ident : Uchar.t option -> t
    | Malformed_number_literal : t
    | Associativity_defined_twice : Nfc_string.t -> t
    | Reserved_token : Nfc_string.t -> t
    | Unrecognized_character : Uchar.t -> t
    | Unexpected_token : Error_Expected.t * Token.t -> t
end =
  Error

and Token : sig
  type t =
    | Open_paren : t
    | Close_paren : t
    | Open_brace : t
    | Close_brace : t
    | Open_square : t
    | Close_square : t
    | Semicolon : t
    | Dot : t
    | Comma : t
    | Integer_literal : int -> t
    | Assign : t
    | Arrow : t
    | Thicc_arrow : t
    | Colon : t
    | Double_colon : t
    | Operator : Nfc_string.t -> t
    | Identifier_operator : Nfc_string.t -> t
    | Identifier : Nfc_string.t -> t
    | Keyword_true : t
    | Keyword_false : t
    | Keyword_match : t
    | Keyword_if : t
    | Keyword_else : t
    | Keyword_infix : t
    | Keyword_prefix : t
    | Keyword_group : t
    | Keyword_func : t
    | Keyword_type : t
    | Keyword_data : t
    | Keyword_record : t
    | Keyword_variant : t
    | Keyword_alias : t
    | Keyword_let : t
    | Keyword_mut : t
    | Keyword_builtin : t
    | Keyword_underscore : t
    | Eof : t
end =
  Token

and Ast_Type : sig
  type t =
    | Named : Nfc_string.t -> t
    | Reference : {is_mut: bool; pointee: t Spanned.t} -> t
    | Function : {params: t Spanned.t list; ret_ty: t Spanned.t option} -> t
end =
  Ast_Type

and Ast_Type_Data : sig
  type members = (Nfc_string.t * Ast_Type.t) Spanned.t list

  type kind = Record | Variant

  type t = Data : {kind: kind; members: members} -> t
end =
  Ast_Type_Data

and Ast_Type_Definition : sig
  type kind =
    | Alias : Ast_Type.t -> kind
    | User_defined : {data: Ast_Type_Data.t} -> kind

  type t = Definition : {name: Nfc_string.t Spanned.t; kind: kind} -> t
end =
  Ast_Type_Definition

and Ast_Expr_Block : sig
  type t =
    | Block :
        { stmts: Ast_Stmt.t Spanned.t list
        ; expr: Ast_Expr.t Spanned.t option }
        -> t
end =
  Ast_Expr_Block

and Ast_Expr : sig
  type infix = Infix_assign : infix | Infix_name : Name.infix Name.t -> infix

  type _ qualified =
    | Qualified :
        { path: Nfc_string.t Spanned.t list
        ; name: 'f Name.t Spanned.t }
        -> 'f qualified

  type pattern =
    | Pattern :
        { constructor: Name.nonfix qualified Spanned.t
        ; binding: Name.anyfix Name.t Spanned.t }
        -> pattern

  type t =
    | Unit_literal : t
    | Bool_literal : bool -> t
    | Integer_literal : int -> t
    | Match :
        { cond: t Spanned.t
        ; arms: (pattern Spanned.t * Ast_Expr_Block.t Spanned.t) list }
        -> t
    | If_else :
        { cond: t Spanned.t
        ; thn: Ast_Expr_Block.t Spanned.t
        ; els: Ast_Expr_Block.t Spanned.t }
        -> t
    | Name : _ qualified -> t
    | Block : Ast_Expr_Block.t Spanned.t -> t
    | Builtin : Nfc_string.t Spanned.t * t Spanned.t list -> t
    | Call : t Spanned.t * t Spanned.t list -> t
    | Prefix_operator : Name.prefix Name.t Spanned.t * t Spanned.t -> t
    | Infix_list : t Spanned.t * (infix Spanned.t * t Spanned.t) list -> t
    | Reference : {is_mut: bool; place: Ast_Expr.t Spanned.t} -> t
    | Dereference : Ast_Expr.t Spanned.t -> t
    | Record_literal :
        { ty: Ast_Type.t Spanned.t
        ; members: (Name.nonfix Name.t * t Spanned.t) Spanned.t list }
        -> t
    | Record_access : t Spanned.t * Name.nonfix Name.t -> t
end =
  Ast_Expr

and Ast_Stmt : sig
  type t =
    | Expression : Ast_Expr.t Spanned.t -> t
    | Let :
        { name: Name.anyfix Name.t Spanned.t
        ; is_mut: bool
        ; ty: Ast_Type.t Spanned.t option
        ; expr: Ast_Expr.t Spanned.t }
        -> t
end =
  Ast_Stmt

and Ast_Func : sig
  type t =
    | Func :
        { name: _ Name.t
        ; params:
            (Name.anyfix Name.t Spanned.t * Ast_Type.t Spanned.t) Spanned.t
            list
        ; ret_ty: Ast_Type.t Spanned.t option
        ; body: Ast_Expr_Block.t Spanned.t }
        -> t
end =
  Ast_Func

and Ast_Infix_group : sig
  type associativity =
    | Assoc_start : associativity
    | Assoc_end : associativity
    | Assoc_none : associativity

  type order = Less : Nfc_string.t Spanned.t -> order

  type t =
    | Infix_group :
        { name: Nfc_string.t Spanned.t
        ; associativity: associativity
        ; precedence: order list }
        -> t
end =
  Ast_Infix_group

and Ast_Infix_declaration : sig
  type t =
    | Infix_declaration :
        { name: Name.infix Name.t Spanned.t
        ; group: Nfc_string.t Spanned.t }
        -> t
end =
  Ast_Infix_declaration

and Ast : sig
  type t =
    | Ast :
        { funcs: Ast_Func.t Spanned.t list
        ; infix_decls: Ast_Infix_declaration.t Spanned.t list
        ; infix_groups: Ast_Infix_group.t Spanned.t list
        ; types: Ast_Type_Definition.t Spanned.t list }
        -> t
end =
  Ast

module Pervasives = struct
  include Spanned.Result.Monad

  type error = Error.t

  type 'a result = ('a, error) Spanned.Result.t
end
