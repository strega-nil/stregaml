module rec Error_Expected : sig
  type t =
    | Specific : Token.t -> t
    | Item_declarator : t
    | Identifier : t
    | Operator : t
    | Name : t
    | Variable_decl : t
    | Value_type : t
    | Place : t
    | Type : t
    | Data : t
    | Associativity : t
    | Precedence : t
    | Integer_literal : t
    | Integer_data_member : t
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
    | Unrecognized_attribute : Nfc_string.t -> t
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
    | Attribute : t
    | Semicolon : t
    | Dot : t
    | Comma : t
    | Integer_literal : int -> t
    | Assign : t
    | Arrow : t
    | Thicc_arrow : t
    | Colon : t
    | Double_colon : t
    | Keyword : Token_Keyword.t -> t
    | Operator : Nfc_string.t -> t
    | Identifier_operator : Nfc_string.t -> t
    | Identifier : Nfc_string.t -> t
    | Eof : t
end =
  Token

and Token_Keyword : sig
  type t =
    | Match : t
    | If : t
    | Else : t
    | Infix : t
    | Prefix : t
    | Group : t
    | Func : t
    | Type : t
    | Data : t
    | Record : t
    | Variant : t
    | Integer : t
    | Alias : t
    | Let : t
    | Ref : t
    | Mut : t
    | Builtin : t
    | Underscore : t
end =
  Token_Keyword

and Token_Keyword_Contextual : sig
  type t =
    | Associativity : t
    | Precedence : t
    | Start : t
    | End : t
    | None : t
    | Bits : t
end =
  Token_Keyword_Contextual

and Token_Attribute : sig
  type t = Entrypoint : t
end =
  Token_Attribute

and Type : sig
  (* TODO: Raw, Owned *)

  type mutability =
    | Immutable : mutability
    | Mutable : mutability

  type value = Value_type

  type place = Place_type

  type any = Any_type

  type _ t =
    | Named : Nfc_string.t -> value t
    | Reference : place t Spanned.t -> value t
    | Tuple : value t Spanned.t list -> value t
    | Function :
        { params : any t Spanned.t list
        ; ret_ty : any t Spanned.t option }
        -> value t
    | Place :
        { mutability : Type.mutability Spanned.t
        ; ty : value t Spanned.t }
        -> place t
    | Any : _ t -> any t
end =
  Type

and Type_Data : sig
  type field =
    Nfc_string.t Spanned.t * Type.value Type.t Spanned.t

  type variant =
    Nfc_string.t Spanned.t * Type.value Type.t Spanned.t option

  type t =
    | Record : {fields : field Spanned.t list} -> t
    | Variant : {variants : variant Spanned.t list} -> t
    | Integer : {bits : int} -> t
end =
  Type_Data

and Type_Definition : sig
  type kind =
    | Alias : Type.value Type.t -> kind
    | User_defined : {data : Type_Data.t} -> kind

  type t =
    | Definition :
        { name : Nfc_string.t Spanned.t
        ; kind : kind
        ; attributes : Ast_Attribute.t Spanned.t list }
        -> t
end =
  Type_Definition

and Ast_Attribute : sig
  type t = Entrypoint : t
end =
  Ast_Attribute

and Ast_Expr_Block : sig
  type t =
    | Block :
        { stmts : Ast_Stmt.t Spanned.t list
        ; expr : Ast_Expr.t Spanned.t option }
        -> t
end =
  Ast_Expr_Block

and Ast_Expr : sig
  type infix =
    | Infix_assign : infix
    | Infix_name : Name.infix Name.t -> infix

  type _ qualified =
    | Qualified :
        { path : Nfc_string.t Spanned.t list
        ; name : 'f Name.t Spanned.t }
        -> 'f qualified

  type pattern =
    | Pattern :
        { constructor : Name.nonfix qualified Spanned.t
        ; binding : Name.anyfix Name.t Spanned.t option }
        -> pattern

  type t =
    | Integer_literal : int -> t
    | Tuple_literal : t Spanned.t list -> t
    | Match :
        { cond : t Spanned.t
        ; arms : (pattern Spanned.t * Ast_Expr_Block.t Spanned.t) list
        }
        -> t
    | Name : _ qualified -> t
    | Block : Ast_Expr_Block.t Spanned.t -> t
    | Builtin : Nfc_string.t Spanned.t * t Spanned.t list -> t
    | Call : t Spanned.t * t Spanned.t list -> t
    | Prefix_operator : Name.prefix Name.t Spanned.t * t Spanned.t -> t
    | Infix_list :
        t Spanned.t * (infix Spanned.t * t Spanned.t) list
        -> t
    | Place :
        { mutability : Type.mutability Spanned.t
        ; expr : t Spanned.t }
        -> t
    | Reference : Ast_Expr.t Spanned.t -> t
    | Dereference : Ast_Expr.t Spanned.t -> t
    | Record_literal :
        { ty : Type.value Type.t Spanned.t
        ; fields : (Name.nonfix Name.t * t Spanned.t) Spanned.t list
        }
        -> t
    | Record_access : t Spanned.t * Name.nonfix Name.t -> t
end =
  Ast_Expr

and Ast_Stmt : sig
  type t =
    | Expression : Ast_Expr.t Spanned.t -> t
    | Let :
        { name : Name.anyfix Name.t Spanned.t
        ; is_mut : bool
        ; ty : Type.any Type.t Spanned.t option
        ; expr : Ast_Expr.t Spanned.t }
        -> t
end =
  Ast_Stmt

and Ast_Func : sig
  type params =
    (Name.anyfix Name.t Spanned.t * Type.any Type.t Spanned.t)
    Spanned.t
    list

  type t =
    | Func :
        { name : _ Name.t
        ; params : params
        ; ret_ty : Type.any Type.t Spanned.t option
        ; body : Ast_Expr_Block.t Spanned.t
        ; attributes : Ast_Attribute.t Spanned.t list }
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
        { name : Nfc_string.t Spanned.t
        ; associativity : associativity
        ; precedence : order list
        ; attributes : Ast_Attribute.t Spanned.t list }
        -> t
end =
  Ast_Infix_group

and Ast_Infix_declaration : sig
  type t =
    | Infix_declaration :
        { name : Name.infix Name.t Spanned.t
        ; group : Nfc_string.t Spanned.t
        ; attributes : Ast_Attribute.t Spanned.t list }
        -> t
end =
  Ast_Infix_declaration

and Ast : sig
  type t =
    | Ast :
        { funcs : Ast_Func.t Spanned.t list
        ; infix_decls : Ast_Infix_declaration.t Spanned.t list
        ; infix_groups : Ast_Infix_group.t Spanned.t list
        ; types : Type_Definition.t Spanned.t list }
        -> t
end =
  Ast

module Pervasives = struct
  include Spanned.Result.Monad

  type error = Error.t

  type 'a result = ('a, error) Spanned.Result.t
end

module type Language = sig
  val contextual_keyword_of_string :
    Nfc_string.t -> Token_Keyword_Contextual.t option

  val contextual_keyword_to_string :
    Token_Keyword_Contextual.t -> string

  val keyword_of_string : Nfc_string.t -> Token_Keyword.t option

  val keyword_to_string : Token_Keyword.t -> string

  val attribute_of_string : Nfc_string.t -> Token_Attribute.t option

  val attribute_to_string : Token_Attribute.t -> string
end
