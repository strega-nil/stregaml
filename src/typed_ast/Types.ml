module rec Error : sig
  type t =
    | Name_not_found : Name.t -> t
    | Name_not_found_in_type : Type.t * Name.t -> t
    | Type_not_found : Nfc_string.t -> t
    | Type_defined_multiple_times : Nfc_string.t -> t
    | Infix_group_not_found : Nfc_string.t -> t
    | Infix_group_defined_multiple_times : Nfc_string.t -> t
    | Infix_group_recursive_precedence : Nfc_string.t * Nfc_string.t -> t
    | Incorrect_let_type : {name: Name.t; let_ty: Type.t; expr_ty: Type.t} -> t
    | Assignment_to_incompatible_type : {dest: Type.t; source: Type.t} -> t
    | Assignment_to_value : t
    | Assignment_to_immutable_place : t
    | Reference_taken_to_value : Type.mutability -> t
    | Mutable_reference_taken_to_immutable_place : t
    | Dereference_of_non_reference : Type.t -> t
    | Record_literal_non_record_type : Type.t -> t
    | Record_literal_duplicate_members : Nfc_string.t -> t
    | Record_literal_incorrect_type :
        { field: Name.t
        ; field_ty: Type.t
        ; member_ty: Type.t }
        -> t
    | Record_literal_extra_field : Type.t * Name.t -> t
    | Record_literal_missing_field : Type.t * Nfc_string.t -> t
    | Record_access_non_record_type : Type.t * Name.t -> t
    | Record_access_non_member : Type.t * Name.t -> t
    | Match_non_variant_type : Type.t -> t
    | Match_branches_of_different_type : {expected: Type.t; found: Type.t} -> t
    | Match_repeated_branches : Nfc_string.t -> t
    | Match_missing_branch : Nfc_string.t -> t
    | Pattern_of_wrong_type : {expected: Type.t; found: Type.t} -> t
    | If_non_bool : Type.t -> t
    | If_branches_of_differing_type : Type.t * Type.t -> t
    | Builtin_mismatched_arity :
        { name: Nfc_string.t
        ; expected: int
        ; found: int }
        -> t
    | Builtin_invalid_arguments : {name: Nfc_string.t; found: Type.t list} -> t
    | Unordered_operators :
        { op1: Cafec_Parse.Ast.Expr.infix Spanned.t
        ; op2: Cafec_Parse.Ast.Expr.infix Spanned.t }
        -> t
    | Unknown_builtin : Nfc_string.t -> t
    | Call_of_non_function : Type.t -> t
    | Prefix_function_wrong_arity : {name: Name.t; num_params: int} -> t
    | Infix_function_wrong_arity : {name: Name.t; num_params: int} -> t
    | Defined_function_multiple_times :
        { name: Name.t
        ; original_declaration: Spanned.Span.t }
        -> t
    | Defined_type_multiple_times : Nfc_string.t -> t
    | Return_type_mismatch : {expected: Type.t; found: Type.t} -> t
    | Invalid_function_arguments :
        { expected: Type.t list
        ; found: Type.t list }
        -> t
end =
  Error

and Type : sig
  type mutability = Immutable : mutability | Mutable : mutability

  type builtin =
    | Unit : builtin
    | Bool : builtin
    | Int32 : builtin
    | Reference : {mutability: mutability; pointee: t} -> builtin
    | Function : {params: t list; ret_ty: t} -> builtin

  and t = Builtin : builtin -> t | User_defined : int -> t
end =
  Type

and Type_Structural : sig
  type members = (Nfc_string.t * Type.t) Array.t

  type t =
    | Builtin : Type.builtin -> t
    | Record : members -> t
    | Variant : members -> t
end =
  Type_Structural

and Type_Structural_Kind : sig
  type t = Cafec_Parse.Ast.Type.Data.kind = Record | Variant
end =
  Type_Structural_Kind

and Ast_Expr_Type : sig
  (* type owned = Owned | Borrowed *)

  type category =
    | Value : category
    | Place : {mutability: Type.mutability (* ; owned: owned *)} -> category

  type t = Type : {category: category; ty: Type.t} -> t
end =
  Ast_Expr_Type

and Ast_Binding : sig
  type t =
    | Binding :
        { name: Name.t Spanned.t
        ; mutability: Type.mutability
        ; ty: Type.t }
        -> t
end =
  Ast_Binding

and Ast_Stmt : sig
  type t =
    | Expression : Ast_Expr.t -> t
    | Let : {binding: Ast_Binding.t; expr: Ast_Expr.t Spanned.t} -> t
end =
  Ast_Stmt

and Ast_Expr_Local : sig
  type t = Local : {binding: Ast_Binding.t; index: int} -> t
end =
  Ast_Expr_Local

and Ast_Expr_Block : sig
  type t =
    | Block :
        { stmts: Ast_Stmt.t Spanned.t list
        ; expr: Ast_Expr.t Spanned.t option }
        -> t
end =
  Ast_Expr_Block

and Ast_Expr : sig
  type variant =
    | Unit_literal : variant
    | Bool_literal : bool -> variant
    | Integer_literal : int -> variant
    | Match :
        { cond: t Spanned.t
        ; arms: (Type.t * Ast_Expr_Block.t Spanned.t) Array.t }
        -> variant
    | If_else :
        { cond: t Spanned.t
        ; thn: Ast_Expr_Block.t Spanned.t
        ; els: Ast_Expr_Block.t Spanned.t }
        -> variant
    | Assign : {dest: t Spanned.t; source: t Spanned.t} -> variant
    | Builtin : Ast_Expr_Builtin.t -> variant
    | Call : t Spanned.t * t Spanned.t list -> variant
    | Block : Ast_Expr_Block.t Spanned.t -> variant
    | Reference : {mutability: Type.mutability; place: t Spanned.t} -> variant
    | Dereference : t Spanned.t -> variant
    | Record_literal : {ty: Type.t Spanned.t; members: t Array.t} -> variant
    | Record_access : t Spanned.t * int -> variant
    | Global_function : int -> variant
    | Constructor : Type.t * int -> variant
    | Local : Ast_Expr_Local.t -> variant

  and t = Expr : {variant: variant; ty: Ast_Expr_Type.t} -> t
end =
  Ast_Expr

and Ast_Expr_Builtin : sig
  type t =
    | Less_eq : Ast_Expr.t Spanned.t * Ast_Expr.t Spanned.t -> t
    | Add : Ast_Expr.t Spanned.t * Ast_Expr.t Spanned.t -> t
    | Sub : Ast_Expr.t Spanned.t * Ast_Expr.t Spanned.t -> t
    | Mul : Ast_Expr.t Spanned.t * Ast_Expr.t Spanned.t -> t
end =
  Ast_Expr_Builtin

module Pervasives = struct
  include Spanned.Result.Monad
  module Error = Error

  type error = Error.t

  type 'a result = ('a, error) Spanned.Result.t
end
