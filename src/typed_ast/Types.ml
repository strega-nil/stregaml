module rec Error : sig
  type t =
    | Name_not_found of Name.t
    | Name_not_found_in_type of Type.t * Name.t
    | Type_not_found of Nfc_string.t
    | Type_defined_multiple_times of Nfc_string.t
    | Infix_group_not_found of Nfc_string.t
    | Infix_group_defined_multiple_times of Nfc_string.t
    | Incorrect_let_type of {name: Name.t; let_ty: Type.t; expr_ty: Type.t}
    | Assignment_to_incompatible_type of {dest: Type.t; source: Type.t}
    | Assignment_to_value
    | Assignment_to_immutable_place
    | Reference_taken_to_value of Type.mutability
    | Mutable_reference_taken_to_immutable_place
    | Dereference_of_non_reference of Type.t
    | Record_literal_non_record_type of Type.t
    | Record_literal_duplicate_members of Nfc_string.t
    | Record_literal_incorrect_type of
        { field: Nfc_string.t
        ; field_ty: Type.t
        ; member_ty: Type.t }
    | Record_literal_extra_field of Type.t * Nfc_string.t
    | Record_literal_missing_field of Type.t * Nfc_string.t
    | Record_access_non_record_type of Type.t * Nfc_string.t
    | Record_access_non_member of Type.t * Nfc_string.t
    | If_non_bool of Type.t
    | If_branches_of_differing_type of Type.t * Type.t
    | Builtin_mismatched_arity of
        { name: Nfc_string.t
        ; expected: int
        ; found: int }
    | Builtin_invalid_arguments of {name: Nfc_string.t; found: Type.t list}
    | Unordered_operators of
        { op1: Cafec_Parse.Ast.Expr.infix Spanned.t
        ; op2: Cafec_Parse.Ast.Expr.infix Spanned.t }
    | Unknown_builtin of Nfc_string.t
    | Call_of_non_function of Type.t
    | Defined_function_multiple_times of
        { name: Name.t
        ; original_declaration: Spanned.Span.t }
    | Defined_type_multiple_times of Nfc_string.t
    | Return_type_mismatch of {expected: Type.t; found: Type.t}
    | Invalid_function_arguments of {expected: Type.t list; found: Type.t list}
end =
  Error

and Type : sig
  type mutability = Immutable | Mutable

  type builtin =
    | Unit
    | Bool
    | Int32
    | Reference of {mutability: mutability; pointee: t}
    | Function of {params: t list; ret_ty: t}

  and t = Builtin of builtin | User_defined of int
end =
  Type

and Type_Structural : sig
  type t =
    | Builtin of Type.builtin
    | Record of (Nfc_string.t * Type.t) list
    | Variant of (Nfc_string.t * Type.t) list
end =
  Type_Structural

and Ast_Expr_Type : sig
  (* type owned = Owned | Borrowed *)

  type category =
    | Value
    | Place of {mutability: Type.mutability (* ; owned: owned *)}

  type t = {category: category; ty: Type.t}
end =
  Ast_Expr_Type

and Ast_Binding : sig
  type t =
    {name: Name.t Spanned.t; mutability: Type.mutability; ty: Type.t Spanned.t}
end =
  Ast_Binding

and Ast_Stmt : sig
  type t =
    | Expression of Ast_Expr.t
    | Let of {binding: Ast_Binding.t; expr: Ast_Expr.t Spanned.t}
end =
  Ast_Stmt

and Ast_Expr_Local : sig
  type t = {binding: Ast_Binding.t; index: int}
end =
  Ast_Expr_Local

and Ast_Expr : sig
  type block = {stmts: Ast_Stmt.t Spanned.t list; expr: t Spanned.t option}

  and variant =
    | Unit_literal
    | Bool_literal of bool
    | Integer_literal of int
    | If_else of {cond: t Spanned.t; thn: block Spanned.t; els: block Spanned.t}
    | Assign of {dest: t Spanned.t; source: t Spanned.t}
    | Builtin of Ast_Expr_Builtin.t
    | Call of t Spanned.t * t Spanned.t list
    | Block of block Spanned.t
    | Reference of {mutability: Type.mutability; place: t Spanned.t}
    | Dereference of t Spanned.t
    | Record_literal of
        { ty: Type.t Spanned.t
        ; members: (Nfc_string.t * t Spanned.t) Spanned.t list }
    | Record_access of t Spanned.t * Nfc_string.t
    | Global_function of int
    | Constructor of Type.t * int
    | Local of Ast_Expr_Local.t

  and t = {variant: variant; ty: Ast_Expr_Type.t}
end =
  Ast_Expr

and Ast_Expr_Builtin : sig
  type t =
    | Less_eq of Ast_Expr.t Spanned.t * Ast_Expr.t Spanned.t
    | Add of Ast_Expr.t Spanned.t * Ast_Expr.t Spanned.t
    | Sub of Ast_Expr.t Spanned.t * Ast_Expr.t Spanned.t
    | Mul of Ast_Expr.t Spanned.t * Ast_Expr.t Spanned.t
end =
  Ast_Expr_Builtin

module Pervasives = struct
  include Spanned.Result.Monad
  module Error = Error

  type error = Error.t

  type 'a result = ('a, error) Spanned.Result.t
end
