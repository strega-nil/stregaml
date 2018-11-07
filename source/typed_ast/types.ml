module rec Error : sig
  type t =
    | Name_not_found of string
    | Type_not_found of string
    | Type_defined_multiple_times of string
    | Incorrect_let_type of {name: string; let_ty: Type.t; expr_ty: Type.t}
    | Assignment_to_incompatible_type of {dest: Type.t; source: Type.t}
    | Assignment_to_value
    | Assignment_to_immutable_place
    | Record_literal_non_record_type of Type.t
    | Record_literal_duplicate_members of string
    | Record_literal_incorrect_type of
        { field: string
        ; field_ty: Type.t
        ; member_ty: Type.t }
    | Record_literal_extra_field of Type.t * string
    | Record_literal_missing_field of Type.t * string
    | Record_access_non_record_type of Type.t * string
    | Record_access_non_member of Type.t * string
    | If_non_bool of Type.t
    | If_branches_of_differing_type of Type.t * Type.t
    | Call_of_non_function of Type.t
    | Defined_function_multiple_times of
        { name: string
        ; original_declaration: Spanned.Span.t }
    | Defined_type_multiple_times of string
    | Return_type_mismatch of {expected: Type.t; found: Type.t}
    | Invalid_function_arguments of {expected: Type.t list; found: Type.t list}
end =
  Error

and Type : sig
  type builtin = Unit | Bool | Int32 | Function of {params: t list; ret_ty: t}

  and t = Builtin of builtin | User_defined of int
end =
  Type

and Type_Structural : sig
  type t = Builtin of Type.builtin | Record of (string * Type.t) list
end =
  Type_Structural

and Ast_Expr_Type : sig
  type mutability = Immutable | Mutable

  (* type owned = Owned | Borrowed *)

  type category = Value | Place of {mutability: mutability (* owned: owned *)}

  type t = {category: category; ty: Type.t}
end =
  Ast_Expr_Type

and Ast_Binding : sig
  type t =
    { name: string Spanned.t
    ; mutability: Ast_Expr_Type.mutability
    ; ty: Type.t Spanned.t }
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
    | Call of t Spanned.t * t Spanned.t list
    | Block of block Spanned.t
    | Record_literal of
        { ty: Type.t Spanned.t
        ; members: (string * t Spanned.t) Spanned.t list }
    | Record_access of t Spanned.t * string
    | Builtin of Ast_Expr_Builtin.t
    | Global_function of int
    | Local of Ast_Expr_Local.t

  and t = {variant: variant; ty: Ast_Expr_Type.t}
end =
  Ast_Expr

and Ast_Expr_Builtin : sig
  type t = Less_eq | Add | Sub | Mul
end =
  Ast_Expr_Builtin

module Pervasives = struct
  include Cafec_containers
  include Spanned.Result.Monad
  module Error = Error

  type error = Error.t

  type 'a result = ('a, error) Spanned.Result.t
end
