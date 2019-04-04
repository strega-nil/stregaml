module rec Error : sig
  type t =
    | Name_not_found : Name.anyfix Name.t -> t
    | Name_not_found_in_type :
        { ty : Type_Category.value Type.t
        ; name : Name.anyfix Name.t }
        -> t
    | Type_not_found : Nfc_string.t -> t
    | Type_defined_multiple_times : Nfc_string.t -> t
    | Infix_group_not_found : Nfc_string.t -> t
    | Infix_group_defined_multiple_times : Nfc_string.t -> t
    | Infix_group_recursive_precedence :
        Nfc_string.t * Nfc_string.t
        -> t
    | Incorrect_let_type :
        { name : _ Name.t
        ; let_ty : Type_Category.any Type.t
        ; expr_ty : Type_Category.any Type.t }
        -> t
    | Assignment_to_incompatible_type :
        { dest : Type_Category.value Type.t
        ; source : Type_Category.value Type.t }
        -> t
    | Assignment_to_value : t
    | Assignment_to_immutable_place : t
    | Reference_taken_to_value : Type_Category.value Type.t -> t
    | Mutable_reference_taken_to_immutable_place : t
    | Dereference_of_non_reference : Type_Category.value Type.t -> t
    | Record_literal_non_record_type : Type_Category.value Type.t -> t
    | Record_literal_duplicate_fields : Nfc_string.t -> t
    | Record_literal_incorrect_type :
        { field : Name.nonfix Name.t
        ; field_ty : Type_Category.value Type.t
        ; member_ty : Type_Category.value Type.t }
        -> t
    | Record_literal_extra_field :
        Type_Category.value Type.t * Name.nonfix Name.t
        -> t
    | Record_literal_missing_field :
        Type_Category.value Type.t * Nfc_string.t
        -> t
    | Record_access_non_record_type :
        Type_Category.value Type.t * Name.nonfix Name.t
        -> t
    | Record_access_non_field :
        Type_Category.value Type.t * Name.nonfix Name.t
        -> t
    | Match_non_variant_type : Type_Category.value Type.t -> t
    | Match_branches_of_different_type :
        { expected : Type_Category.value Type.t
        ; found : Type_Category.value Type.t }
        -> t
    | Match_repeated_branches : Nfc_string.t -> t
    | Match_missing_branch : Nfc_string.t -> t
    | Match_not_binding_data : t
    | Match_binding_without_data : t
    | Pattern_of_wrong_type :
        { expected : Type_Category.value Type.t
        ; found : Type_Category.value Type.t }
        -> t
    | If_non_bool : Type_Category.value Type.t -> t
    | If_branches_of_differing_type :
        Type_Category.value Type.t * Type_Category.value Type.t
        -> t
    | Builtin_mismatched_arity :
        { name : Nfc_string.t
        ; expected : int
        ; found : int }
        -> t
    | Builtin_invalid_arguments :
        { name : Nfc_string.t
        ; found : Type_Category.any Type.t Array.t }
        -> t
    | Unordered_operators :
        { op1 : Cafec_Parse.Ast.Expr.infix Spanned.t
        ; op2 : Cafec_Parse.Ast.Expr.infix Spanned.t }
        -> t
    | Unknown_builtin : Nfc_string.t -> t
    | Call_of_non_function : Type_Category.value Type.t -> t
    | Prefix_function_wrong_arity :
        { name : Name.prefix Name.t
        ; num_params : int }
        -> t
    | Infix_function_wrong_arity :
        { name : Name.infix Name.t
        ; num_params : int }
        -> t
    | Defined_function_multiple_times : Name.anyfix Name.t -> t
    | Defined_type_multiple_times : Nfc_string.t -> t
    | Defined_infix_declaration_multiple_times : Name.infix Name.t -> t
    | Return_type_mismatch :
        { expected : Type_Category.any Type.t
        ; found : Type_Category.any Type.t }
        -> t
    | Invalid_function_arguments :
        { expected : Type_Category.any Type.t Array.t
        ; found : Type_Category.any Type.t Array.t }
        -> t
    | Multiple_entrypoints : {first : _ Name.t; second : _ Name.t} -> t
end =
  Error

and Type : sig
  type _ t =
    | Structural : Type_Structural.t -> Type_Category.value t
    | User_defined : int -> Type_Category.value t
    | Place :
        { mutability : Type_Category.mutability
        ; ty : Type_Category.value t }
        -> Type_Category.place t
    | Any : _ t -> Type_Category.any t
end =
  Type

and Type_Structural : sig
  type t =
    | Tuple : Type_Category.value Type.t Array.t -> t
    | Reference : Type_Category.place Type.t -> t
    | Function :
        { params : Type_Category.any Type.t Array.t
        ; ret_ty : Type_Category.any Type.t }
        -> t
end =
  Type_Structural

and Type_Category : sig
  type mutability = Cafec_Parse.Type.mutability =
    | Immutable : mutability
    | Mutable : mutability

  type value = Cafec_Parse.Type.value

  type place = Cafec_Parse.Type.place

  type any = Cafec_Parse.Type.any

  type _ t =
    | Value : value t
    | Place : mutability -> place t
    | Any : _ t -> any t
end =
  Type_Category

and Type_Representation : sig
  type field =
    Nfc_string.t Spanned.t * Type_Category.value Type.t Spanned.t

  type variant =
    Nfc_string.t Spanned.t * Type_Category.value Type.t Spanned.t option

  type t =
    | Structural : Type_Structural.t -> t
    | Record : {fields : field Spanned.t Array.t} -> t
    | Variant : {variants : variant Spanned.t Array.t} -> t
    | Integer : {bits : int} -> t
end =
  Type_Representation

and Ast_Binding : sig
  type t =
    | Binding :
        { name : Name.anyfix Name.t Spanned.t
        ; is_mut : bool
        ; ty : Type_Category.any Type.t }
        -> t
end =
  Ast_Binding

and Ast_Stmt : sig
  type t =
    | Expression : Ast_Expr.t -> t
    | Let : {binding : Ast_Binding.t; expr : Ast_Expr.t Spanned.t} -> t
end =
  Ast_Stmt

and Ast_Expr_Local : sig
  type t = Local : {binding : Ast_Binding.t; index : int} -> t
end =
  Ast_Expr_Local

and Ast_Expr_Block : sig
  type t =
    | Block :
        { stmts : Ast_Stmt.t Spanned.t Array.t
        ; expr : Ast_Expr.t Spanned.t option }
        -> t
end =
  Ast_Expr_Block

and Ast_Expr : sig
  type _match_arm =
    Type_Category.any Type.t option * Ast_Expr_Block.t Spanned.t

  type variant =
    | Integer_literal : int -> variant
    | Tuple_literal : t Spanned.t array -> variant
    | Match :
        { cond : t Spanned.t
        ; arms : _match_arm Array.t }
        -> variant
    | Assign : {dest : t Spanned.t; source : t Spanned.t} -> variant
    | Builtin : Ast_Expr_Builtin.t -> variant
    | Call : t Spanned.t * t Spanned.t Array.t -> variant
    | Block : Ast_Expr_Block.t Spanned.t -> variant
    | Reference : t Spanned.t -> variant
    | Dereference : t Spanned.t -> variant
    | Record_literal :
        { ty : Type_Category.value Type.t Spanned.t
        ; fields : t Array.t }
        -> variant
    | Record_access : t Spanned.t * int -> variant
    | Global_function : int -> variant
    | Constructor : Type_Category.value Type.t * int -> variant
    | Local : Ast_Expr_Local.t -> variant

  and t =
    | Expr : {variant : variant; ty : Type_Category.any Type.t} -> t
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
