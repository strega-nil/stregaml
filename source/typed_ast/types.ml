module rec Error : sig
  type t =
    | Name_not_found of string
    | Type_not_found of string
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
end = struct
  type t =
    | Name_not_found of string
    | Type_not_found of string
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
end

and Type : sig
  type builtin = Unit | Bool | Int | Function of {params: t list; ret_ty: t}

  and t = Builtin of builtin | User_defined of Type_context.index
end = struct
  type builtin = Unit | Bool | Int | Function of {params: t list; ret_ty: t}

  and t = Builtin of builtin | User_defined of Type_context.index
end

and Type_context : sig
  type t

  type underlying = Cafec_parse.Ast.Type.Definition.t Spanned.t list

  type index

  type index_underlying = int

  val of_underlying : underlying -> t

  val to_underlying : t -> underlying

  val index_of_underlying : index_underlying -> index

  val index_to_underlying : index -> index_underlying
end = struct
  type underlying = Cafec_parse.Ast.Type.Definition.t Spanned.t list

  type t = underlying

  type index_underlying = int

  type index = index_underlying

  let of_underlying x = x

  let to_underlying x = x

  let index_of_underlying x = x

  let index_to_underlying x = x
end

and Type_structural : sig
  type t = Builtin of Type.builtin | Record of (string * Type.t) list
end = struct
  type t = Builtin of Type.builtin | Record of (string * Type.t) list
end

and Ast_stmt : sig
  type t =
    | Expression of Ast_expr.t
    | Let of
        { name: string Spanned.t
        ; ty: Type.t Spanned.t
        ; expr: Ast_expr.t Spanned.t }
end = struct
  type t =
    | Expression of Ast_expr.t
    | Let of
        { name: string Spanned.t
        ; ty: Type.t Spanned.t
        ; expr: Ast_expr.t Spanned.t }
end

and Ast_expr : sig
  type block = {stmts: Ast_stmt.t Spanned.t list; expr: t Spanned.t option}

  and variant =
    | Unit_literal
    | Bool_literal of bool
    | Integer_literal of int
    | If_else of {cond: t Spanned.t; thn: block Spanned.t; els: block Spanned.t}
    | Call of t Spanned.t * t Spanned.t list
    | Block of block Spanned.t
    | Record_literal of
        { ty: Type.t Spanned.t
        ; members: (string * t Spanned.t) Spanned.t list }
    | Record_access of t Spanned.t * string
    | Builtin of Ast_expr_builtin.t
    | Global_function of int
    | Local of int

  and t = {variant: variant; ty: Type.t}
end = struct
  type block = {stmts: Ast_stmt.t Spanned.t list; expr: t Spanned.t option}

  and variant =
    | Unit_literal
    | Bool_literal of bool
    | Integer_literal of int
    | If_else of {cond: t Spanned.t; thn: block Spanned.t; els: block Spanned.t}
    | Call of t Spanned.t * t Spanned.t list
    | Block of block Spanned.t
    | Record_literal of
        { ty: Type.t Spanned.t
        ; members: (string * t Spanned.t) Spanned.t list }
    | Record_access of t Spanned.t * string
    | Builtin of Ast_expr_builtin.t
    | Global_function of int
    | Local of int

  and t = {variant: variant; ty: Type.t}
end

and Ast_expr_builtin : sig
  type t = Less_eq | Add | Sub | Mul
end = struct
  type t = Less_eq | Add | Sub | Mul
end

module Pervasives = struct
  include Cafec_containers
  include Spanned.Result.Monad
  module Error = Error

  type error = Error.t

  type 'a result = ('a, error) Spanned.Result.t
end
