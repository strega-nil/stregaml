open Cafec_spanned.Prelude

type t =
  | Name_not_found of string
  | Type_not_found of Cafec_parse.Ast.Type.t
  | Struct_literal_of_non_struct_type of Type.t
  | Struct_literal_with_unknown_member_name of (Type.t * string)
  | Struct_literal_without_member of (Type.t * string)
  | Struct_literal_incorrect_member_type of
      { ty: Type.t
      ; member: string
      ; expected: Type.t
      ; found: Type.t }
  | Struct_literal_member_defined_multiple_times of (Type.t * string)
  | Struct_access_on_non_struct_type of (Type.t * string)
  | Struct_access_non_member of (Type.t * string)
  | If_on_non_bool of Type.t
  | If_branches_of_differing_type of (Type.t * Type.t)
  | Call_of_non_function of Type.t
  | Defined_function_multiple_times of
      { name: string
      ; original_declaration: span }
  | Defined_type_multiple_times of string
  | Return_type_mismatch of {expected: Type.t; found: Type.t}
  | Invalid_function_arguments of {expected: Type.t list; found: Type.t list}

module Monad_spanned : module type of Cafec_spanned.Monad (struct
  type nonrec t = t end)

val print : t -> Type.context -> unit

val print_spanned : t spanned -> Type.context -> unit
