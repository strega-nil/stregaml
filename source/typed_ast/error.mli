module Spanned = Cafec_containers.Spanned

type t =
  | Name_not_found of string
  | Type_not_found of Cafec_parse.Ast.Type.t
  | Record_literal_duplicate_members of string
  | Record_access_on_non_record_type of Type.t * string
  | Record_access_non_member of Type.t * string
  | If_on_non_bool of Type.t
  | If_branches_of_differing_type of Type.t * Type.t
  | Call_of_non_function of Type.t
  | Defined_function_multiple_times of
      { name: string
      ; original_declaration: Spanned.Span.t }
  | Defined_type_multiple_times of string
  | Return_type_mismatch of {expected: Type.t; found: Type.t}
  | Invalid_function_arguments of {expected: Type.t list; found: Type.t list}

val to_string : t -> string
