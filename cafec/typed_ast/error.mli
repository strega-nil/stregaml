open Cafec_spanned.Prelude

type t =
  | Name_not_found of string
  | Type_not_found of Cafec_parse.Ast.Type.builder
  | If_on_non_bool of Type.t
  | If_branches_of_differing_type of (Type.t * Type.t)
  | Call_of_non_function of Type.t
  | Defined_multiple_times of {name: string; original_declaration: span}
  | Return_type_mismatch of {expected: Type.t; found: Type.t}
  | Invalid_function_arguments of {expected: Type.t list; found: Type.t list}

module Monad_spanned : module type of Cafec_spanned.Monad (struct
  type nonrec t = t end)

val print : t -> unit

val print_spanned : t spanned -> unit
