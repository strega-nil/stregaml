open Cafec_spanned.Prelude

type t =
  | Name_not_found of string
  | Type_not_found of Cafec_parse.Ast.Type.builder
  | Return_type_mismatch of {func_name: string; expected: Type.t; found: Type.t}
  | Invalid_function_arguments of
      { func_name: string
      ; expected: Type.t list
      ; found: Type.t list }

module Monad_spanned : module type of Cafec_spanned.Monad (struct
  type nonrec t = t end)

val print : t -> unit

val print_spanned : t spanned -> unit
