module Spanned = Cafec_spanned

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
      ; original_declaration: Spanned.span }
  | Return_type_mismatch of {expected: Type.t; found: Type.t}
  | Invalid_function_arguments of {expected: Type.t list; found: Type.t list}

module Monad_spanned = Spanned.Monad (struct
  type nonrec t = t
end)

let print err ctxt =
  match err with
  | Name_not_found name -> Printf.printf "Name not found: %s" name
  | Type_not_found ty ->
      print_string "Type not found: " ;
      Cafec_parse.Ast.Type.print ty
  | Struct_literal_of_non_struct_type ty ->
      print_string "Attempted to create a struct literal of non-struct type: " ;
      Type.print ty ctxt
  | Struct_literal_with_unknown_member_name (ty, name) ->
      print_string "Struct literal of type `" ;
      Type.print ty ctxt ;
      Printf.printf "` has an unknown initializer `%s`" name
  | Struct_literal_without_member (ty, name) ->
      print_string "Struct literal of type `" ;
      Type.print ty ctxt ;
      Printf.printf "` has no initializer for member `%s`" name
  | Struct_literal_incorrect_member_type {ty; member; expected; found} ->
      print_string "Struct literal of type `" ;
      Type.print ty ctxt ;
      Printf.printf "` - mismatched member `%s` type:" member ;
      print_string "\n  expected: " ;
      Type.print expected ctxt ;
      print_string "\n  found: " ;
      Type.print found ctxt
  | Struct_literal_member_defined_multiple_times (ty, member) ->
      print_string "Struct literal of type `" ;
      Type.print ty ctxt ;
      Printf.printf "` - member `%s` initialized multiple times" member
  | Struct_access_on_non_struct_type (ty, member) ->
      Printf.printf
        "Attempted to access the `%s` member of a non-struct type: "
        member ;
      Type.print ty ctxt
  | Struct_access_non_member (ty, member) ->
      Printf.printf
        "Attempted to access the `%s` member of a type without that member: "
        member ;
      Type.print ty ctxt
  | If_on_non_bool ty ->
      print_string "Attempted to `if` on an expression of non-boolean type (" ;
      Type.print ty ctxt ;
      print_char ')'
  | If_branches_of_differing_type (t1, t2) ->
      print_string "`if-else` expression had branches of differing types:" ;
      print_string "\n  1st branch: " ;
      Type.print t1 ctxt ;
      print_string "\n  2nd branch: " ;
      Type.print t2 ctxt
  | Call_of_non_function ty ->
      print_string "Attempted to call a non-function type (" ;
      Type.print ty ctxt ;
      print_char ')'
  | Defined_function_multiple_times {name; original_declaration} ->
      Printf.printf "Defined function %s multiple times.\n" name ;
      print_string "  (original definition at " ;
      Spanned.print_span original_declaration ;
      print_char ')'
  | Return_type_mismatch {expected; found} ->
      print_string "Return value did not match the return type.\n" ;
      print_string "  expected: " ;
      Type.print expected ctxt ;
      print_string ", found: " ;
      Type.print found ctxt
  | Invalid_function_arguments {expected; found} ->
      print_string "Function arguments did not match the parameter types.\n" ;
      print_string "  expected: " ;
      Type.print_list expected ctxt ;
      print_string ", found: " ;
      Type.print_list found ctxt


let print_spanned (e, sp) ctxt =
  print e ctxt ; print_string "\n  at " ; Spanned.print_span sp
