module Spanned = Cafec_containers.Spanned

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
  | Defined_type_multiple_times of string
  | Return_type_mismatch of {expected: Type.t; found: Type.t}
  | Invalid_function_arguments of {expected: Type.t list; found: Type.t list}

module Out = Stdio.Out_channel

let output f err ctxt =
  match err with
  | Name_not_found name -> Out.fprintf f "Name not found: %s" name
  | Type_not_found ty ->
      Out.output_string f "Type not found: " ;
      Cafec_parse.Ast.Type.output f ty
  | Struct_literal_of_non_struct_type ty ->
      Out.output_string f
        "Attempted to create a struct literal of non-struct type: " ;
      Type.output f ty ctxt
  | Struct_literal_with_unknown_member_name (ty, name) ->
      Out.output_string f "Struct literal of type `" ;
      Type.output f ty ctxt ;
      Out.fprintf f "` has an unknown initializer `%s`" name
  | Struct_literal_without_member (ty, name) ->
      Out.output_string f "Struct literal of type `" ;
      Type.output f ty ctxt ;
      Out.fprintf f "` has no initializer for member `%s`" name
  | Struct_literal_incorrect_member_type {ty; member; expected; found} ->
      Out.output_string f "Struct literal of type `" ;
      Type.output f ty ctxt ;
      Out.fprintf f "` - mismatched member `%s` type:" member ;
      Out.output_string f "\n  expected: " ;
      Type.output f expected ctxt ;
      Out.output_string f "\n  found: " ;
      Type.output f found ctxt
  | Struct_literal_member_defined_multiple_times (ty, member) ->
      Out.output_string f "Struct literal of type `" ;
      Type.output f ty ctxt ;
      Out.fprintf f "` - member `%s` initialized multiple times" member
  | Struct_access_on_non_struct_type (ty, member) ->
      Out.fprintf f
        "Attempted to access the `%s` member of a non-struct type: " member ;
      Type.output f ty ctxt
  | Struct_access_non_member (ty, member) ->
      Out.fprintf f
        "Attempted to access the `%s` member of a type without that member: "
        member ;
      Type.output f ty ctxt
  | If_on_non_bool ty ->
      Out.output_string f
        "Attempted to `if` on an expression of non-boolean type (" ;
      Type.output f ty ctxt ;
      Out.output_char f ')'
  | If_branches_of_differing_type (t1, t2) ->
      Out.output_string f
        "`if-else` expression had branches of differing types:" ;
      Out.output_string f "\n  1st branch: " ;
      Type.output f t1 ctxt ;
      Out.output_string f "\n  2nd branch: " ;
      Type.output f t2 ctxt
  | Call_of_non_function ty ->
      Out.output_string f "Attempted to call a non-function type (" ;
      Type.output f ty ctxt ;
      Out.output_char f ')'
  | Defined_function_multiple_times {name; original_declaration} ->
      Out.fprintf f "Defined function %s multiple times.\n" name ;
      Out.output_string f "  (original definition at " ;
      Spanned.output_span f original_declaration ;
      Out.output_char f ')'
  | Defined_type_multiple_times name ->
      Out.fprintf f "Defined type %s multiple times." name
  | Return_type_mismatch {expected; found} ->
      Out.output_string f "Return value did not match the return type.\n" ;
      Out.output_string f "  expected: " ;
      Type.output f expected ctxt ;
      Out.output_string f ", found: " ;
      Type.output f found ctxt
  | Invalid_function_arguments {expected; found} ->
      Out.output_string f
        "Function arguments did not match the parameter types.\n" ;
      Out.output_string f "  expected: " ;
      Type.output_list f expected ctxt ;
      Out.output_string f ", found: " ;
      Type.output_list f found ctxt


let output_spanned f (e, sp) ctxt =
  output f e ctxt ;
  Out.output_string f "\n  at " ;
  Spanned.output_span f sp
