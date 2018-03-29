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
      ; original_declaration: Spanned.Span.t }
  | Defined_type_multiple_times of string
  | Return_type_mismatch of {expected: Type.t; found: Type.t}
  | Invalid_function_arguments of {expected: Type.t list; found: Type.t list}

let to_string err ~ctxt =
  match err with
  | Name_not_found name -> Printf.sprintf "Name not found: %s" name
  | Type_not_found ty ->
      Printf.sprintf "Type not found: %s" (Cafec_parse.Ast.Type.to_string ty)
  | Struct_literal_of_non_struct_type ty ->
      Printf.sprintf
        "Attempted to create a struct literal of non-struct type: %s"
        (Type.to_string ty ~ctxt)
  | Struct_literal_with_unknown_member_name (ty, name) ->
      Printf.sprintf
        "Struct literal of type `%s` has an unknown initializer `%s`"
        (Type.to_string ty ~ctxt) name
  | Struct_literal_without_member (ty, name) ->
      Printf.sprintf
        "Struct literal of type `%s` has no initializer for member `%s`"
        (Type.to_string ty ~ctxt) name
  | Struct_literal_incorrect_member_type {ty; member; expected; found} ->
      Printf.sprintf
        {|Struct literal of type `%s` - mismatched member `%s` type:
  expected: %s
  found: %s|}
        (Type.to_string ty ~ctxt) member
        (Type.to_string expected ~ctxt)
        (Type.to_string found ~ctxt)
  | Struct_literal_member_defined_multiple_times (ty, member) ->
      Printf.sprintf
        "Struct literal of type `%s` - member `%s` initialized multiple times"
        (Type.to_string ty ~ctxt) member
  | Struct_access_on_non_struct_type (ty, member) ->
      Printf.sprintf
        "Attempted to access the `%s` member of a non-struct type: `%s`" member
        (Type.to_string ty ~ctxt)
  | Struct_access_non_member (ty, member) ->
      Printf.sprintf
        "Attempted to access the `%s` member of a type without that member: `%s`"
        member (Type.to_string ty ~ctxt)
  | If_on_non_bool ty ->
      Printf.sprintf
        "Attempted to `if` on an expression of non-boolean type `%s`"
        (Type.to_string ty ~ctxt)
  | If_branches_of_differing_type (t1, t2) ->
      Printf.sprintf
        {|`if-else` expression had branches of differing types:
  1st branch: `%s`
  2nd branch: `%s`|}
        (Type.to_string t1 ~ctxt) (Type.to_string t2 ~ctxt)
  | Call_of_non_function ty ->
      Printf.sprintf "Attempted to call a non-function type `%s`"
        (Type.to_string ty ~ctxt)
  | Defined_function_multiple_times {name; original_declaration} ->
      Printf.sprintf
        {|Defined function `%s` multiple times
  (original declaration at %s)|}
        name
        (Spanned.Span.to_string original_declaration)
  | Defined_type_multiple_times name ->
      Printf.sprintf "Defined type `%s` multiple times" name
  | Return_type_mismatch {expected; found} ->
      Printf.sprintf
        {|Return value did not match the return type.
  expected: `%s`
  found: `%s`|}
        (Type.to_string expected ~ctxt)
        (Type.to_string found ~ctxt)
  | Invalid_function_arguments {expected; found} ->
      let helper tys =
        let f ty = Type.to_string ty ~ctxt in
        String.concat ~sep:", " (List.map ~f tys)
      in
      Printf.sprintf
        {|Function arguments did not match the parameter types:
  expected: `(%s)`
  found: `(%s)`|}
        (helper expected) (helper found)
