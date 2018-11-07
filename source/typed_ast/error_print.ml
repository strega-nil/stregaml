open! Types.Pervasives
include Types.Error

let to_string err ~ctxt =
  match err with
  | Name_not_found name -> Printf.sprintf "Name not found: %s" name
  | Type_not_found ty -> Printf.sprintf "Type not found: %s" ty
  | Type_defined_multiple_times name ->
      Printf.sprintf "Type declared multiple times: %s" name
  | Incorrect_let_type {name; let_ty; expr_ty} ->
      Printf.sprintf
        {|Let binding of `%s` is typed incorrectly:
  let binding expected : %s
  expression is of type: %s|}
        name
        (Type.to_string let_ty ~ctxt)
        (Type.to_string expr_ty ~ctxt)
  | Assignment_to_incompatible_type {dest; source} ->
      Printf.sprintf
        {|Attempted to assign to incompatible type:
value is of type: `%s`
place is of type: `%s`|}
        (Type.to_string dest ~ctxt)
        (Type.to_string source ~ctxt)
  | Assignment_to_value -> "Attempted to assign to a value"
  | Assignment_to_immutable_place ->
      "Attempted to assign to an immutable place"
  | Record_literal_non_record_type ty ->
      Printf.sprintf
        "Attempted to create a record literal of non-record type `%s`"
        (Type.to_string ty ~ctxt)
  | Record_literal_duplicate_members member ->
      Printf.sprintf "Record literal - member `%s` initialized multiple times"
        member
  | Record_literal_incorrect_type {field; field_ty; member_ty} ->
      Printf.sprintf
        {|Record literal - initializing member `%s` with incorrect type:
  type of member    : %s
  type of expression: %s|}
        field
        (Type.to_string member_ty ~ctxt)
        (Type.to_string field_ty ~ctxt)
  | Record_literal_extra_field (ty, member) ->
      Printf.sprintf "Record literal - member `%s` not in type `%s`" member
        (Type.to_string ty ~ctxt)
  | Record_literal_missing_field (ty, member) ->
      Printf.sprintf "Record literal - member `%s` of type `%s` missing" member
        (Type.to_string ty ~ctxt)
  | Record_access_non_record_type (ty, member) ->
      Printf.sprintf
        "Attempted to access the `%s` member of a non-struct type: `%s`" member
        (Type.to_string ty ~ctxt)
  | Record_access_non_member (ty, member) ->
      Printf.sprintf
        "Attempted to access the `%s` member of a type without that member: \
         `%s`"
        member (Type.to_string ty ~ctxt)
  | If_non_bool ty ->
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
