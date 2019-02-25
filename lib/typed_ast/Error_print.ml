open! Types.Pervasives
include Types.Error

let to_string err ~ctxt =
  let type_list tys =
    let f ty = Type.to_string ty ~ctxt in
    String.concat_sequence ~sep:", " (Sequence.map ~f tys)
  in
  match err with
  | Name_not_found name ->
      Printf.sprintf "Name not found: %s" (Name.to_ident_string name)
  | Name_not_found_in_type {ty; name} ->
      Printf.sprintf "Name `%s` not found in type `%s`"
        (Name.to_ident_string name)
        (Type.to_string ty ~ctxt)
  | Type_not_found ty ->
      Printf.sprintf "Type not found: %s" (ty :> string)
  | Type_defined_multiple_times name ->
      Printf.sprintf "Type declared multiple times: %s" (name :> string)
  | Infix_group_not_found name ->
      Printf.sprintf "Infix group not found: %s" (name :> string)
  | Infix_group_defined_multiple_times name ->
      Printf.sprintf "Infix group declared multiple times: %s"
        (name :> string)
  | Infix_group_recursive_precedence (fst, snd) ->
      Printf.sprintf
        {|Infix groups declared with recursive precedence
  group 1: %s
  group 2: %s|}
        (fst :> string)
        (snd :> string)
  | Incorrect_let_type {name; let_ty; expr_ty} ->
      Printf.sprintf
        {|Let binding of `%s` is typed incorrectly:
  let binding expected : %s
  expression is of type: %s|}
        (Name.to_ident_string name)
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
  | Reference_taken_to_value ty ->
      Printf.sprintf
        "Attempted to take a reference to a value of type `%s`"
        (Type.to_string ty ~ctxt)
  | Mutable_reference_taken_to_immutable_place ->
      "Attempted to take a mutable reference to a mutable place"
  | Dereference_of_non_reference ty ->
      "Attempted to dereference non-reference type: "
      ^ Type.to_string ty ~ctxt
  | Record_literal_non_record_type ty ->
      Printf.sprintf
        "Attempted to create a record literal of non-record type `%s`"
        (Type.to_string ty ~ctxt)
  | Record_literal_duplicate_members member ->
      Printf.sprintf
        "Record literal - member `%s` initialized multiple times"
        (member :> string)
  | Record_literal_incorrect_type {field; field_ty; member_ty} ->
      Printf.sprintf
        {|Record literal - initializing member `%s` with incorrect type:
  type of member    : %s
  type of expression: %s|}
        (Name.to_ident_string field)
        (Type.to_string member_ty ~ctxt)
        (Type.to_string field_ty ~ctxt)
  | Record_literal_extra_field (ty, member) ->
      Printf.sprintf "Record literal - member `%s` not in type `%s`"
        (Name.to_ident_string member)
        (Type.to_string ty ~ctxt)
  | Record_literal_missing_field (ty, member) ->
      Printf.sprintf
        "Record literal - member `%s` of type `%s` missing"
        (member :> string)
        (Type.to_string ty ~ctxt)
  | Record_access_non_record_type (ty, member) ->
      Printf.sprintf
        "Attempted to access the `%s` member of a non-record type: `%s`"
        (Name.to_ident_string member)
        (Type.to_string ty ~ctxt)
  | Record_access_non_member (ty, member) ->
      Printf.sprintf
        "Attempted to access the `%s` member of a type without that member: `%s`"
        (Name.to_ident_string member)
        (Type.to_string ty ~ctxt)
  | Match_non_variant_type ty ->
      Printf.sprintf
        "Attempted to `match` on an expression of non-variant type `%s`"
        (Type.to_string ty ~ctxt)
  | Match_branches_of_different_type {expected; found} ->
      Printf.sprintf
        {|`match` expression has branches of differing types:
  expected: `%s`
  found: `%s`|}
        (Type.to_string expected ~ctxt)
        (Type.to_string found ~ctxt)
  | Match_repeated_branches name ->
      Printf.sprintf "`match` expression has duplicated pattern: `%s`"
        (name :> string)
  | Match_missing_branch name ->
      Printf.sprintf "`match` expression is missing pattern: `%s`"
        (name :> string)
  | Pattern_of_wrong_type {expected; found} ->
      Printf.sprintf
        {|`match` pattern of incorrect type:
  expected: `%s`
  found: `%s`|}
        (Type.to_string expected ~ctxt)
        (Type.to_string found ~ctxt)
  | If_non_bool ty ->
      Printf.sprintf
        "Attempted to `if` on an expression of non-boolean type `%s`"
        (Type.to_string ty ~ctxt)
  | If_branches_of_differing_type (t1, t2) ->
      Printf.sprintf
        {|`if-else` expression has branches of differing types:
  1st branch: `%s`
  2nd branch: `%s`|}
        (Type.to_string t1 ~ctxt)
        (Type.to_string t2 ~ctxt)
  | Builtin_mismatched_arity {name; expected; found} ->
      Printf.sprintf "Builtin `%s` expects %d arguments; found %d"
        (name :> string)
        expected found
  | Builtin_invalid_arguments {name; found} ->
      Printf.sprintf
        {|Builtin `%s` passed arguments of incorrect type:
  found: `(%s)`|}
        (name :> string)
        (type_list (Array.to_sequence found))
  | Unknown_builtin name ->
      Printf.sprintf "Builtin `%s` is unknown" (name :> string)
  | Unordered_operators {op1 = op1, _; op2 = op2, _} ->
      let module E = Cafec_Parse.Ast.Expr in
      let op_to_string = function
        | E.Infix_assign -> "<-"
        | E.Infix_name (Name.Name {string; kind = Name.Operator; _}) ->
            (string :> string)
        | E.Infix_name (Name.Name {string; kind = Name.Identifier; _})
          ->
            "\\" ^ (string :> string)
      in
      Printf.sprintf
        "Used mutually unordered operators `%s` and `%s` in the same expression"
        (op_to_string op1) (op_to_string op2)
  | Call_of_non_function ty ->
      Printf.sprintf "Attempted to call a non-function type `%s`"
        (Type.to_string ty ~ctxt)
  | Prefix_function_wrong_arity {name; num_params} ->
      Printf.sprintf
        {|Attempted to define prefix operator `%s` with invalid number of parameters.
  expected: 1
  found: %d|}
        (Name.string name :> string)
        num_params
  | Infix_function_wrong_arity {name; num_params} ->
      Printf.sprintf
        {|Attempted to define infix operator `%s` with invalid number of parameters.
  expected: 2
  found: %d|}
        (Name.string name :> string)
        num_params
  | Defined_function_multiple_times name ->
      Printf.sprintf "Defined function `%s` multiple times"
        (Name.to_ident_string name)
  | Defined_type_multiple_times name ->
      Printf.sprintf "Defined type `%s` multiple times" (name :> string)
  | Defined_infix_declaration_multiple_times name ->
      Printf.sprintf
        "Defined infix declaration for `%s` multiple times"
        (Name.to_ident_string name)
  | Return_type_mismatch {expected; found} ->
      Printf.sprintf
        {|Return value did not match the return type.
  expected: `%s`
  found: `%s`|}
        (Type.to_string expected ~ctxt)
        (Type.to_string found ~ctxt)
  | Invalid_function_arguments {expected; found} ->
      Printf.sprintf
        {|Function arguments did not match the parameter types:
  expected: `(%s)`
  found: `(%s)`|}
        (type_list (Array.to_sequence expected))
        (type_list (Array.to_sequence found))
