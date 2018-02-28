module Spanned = Cafec_spanned

type t =
  | Name_not_found of string
  | Type_not_found of Cafec_parse.Ast.Type.builder
  | Return_type_mismatch of {func_name: string; expected: Type.t; found: Type.t}
  | Invalid_function_arguments of
      { func_name: string
      ; expected: Type.t list
      ; found: Type.t list }

module Monad_spanned = Spanned.Monad (struct
  type nonrec t = t
end)

let print = function
  | Name_not_found name -> Printf.printf "Name not found: %s" name
  | Type_not_found ty ->
      print_string "Type not found: " ;
      Cafec_parse.Ast.Type.print ty
  | Return_type_mismatch {func_name; expected; found} ->
      Printf.printf "Return value of %s did not match the return type.\n"
        func_name ;
      print_string "  expected: " ;
      Type.print expected ;
      print_string ", found: " ;
      Type.print found
  | Invalid_function_arguments {func_name; expected; found} ->
      Printf.printf
        "Function arguments of %s did not match the parameter types.\n"
        func_name ;
      print_string "  expected: " ;
      Type.print_list expected ;
      print_string ", found: " ;
      Type.print_list found


let print_spanned (e, sp) =
  print e ; print_string " at " ; Spanned.print_span sp
