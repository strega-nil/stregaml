type t = Builtin of builtin | User_defined of int

and builtin =
  | Builtin_unit
  | Builtin_bool
  | Builtin_int
  | Builtin_function of {params: t list; ret_ty: t}

let rec print = function
  | Builtin Builtin_unit -> print_string "unit"
  | Builtin Builtin_bool -> print_string "bool"
  | Builtin Builtin_int -> print_string "int"
  | Builtin Builtin_function {params; ret_ty} ->
      print_string "func" ;
      print_list params ;
      print_string " -> " ;
      print ret_ty
  | User_defined i -> assert false

and print_list lst =
  print_char '(' ;
  ( match lst with
  | x :: xs ->
      let rec helper = function
        | x :: xs -> print_string ", " ; print x ; helper xs
        | [] -> ()
      in
      print x ; helper xs
  | [] -> () ) ;
  print_char ')'
