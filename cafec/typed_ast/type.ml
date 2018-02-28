type t = Unit | Bool | Int | Function of {params: t list; ret_ty: t}

let rec print = function
  | Unit -> print_string "unit"
  | Bool -> print_string "bool"
  | Int -> print_string "int"
  | Function {params; ret_ty} ->
      print_string "func" ;
      print_list params ;
      print_string " -> " ;
      print ret_ty

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
