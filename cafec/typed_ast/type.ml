type t =
  | Unit
  | Bool
  | Int

let print = function
  | Unit -> print_string "unit"
  | Bool -> print_string "bool"
  | Int -> print_string "int"
