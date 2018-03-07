type t = Builtin of builtin | User_defined of int

and builtin =
  | Builtin_unit
  | Builtin_bool
  | Builtin_int
  | Builtin_function of {params: t list; ret_ty: t}

type definition = Def_alias of t | Def_struct of (string * t) list

type context = string list

let rec print ty (ctxt: string list) =
  match ty with
  | Builtin Builtin_unit -> print_string "unit"
  | Builtin Builtin_bool -> print_string "bool"
  | Builtin Builtin_int -> print_string "int"
  | Builtin Builtin_function {params; ret_ty} ->
      print_string "func" ;
      print_list params ctxt ;
      print_string " -> " ;
      print ret_ty ctxt
  | User_defined i -> print_string (List.nth_exn i ctxt)

and print_list lst ctxt =
  print_char '(' ;
  ( match lst with
  | x :: xs ->
      let rec helper = function
        | x :: xs -> print_string ", " ; print x ctxt ; helper xs
        | [] -> ()
      in
      print x ctxt ; helper xs
  | [] -> () ) ;
  print_char ')'
