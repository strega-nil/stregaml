type t = Builtin of builtin | User_defined of int

and builtin =
  | Builtin_unit
  | Builtin_bool
  | Builtin_int
  | Builtin_function of {params: t list; ret_ty: t}

type definition = Def_alias of t | Def_struct of (string * t) list

type context = string list

module Out = Stdio.Out_channel

let rec output f ty ctxt =
  match ty with
  | Builtin Builtin_unit -> Out.output_string f "unit"
  | Builtin Builtin_bool -> Out.output_string f "bool"
  | Builtin Builtin_int -> Out.output_string f "int"
  | Builtin Builtin_function {params; ret_ty} ->
      Out.output_string f "func" ;
      output_list f params ctxt ;
      Out.output_string f " -> " ;
      output f ret_ty ctxt
  | User_defined i -> Out.output_string f (List.nth_exn ctxt i)


and output_list f lst ctxt =
  Out.output_char f '(' ;
  ( match lst with
  | x :: xs ->
      let rec helper = function
        | x :: xs -> Out.output_string f ", " ; output f x ctxt ; helper xs
        | [] -> ()
      in
      output f x ctxt ; helper xs
  | [] -> () ) ;
  Out.output_char f ')'


let rec equal l r =
  match (l, r) with
  | Builtin bl, Builtin br -> (
    match (bl, br) with
    | Builtin_unit, Builtin_unit -> true
    | Builtin_bool, Builtin_bool -> true
    | Builtin_int, Builtin_int -> true
    | Builtin_function fl, Builtin_function fr ->
        equal fl.ret_ty fr.ret_ty && List.equal fl.params fr.params ~equal
    | _ -> false )
  | User_defined l, User_defined r -> l = r
  | _ -> false
