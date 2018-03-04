module Spanned = Cafec_spanned
open Spanned.Prelude

let print_indent indent =
  let rec helper = function
    | n when n <= 0 -> ()
    | n ->
        print_string "  " ;
        helper (n - 1)
  in
  if indent < 0 then raise (Invalid_argument "Parse.Ast.print_indent")
  else helper indent


module Expr = struct
  type builder =
    | Unit_literal
    | Bool_literal of bool
    | Integer_literal of int
    | If_else of (t * t * t)
    | Variable of string
    | Call of (t * t list)

  and t = builder spanned

  let rec print indent (e, _) =
    let rec print_args ?start = function
      | [] -> ()
      | arg :: args ->
          (match start with Some () -> print_string ", " | None -> ()) ;
          print (indent + 1) arg ;
          print_args ~start:() args
    in
    match e with
    | Unit_literal -> print_string "()"
    | Bool_literal true -> print_string "true"
    | Bool_literal false -> print_string "false"
    | Integer_literal n -> print_int n
    | If_else (cond, thn, els) ->
        let print_inner = print (indent + 1) in
        print_string "if (" ;
        print_inner cond ;
        print_string ") {\n" ;
        print_indent (indent + 1) ;
        print_inner thn ;
        print_char '\n' ;
        print_indent indent ;
        print_string "} else {\n" ;
        print_indent (indent + 1) ;
        print_inner els ;
        print_char '\n' ;
        print_indent indent ;
        print_char '}'
    | Variable s -> print_string s
    | Call (e, args) ->
        print indent e ; print_char '(' ; print_args args ; print_char ')'
end

module Type = struct
  type builder = Named of string
 and t = builder spanned

  let print (Named self) = print_string self
end

module Function = struct
  type builder =
    { name: string
    ; params: (string * Type.t) list
    ; ret_ty: Type.t option
    ; expr: Expr.t }

  type t = builder spanned

  let print (self, _) =
    let print_parameter_list lst =
      let rec helper ?start = function
        | (name, (ty, _)) :: xs ->
            (match start with Some () -> print_string ", " | None -> ()) ;
            print_string name ;
            print_string ": " ;
            Type.print ty ;
            helper ~start:() xs
        | [] -> ()
      in
      print_char '(' ; helper lst ; print_char ')'
    in
    print_string "let " ;
    print_string self.name ;
    print_parameter_list self.params ;
    ( match self.ret_ty with
    | Some (ty, _) -> print_string ": " ; Type.print ty
    | None -> print_char ' ' ) ;
    print_string " =\n" ;
    print_indent 1 ;
    Expr.print 1 self.expr ;
    print_string ";\n"
end

type t = {funcs: Function.t list}

let make funcs = {funcs}

let print self =
  let rec helper = function
    | x :: xs -> Function.print x ; helper xs
    | [] -> ()
  in
  helper self.funcs
