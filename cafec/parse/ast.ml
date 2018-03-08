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


module Type = struct
  type t = Named of string | Function of (t spanned list * t spanned option)

  let rec print = function
    | Named s -> print_string s
    | Function (parms, ret) ->
        let rec print_list = function
          | (x, _) :: xs -> print_string ", " ; print x ; print_list xs
          | [] -> ()
        in
        print_string "func(" ;
        (match parms with (x, _) :: xs -> print x ; print_list xs | [] -> ()) ;
        print_string ")" ;
        match ret with
        | Some (ret, _) -> print_string " -> " ; print ret
        | None -> ()
end

module Expr = struct
  type t =
    | Unit_literal
    | Bool_literal of bool
    | Integer_literal of int
    | If_else of (t spanned * t spanned * t spanned)
    | Variable of string
    | Call of (t spanned * t spanned list)
    | Struct_literal of (Type.t * (string * t spanned) spanned list)
    | Struct_access of (t spanned * string)

  let rec print indent e =
    let rec print_args ?start = function
      | [] -> ()
      | (arg, _) :: args ->
          (match start with Some () -> print_string ", " | None -> ()) ;
          print (indent + 1) arg ;
          print_args ~start:() args
    in
    match e with
    | Unit_literal -> print_string "()"
    | Bool_literal true -> print_string "true"
    | Bool_literal false -> print_string "false"
    | Integer_literal n -> print_int n
    | If_else ((cond, _), (thn, _), (els, _)) ->
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
    | Call ((e, _), args) ->
        print indent e ; print_char '(' ; print_args args ; print_char ')'
    | Struct_literal (ty, members) ->
        let rec helper = function
          | ((name, (expr, _)), _) :: xs ->
              print_string "; " ;
              print_string name ;
              print_string " = " ;
              print (indent + 1) expr ;
              helper xs
          | [] -> ()
        in
        Type.print ty ;
        print_string " { " ;
        ( match members with
        | [] -> ()
        | ((name, (expr, _)), _) :: xs ->
            print_string name ;
            print_string " = " ;
            print (indent + 1) expr ;
            helper xs ) ;
        print_string " }"
    | Struct_access ((e, _), member) ->
        print (indent + 1) e ;
        print_char '.' ;
        print_string member
end

module Item = struct
  type func =
    { fname: string
    ; params: (string * Type.t spanned) list
    ; ret_ty: Type.t spanned option
    ; expr: Expr.t spanned }

  type type_kind =
    | Alias of Type.t spanned
    | Struct of (string * Type.t spanned) list

  type type_def = {tname: string; kind: type_kind}

  let print_func self =
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
    print_string "func " ;
    print_string self.fname ;
    print_parameter_list self.params ;
    ( match self.ret_ty with
    | Some (ty, _) -> print_string ": " ; Type.print ty
    | None -> print_char ' ' ) ;
    print_string " =\n" ;
    print_indent 1 ;
    (let expr, _ = self.expr in
     Expr.print 1 expr) ;
    print_string ";\n"


  let print_type_def self =
    print_string "type " ;
    print_string self.tname ;
    print_string " = " ;
    ( match self.kind with
    | Alias (ty, _) -> Type.print ty
    | Struct xs ->
        let rec helper = function
          | (name, (ty, _)) :: xs ->
              print_char '\n' ;
              print_indent 1 ;
              print_string name ;
              print_string ": " ;
              Type.print ty ;
              print_char ';' ;
              helper xs
          | [] -> ()
        in
        print_string "struct {" ; helper xs ; print_string "\n}" ) ;
    print_string ";\n"
end

type t = {funcs: Item.func spanned list; types: Item.type_def spanned list}

let print self =
  let rec print_types = function
    | (x, _) :: xs -> Item.print_type_def x ; print_types xs
    | [] -> ()
  in
  let rec print_funcs = function
    | (x, _) :: xs -> Item.print_func x ; print_funcs xs
    | [] -> ()
  in
  print_types self.types ; print_funcs self.funcs
