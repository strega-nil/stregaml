module Spanned = Cafec_containers.Spanned
open Spanned.Prelude
module Out = Stdio.Out_channel

let output_indent f indent =
  let rec helper = function
    | n when n <= 0 -> ()
    | n ->
        Out.output_string f "  " ;
        helper (n - 1)
  in
  if indent < 0 then raise (Invalid_argument "Parse.Ast.output_indent")
  else helper indent


module Type = struct
  type t = Named of string | Function of (t spanned list * t spanned option)

  let rec output f = function
    | Named s -> Out.output_string f s
    | Function (parms, ret) ->
        let rec output_list f = function
          | (x, _) :: xs ->
              Out.output_string f ", " ; output f x ; output_list f xs
          | [] -> ()
        in
        Out.output_string f "func(" ;
        ( match parms with
        | (x, _) :: xs -> output f x ; output_list f xs
        | [] -> () ) ;
        Out.output_string f ")" ;
        match ret with
        | Some (ret, _) -> Out.output_string f " -> " ; output f ret
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

  let rec output f indent e =
    let rec output_args f ?start = function
      | [] -> ()
      | (arg, _) :: args ->
          (match start with Some () -> Out.output_string f ", " | None -> ()) ;
          output f (indent + 1) arg ;
          output_args f ~start:() args
    in
    match e with
    | Unit_literal -> Out.output_string f "()"
    | Bool_literal true -> Out.output_string f "true"
    | Bool_literal false -> Out.output_string f "false"
    | Integer_literal n -> Out.fprintf f "%d" n
    | If_else ((cond, _), (thn, _), (els, _)) ->
        let output_inner f = output f (indent + 1) in
        Out.output_string f "if (" ;
        output_inner f cond ;
        Out.output_string f ") {\n" ;
        output_indent f (indent + 1) ;
        output_inner f thn ;
        Out.output_char f '\n' ;
        output_indent f indent ;
        Out.output_string f "} else {\n" ;
        output_indent f (indent + 1) ;
        output_inner f els ;
        Out.output_char f '\n' ;
        output_indent f indent ;
        Out.output_char f '}'
    | Variable s -> Out.output_string f s
    | Call ((e, _), args) ->
        output f indent e ;
        Out.output_char f '(' ;
        output_args f args ;
        Out.output_char f ')'
    | Struct_literal (ty, members) ->
        let rec helper = function
          | ((name, (expr, _)), _) :: xs ->
              Out.output_string f "; " ;
              Out.output_string f name ;
              Out.output_string f " = " ;
              output f (indent + 1) expr ;
              helper xs
          | [] -> ()
        in
        Type.output f ty ;
        Out.output_string f " { " ;
        ( match members with
        | [] -> ()
        | ((name, (expr, _)), _) :: xs ->
            Out.output_string f name ;
            Out.output_string f " = " ;
            output f (indent + 1) expr ;
            helper xs ) ;
        Out.output_string f " }"
    | Struct_access ((e, _), member) ->
        output f (indent + 1) e ;
        Out.output_char f '.' ;
        Out.output_string f member
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

  let output_func f self =
    let output_parameter_list f lst =
      let rec helper ?start = function
        | (name, (ty, _)) :: xs ->
            ( match start with
            | Some () -> Out.output_string f ", "
            | None -> () ) ;
            Out.output_string f name ;
            Out.output_string f ": " ;
            Type.output f ty ;
            helper ~start:() xs
        | [] -> ()
      in
      Out.output_char f '(' ; helper lst ; Out.output_char f ')'
    in
    Out.output_string f "func " ;
    Out.output_string f self.fname ;
    output_parameter_list f self.params ;
    ( match self.ret_ty with
    | Some (ty, _) -> Out.output_string f ": " ; Type.output f ty
    | None -> Out.output_char f ' ' ) ;
    Out.output_string f " =\n" ;
    output_indent f 1 ;
    (let expr, _ = self.expr in
     Expr.output f 1 expr) ;
    Out.output_string f ";\n"


  let output_type_def f self =
    Out.output_string f "type " ;
    Out.output_string f self.tname ;
    Out.output_string f " = " ;
    ( match self.kind with
    | Alias (ty, _) -> Type.output f ty
    | Struct xs ->
        let rec helper = function
          | (name, (ty, _)) :: xs ->
              Out.output_char f '\n' ;
              output_indent f 1 ;
              Out.output_string f name ;
              Out.output_string f ": " ;
              Type.output f ty ;
              Out.output_char f ';' ;
              helper xs
          | [] -> ()
        in
        Out.output_string f "struct {" ;
        helper xs ;
        Out.output_string f "\n}" ) ;
    Out.output_string f ";\n"
end

type t = {funcs: Item.func spanned list; types: Item.type_def spanned list}

let output f self =
  let rec output_types f = function
    | (x, _) :: xs -> Item.output_type_def f x ; output_types f xs
    | [] -> ()
  in
  let rec output_funcs f = function
    | (x, _) :: xs -> Item.output_func f x ; output_funcs f xs
    | [] -> ()
  in
  output_types f self.types ; output_funcs f self.funcs
