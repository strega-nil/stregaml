module Spanned = Cafec_containers.Spanned

let indent_to_string indent = String.make (indent * 2) ' '

module Type = struct
  type t =
    | Named of string
    | Function of (t Spanned.t list * t Spanned.t option)

  let rec to_string = function
    | Named s -> s
    | Function (parms, ret) ->
        let f (x, _) = to_string x in
        let ret = match ret with Some x -> ") -> " ^ f x | None -> ")" in
        let parms = String.concat ~sep:", " (List.map parms ~f) in
        String.concat ["func("; parms; ret]
end

module Expr = struct
  type t =
    | Unit_literal
    | Bool_literal of bool
    | Integer_literal of int
    | If_else of (t Spanned.t * t Spanned.t * t Spanned.t)
    | Variable of string
    | Call of (t Spanned.t * t Spanned.t list)
    | Struct_literal of (Type.t * (string * t Spanned.t) Spanned.t list)
    | Struct_access of (t Spanned.t * string)

  let rec to_string e ~indent =
    match e with
    | Unit_literal -> "()"
    | Bool_literal true -> "true"
    | Bool_literal false -> "false"
    | Integer_literal n -> Int.to_string n
    | If_else ((cond, _), (thn, _), (els, _)) ->
        let helper x = to_string x ~indent:(indent + 1) in
        String.concat
          [ "if ("
          ; helper cond
          ; ") {\n"
          ; indent_to_string (indent + 1)
          ; helper thn
          ; "\n"
          ; indent_to_string indent
          ; "} else {\n"
          ; indent_to_string (indent + 1)
          ; helper els
          ; "\n"
          ; indent_to_string indent
          ; "}" ]
    | Variable s -> s
    | Call ((e, _), args) ->
        let args =
          let f (x, _) = to_string x ~indent:(indent + 1) in
          String.concat ~sep:", " (List.map args ~f)
        in
        String.concat [to_string ~indent e; "("; args; ")"]
    | Struct_literal (ty, members) ->
        let members =
          let f ((name, (expr, _)), _) =
            String.concat [name; " = "; to_string expr ~indent:(indent + 1)]
          in
          String.concat ~sep:"; " (List.map ~f members)
        in
        String.concat [Type.to_string ty; " { "; members; " }"]
    | Struct_access ((e, _), member) ->
        String.concat [to_string ~indent:(indent + 1) e; "."; member]
end

module Item = struct
  type func =
    { fname: string
    ; params: (string * Type.t Spanned.t) list
    ; ret_ty: Type.t Spanned.t option
    ; expr: Expr.t Spanned.t }

  type type_kind =
    | Alias of Type.t Spanned.t
    | Struct of (string * Type.t Spanned.t) list

  type type_def = {tname: string; kind: type_kind}

  let func_to_string self =
    let parameters =
      let f (name, (ty, _)) = String.concat [name; ": "; Type.to_string ty] in
      String.concat ~sep:", " (List.map ~f self.params)
    in
    let ret_ty =
      match self.ret_ty with
      | Some (ty, _) -> ": " ^ Type.to_string ty
      | None -> ""
    in
    String.concat
      [ "func "
      ; self.fname
      ; "("
      ; parameters
      ; ")"
      ; ret_ty
      ; " =\n"
      ; indent_to_string 1
      ; (let expr, _ = self.expr in
         Expr.to_string ~indent:1 expr)
      ; ";\n" ]


  let type_def_to_string self =
    let rhs =
      match self.kind with
      | Alias (ty, _) -> Type.to_string ty
      | Struct xs ->
          let f (name, (ty, _)) =
            String.concat [name; ": "; Type.to_string ty]
          in
          let sep = ";\n" ^ indent_to_string 1 in
          let members = String.concat ~sep (List.map ~f xs) in
          String.concat ["struct {\n"; members; "\n}"]
    in
    String.concat ["type "; self.tname; " = "; rhs; ";"]
end

type t = {funcs: Item.func Spanned.t list; types: Item.type_def Spanned.t list}

let to_string self =
  let types =
    let f (ty, _) = Item.type_def_to_string ty in
    String.concat ~sep:"\n" (List.map ~f self.types)
  in
  let funcs =
    let f (func, _) = Item.func_to_string func in
    String.concat ~sep:"\n" (List.map ~f self.funcs)
  in
  String.concat [types; "\n\n"; funcs]
