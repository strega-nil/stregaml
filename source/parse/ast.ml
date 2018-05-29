module Spanned = Cafec_containers.Spanned

let indent_to_string indent = String.make (indent * 2) ' '

module Type = struct
  type t =
    | Named of string
    | Record of (string * t) Spanned.t list
    | Function of t Spanned.t list * t Spanned.t option

  let rec to_string = function
    | Named s -> s
    | Record members ->
        let f ((name, ty), _) = String.concat [name; ": "; to_string ty] in
        let members = String.concat ~sep:"; " (List.map members ~f) in
        String.concat ["{| "; members; " |}"]
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
    | If_else of t Spanned.t * t Spanned.t * t Spanned.t
    | Variable of {path: string list; name: string}
    | Call of t Spanned.t * t Spanned.t list
    | Record_literal of
        { path: string list
        ; members: (string * t Spanned.t) Spanned.t list }
    | Record_access of t Spanned.t * string

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
    | Variable {path; name} -> String.concat ~sep:"::" (path @ [name])
    | Call ((e, _), args) ->
        let args =
          let f (x, _) = to_string x ~indent:(indent + 1) in
          String.concat ~sep:", " (List.map args ~f)
        in
        String.concat [to_string ~indent e; "("; args; ")"]
    | Record_literal {path; members} ->
        let members =
          let f ((name, (expr, _)), _) =
            String.concat [name; " = "; to_string expr ~indent:(indent + 1)]
          in
          String.concat ~sep:"; " (List.map ~f members)
        in
        if not (List.is_empty path) then
          let path = String.concat ~sep:"::" path in
          String.concat [path; "::"; "{| "; members; " |}"]
        else String.concat ["{| "; members; " |}"]
    | Record_access ((e, _), member) ->
        String.concat [to_string ~indent:(indent + 1) e; "."; member]
end

module Func = struct
  type t =
    { name: string
    ; params: (string * Type.t) Spanned.t list
    ; ret_ty: Type.t Spanned.t option
    ; expr: Expr.t Spanned.t }

  let to_string self =
    let parameters =
      let f ((name, ty), _) = String.concat [name; ": "; Type.to_string ty] in
      String.concat ~sep:", " (List.map ~f self.params)
    in
    let ret_ty =
      match self.ret_ty with
      | Some (ty, _) -> " -> " ^ Type.to_string ty
      | None -> ""
    in
    String.concat
      [ "func "
      ; self.name
      ; "("
      ; parameters
      ; ")"
      ; ret_ty
      ; " {\n"
      ; indent_to_string 1
      ; (let expr, _ = self.expr in
         Expr.to_string ~indent:1 expr)
      ; "\n}\n" ]
end

module Type_definition = struct
  type t = {name: string; data: Type.t}

  let to_string {name; data} =
    String.concat
      ["type "; name; " {\n"; "  data = "; Type.to_string data; ";\n}"]
end

type t =
  { funcs: Func.t Spanned.t list
  ; aliases: (string * Type.t) Spanned.t list
  ; types: Type_definition.t Spanned.t list }

let to_string self =
  let aliases =
    let f ((name, def), _) =
      String.concat ["alias "; name; " = "; Type.to_string def; ";"]
    in
    String.concat ~sep:"\n" (List.map ~f self.aliases)
  in
  let types =
    let f (ty, _) = Type_definition.to_string ty in
    String.concat ~sep:"\n" (List.map ~f self.types)
  in
  let funcs =
    let f (func, _) = Func.to_string func in
    String.concat ~sep:"\n" (List.map ~f self.funcs)
  in
  String.concat [aliases; "\n\n"; types; "\n\n"; funcs]
