module Error = Error
module Expr = Expr
module Internal = Internal

type func_type = {params: (string * Type.t) list; ret_ty: Type.t}

type func = {name: string; decl: func_type; expr: Expr.t}

type t = {funcs: func list}

let convert internal =
  let module I = Internal in
  let rec helper = function
    | ({I.name; I.params; I.ret_ty}, _) :: decls, (expr, _) :: exprs ->
        let decl = {params; ret_ty} in
        {name; decl; expr} :: helper (decls, exprs)
    | [], [] -> []
    | _ -> assert false
  in
  let funcs = helper (internal.I.func_decls, internal.I.func_exprs) in
  {funcs}


let make ast =
  match Internal.make ast with
  | Ok (o, sp) -> Ok (convert o, sp)
  | Error e -> Error e


let function_by_index ctxt idx =
  let rec helper = function
    | 0, func :: _ -> func
    | n, _ :: funcs -> helper (n - 1, funcs)
    | _, [] -> assert false
  in
  if idx < 0 then assert false ;
  helper (idx, ctxt.funcs)


let function_index_by_name ctxt name =
  let rec helper find n = function
    | {name; _} :: _ when name = find -> Some n
    | _ :: rest -> helper find (n + 1) rest
    | [] -> None
  in
  helper name 0 ctxt.funcs
