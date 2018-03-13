module Error = Error
module Expr = Expr
module Internal = Internal

type func_decl = Internal.func_decl =
  {fname: string; params: (string * Type.t) list; ret_ty: Type.t}

(*
type type_kind = Internal.type_kind = Type_alias of Type.t

type type_def = Internal.type_def = {tname: string; kind: type_kind}
*)

type t = Internal.t

let make = Internal.make

let functions ast =
  let rec helper decls defs =
    match (decls, defs) with
    | [], [] -> []
    | decl :: decls, def :: defs -> (decl, def) :: helper decls defs
    | _ -> assert false
  in
  let {Internal.func_decls; Internal.func_defs; _} = ast in
  helper func_decls func_defs
