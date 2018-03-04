module Error = Error
module Expr = Expr
module Internal = Internal

type func_decl = Internal.decl =
  {name: string; params: (string * Type.t) list; ret_ty: Type.t}

type t = Internal.t

let make = Internal.make

let function_seq ast =
  let rec helper decls exprs () =
    match (decls, exprs) with
    | [], [] -> Seq.Nil
    | decl :: decls, expr :: exprs ->
        Seq.Cons ((decl, expr), helper decls exprs)
    | _ -> assert false
  in
  let {Internal.func_decls; Internal.func_exprs} = ast in
  helper func_decls func_exprs
