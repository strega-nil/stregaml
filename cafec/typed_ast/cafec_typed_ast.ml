module Error = Error
module Expr = Expr
module Internal = Internal

type func_decl = Internal.decl =
  {name: string; params: (string * Type.t) list; ret_ty: Type.t}

type t = {number_of_functions: int; ast: Internal.t}

let make unt_ast =
  match Internal.make unt_ast with
  | Ok (ast, sp) ->
      let number_of_functions = List.length ast.Internal.func_decls in
      Ok ({number_of_functions; ast}, sp)
  | Error e -> Error e

let function_seq ast =
  let rec helper decls exprs () =
    match (decls, exprs) with
    | [], [] -> Seq.Nil
    | decl :: decls, expr :: exprs ->
        Seq.Cons ((decl, expr), helper decls exprs)
    | _ -> assert false
  in
  let {ast= {Internal.func_decls; Internal.func_exprs}; _} = ast in
  helper func_decls func_exprs

let number_of_functions {number_of_functions; _} = number_of_functions
