module Error = Error
module Expr = Expr
module Type = Type
module Internal = Internal

module Function = struct
  type t = Internal.func_decl =
    {name: string; params: (string * Type.t) list; ret_ty: Type.t}
end

(*
type type_kind = Internal.type_kind = Type_alias of Type.t

type type_def = Internal.type_def = {tname: string; kind: type_kind}
*)

type t = {number_of_functions: int; ast: Internal.t}

let make unt_ast =
  match Internal.make unt_ast with
  | Ok ast, sp ->
      (*let number_of_types = List.length ast.Internal.type_defs in*)
      let number_of_functions = List.length ast.Internal.func_decls in
      (Ok {number_of_functions; ast}, sp)
  | Error e, sp -> (Error e, sp)


(*
let number_of_types {number_of_types; _} = number_of_types

let type_seq ast =
  let rec helper types () =
    match types with
    | [] -> Seq.Nil
    | ty :: types -> Seq.Cons (ty, helper types)
  in
  let {ast= {Internal.type_defs; _}; _} = ast in
  helper type_defs
*)

let number_of_functions {number_of_functions; _} = number_of_functions

let function_seq ast =
  let {ast= {Internal.func_decls; Internal.func_defs; _}; _} = ast in
  let init = (func_decls, func_defs) in
  let f = function
    | [], [] -> None
    | decl :: decls, def :: defs -> Some ((decl, def), (decls, defs))
    | _ -> assert false
  in
  Sequence.unfold ~init ~f
