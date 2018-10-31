open! Types.Pervasives
module Parse = Cafec_parse
include Types.Type

(*
  NOTE: we may eventually want to do some sort of caching
  so we don't have to calculate types each time
*)
module Context = struct
  include Types.Type_context

  let make lst = return (of_underlying lst)

  let empty = of_underlying []
end

module Structural = Types.Type_structural

let rec of_untyped (unt_ty : Parse.Ast.Type.t Spanned.t) ~(ctxt : Context.t) :
    t result =
  let module U = Parse.Ast.Type in
  let module D = U.Definition in
  let unt_ty, sp = unt_ty in
  let%bind _ = with_span sp in
  match unt_ty with
  | U.Named name -> (
      let f _ ({D.name= name'; _}, _sp) = String.equal name' name in
      match List.findi ~f (Context.to_underlying ctxt) with
      | Some (_, ({D.kind= D.Alias ty; _}, _sp)) -> of_untyped (ty, sp) ~ctxt
      | Some (idx, ({D.kind= D.User_defined _; _}, _sp)) ->
          return (User_defined (Context.index_of_underlying idx))
      | None -> (
        match name with
        | "unit" -> return (Builtin Unit)
        | "bool" -> return (Builtin Bool)
        | "int" -> return (Builtin Int)
        | name -> return_err (Error.Type_not_found name) ) )
  | U.Function (params, ret_ty) ->
      let rec map = function
        | [] -> return []
        | ty :: xs ->
            let%bind ty = of_untyped ty ~ctxt in
            let%bind rest = map xs in
            return (ty :: rest)
      in
      let%bind params = map params in
      let%bind ret_ty =
        match ret_ty with
        | None -> return (Builtin Unit)
        | Some ty -> of_untyped ty ~ctxt
      in
      return (Builtin (Function {params; ret_ty}))

let rec equal l r =
  match (l, r) with
  | Builtin Unit, Builtin Unit -> true
  | Builtin Bool, Builtin Bool -> true
  | Builtin Int, Builtin Int -> true
  | Builtin (Function f1), Builtin (Function f2) ->
      equal f1.ret_ty f2.ret_ty && List.equal f1.params f2.params ~equal
  | User_defined u1, User_defined u2 ->
      Context.index_to_underlying u1 = Context.index_to_underlying u2
  | _ -> false

let structural ty ~(ctxt : Context.t) =
  match ty with
  | Builtin b -> Structural.Builtin b
  | User_defined idx -> (
      let module U = Parse.Ast.Type in
      let ctxt' = Context.to_underlying ctxt in
      let idx' = Context.index_to_underlying idx in
      let U.Definition.({kind; _}), _ = List.nth_exn ctxt' idx' in
      match kind with
      | U.Definition.Alias _ -> assert false
      | U.Definition.User_defined {data} -> (
        match data with U.Data.Record members ->
          let f ((name, ty), sp) =
            match of_untyped (ty, sp) ~ctxt with
            | Result.Ok o, _ -> (name, o)
            | Result.Error _, _ -> failwith "this should fail somewhere else"
          in
          Structural.Record (List.map ~f members) ) )

let rec to_string ty ~(ctxt : Context.t) =
  match ty with
  | Builtin Unit -> "unit"
  | Builtin Bool -> "bool"
  | Builtin Int -> "int"
  | Builtin (Function {params; ret_ty}) ->
      let params =
        let f ty = to_string ty ~ctxt in
        String.concat ~sep:", " (List.map ~f params)
      in
      String.concat ["func"; params; " -> "; to_string ret_ty ~ctxt]
  | User_defined idx ->
      let module U = Parse.Ast.Type in
      let ctxt' = Context.to_underlying ctxt in
      let idx' = Context.index_to_underlying idx in
      let U.Definition.({name; _}), _ = List.nth_exn ctxt' idx' in
      name
