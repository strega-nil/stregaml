module Parse = Cafec_parse
module Spanned = Cafec_containers.Spanned

(*
  NOTE: we may eventually want to do some sort of caching
  so we don't have to calculate types each time
*)
module Context = struct
  type t = Parse.Ast.Type.Definition.t Spanned.t list

  type index = int

  type error = Duplicate_definitions of string

  let make lst = Result.Ok lst

  let empty = []
end

type t =
  | Unit
  | Bool
  | Int
  | Record of (string * t) list
  | Function of {params: t list; ret_ty: t}
  | User_defined of Context.index

let rec equal l r =
  match (l, r) with
  | Unit, Unit -> true
  | Bool, Bool -> true
  | Int, Int -> true
  | Record m1, Record m2 ->
      let equal (name1, ty1) (name2, ty2) =
        String.equal name1 name2 && equal ty1 ty2
      in
      List.equal m1 m2 ~equal
  | Function f1, Function f2 ->
      equal f1.ret_ty f2.ret_ty && List.equal f1.params f2.params ~equal
  | User_defined u1, User_defined u2 -> u1 = u2
  | _ -> false


let rec to_string ty ~(ctxt: Context.t) =
  match ty with
  | Unit -> "unit"
  | Bool -> "bool"
  | Int -> "int"
  | Record members ->
      let members =
        let f (name, ty) = String.concat [name; ": "; to_string ty ~ctxt] in
        String.concat ~sep:"; " (List.map ~f members)
      in
      String.concat ["< "; members; " >"]
  | Function {params; ret_ty} ->
      let params =
        let f ty = to_string ty ~ctxt in
        String.concat ~sep:", " (List.map ~f params)
      in
      String.concat ["func"; params; " -> "; to_string ret_ty ~ctxt]
  | User_defined idx ->
      let Parse.Ast.Type.Definition.({name; kind}), _ =
        List.nth_exn ctxt idx
      in
      let _name, _kind = (name, kind) in
      failwith "not yet implemented"


type make_error = Type_not_found of string

let rec type_untyped unt_ty ~(ctxt: Context.t) =
  let module U = Parse.Ast.Type in
  let module D = U.Definition in
  let open Result.Let_syntax in
  match unt_ty with
  | U.Named name -> (
      let f ({D.name= name'; _}, _) = String.equal name' name in
      match List.find ~f ctxt with
      | Some ({D.kind= D.Alias ty; _}, _) -> type_untyped ty ~ctxt
      | Some ({D.kind= D.User_defined _; _}, _) ->
          failwith "not yet implemented"
      | None ->
        match name with
        | "unit" -> return Unit
        | "bool" -> return Bool
        | "int" -> return Int
        | name -> Result.Error (Type_not_found name) )
  | U.Record members ->
      let rec map = function
        | [] -> return []
        | ((name, ty), _sp) :: xs ->
            let%bind ty = type_untyped ty ~ctxt in
            let%bind rest = map xs in
            return ((name, ty) :: rest)
      in
      let%bind members = map members in
      return (Record members)
  | U.Function (params, ret_ty) ->
      let rec map = function
        | [] -> return []
        | (ty, _sp) :: xs ->
            let%bind ty = type_untyped ty ~ctxt in
            let%bind rest = map xs in
            return (ty :: rest)
      in
      let%bind params = map params in
      let%bind ret_ty =
        match ret_ty with
        | None -> return Unit
        | Some (ty, _) -> type_untyped ty ~ctxt
      in
      return (Function {params; ret_ty})
