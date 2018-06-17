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

type builtin = Unit | Bool | Int | Function of {params: t list; ret_ty: t}

and t =
  | Builtin of builtin
  (* note: this will never be an alias, only a type declaration *)
  | User_defined of Context.index

module Structural = struct
  type nonrec t = Builtin of builtin | Record of (string * t) list
end

type make_error = Type_not_found of string

let rec type_untyped unt_ty ~(ctxt: Context.t) =
  let module U = Parse.Ast.Type in
  let module D = U.Definition in
  let open Result.Let_syntax in
  match unt_ty with
  | U.Named name -> (
      let f _ ({D.name= name'; _}, _sp) = String.equal name' name in
      match List.findi ~f ctxt with
      | Some (_, ({D.kind= D.Alias ty; _}, _sp)) -> type_untyped ty ~ctxt
      | Some (idx, ({D.kind= D.User_defined _; _}, _sp)) ->
          Result.Ok (User_defined idx)
      | None ->
        match name with
        | "unit" -> return (Builtin Unit)
        | "bool" -> return (Builtin Bool)
        | "int" -> return (Builtin Int)
        | name -> Result.Error (Type_not_found name) )
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
        | None -> return (Builtin Unit)
        | Some (ty, _) -> type_untyped ty ~ctxt
      in
      return (Builtin (Function {params; ret_ty}))


let rec equal l r =
  match (l, r) with
  | Builtin Unit, Builtin Unit -> true
  | Builtin Bool, Builtin Bool -> true
  | Builtin Int, Builtin Int -> true
  | Builtin Function f1, Builtin Function f2 ->
      equal f1.ret_ty f2.ret_ty && List.equal f1.params f2.params ~equal
  | User_defined u1, User_defined u2 -> u1 = u2
  | _ -> false


let rec structural ty ~(ctxt: Context.t) =
  match ty with
  | Builtin b -> Structural.Builtin b
  | User_defined idx ->
      let module U = Parse.Ast.Type in
      let U.Definition.({kind; _}), _ = List.nth_exn ctxt idx in
      match kind with
      | U.Definition.Alias _ -> assert false
      | U.Definition.User_defined _ -> failwith "unimplemented"


let rec to_string ty ~(ctxt: Context.t) =
  match ty with
  | Builtin Unit -> "unit"
  | Builtin Bool -> "bool"
  | Builtin Int -> "int"
  | Builtin Function {params; ret_ty} ->
      let params =
        let f ty = to_string ty ~ctxt in
        String.concat ~sep:", " (List.map ~f params)
      in
      String.concat ["func"; params; " -> "; to_string ret_ty ~ctxt]
  | User_defined idx ->
      let module U = Parse.Ast.Type in
      let U.Definition.({name; _}), _ = List.nth_exn ctxt idx in
      name
