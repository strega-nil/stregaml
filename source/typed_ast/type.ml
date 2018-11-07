open! Types.Pervasives
module Parse = Cafec_parse
include Types.Type
module Structural = Types.Type_Structural

module Context = struct
  type user_defined_type = {data: Structural.t}

  (* note: const after construction *)
  type nonrec t =
    {user_defined: user_defined_type array; names: (string Spanned.t * t) array}

  let make lst =
    let module PType = Parse.Ast.Type in
    let defs, defs_len, aliases, aliases_len =
      let module Def = PType.Definition in
      let rec helper defs defs_len aliases aliases_len = function
        | ({Def.name; Def.kind= Def.Alias ty}, _) :: rest ->
            helper defs defs_len ((name, ty) :: aliases) (aliases_len + 1) rest
        | ({Def.name; Def.kind= Def.User_defined {data}}, _) :: rest ->
            helper ((name, data) :: defs) (defs_len + 1) aliases aliases_len
              rest
        | [] -> (defs, defs_len, aliases, aliases_len)
      in
      helper [] 0 [] 0 lst
    in
    let rec get_ast_type pty =
      (* returns -1 if not found *)
      let rec find_index name index = function
        | [] -> -1
        | ((name', _), _) :: _ when String.equal name name' -> index
        | _ :: rest -> find_index name (index + 1) rest
      in
      let rec find_alias name = function
        | [] -> return None
        | ((name', _), alias) :: _ when String.equal name name' ->
            let%bind ty = get_ast_type alias in
            return (Some ty)
        | _ :: rest -> find_alias name rest
      in
      match pty with
      | PType.Named name -> (
        match find_index name 0 defs with
        | -1 -> (
            let%bind ty = find_alias name aliases in
            match ty with
            | Some ty -> return ty
            | None -> (
              match name with
              | "Unit" -> return (Builtin Unit)
              | "Bool" -> return (Builtin Bool)
              | "Int32" -> return (Builtin Int32)
              | _ -> return_err (Error.Type_not_found name) ) )
        | n -> return (User_defined n) )
      | PType.Function (params, ret_ty) ->
          let rec map_params = function
            | [] -> return []
            | (x, _) :: xs ->
                let%bind first = get_ast_type x in
                let%bind rest = map_params xs in
                return (first :: rest)
          in
          let%bind params = map_params params in
          let%bind ret_ty =
            match ret_ty with
            | Some (ty, _) -> get_ast_type ty
            | None -> return (Builtin Unit)
          in
          return (Builtin (Function {params; ret_ty}))
    in
    let names_len = defs_len + aliases_len in
    let user_defined =
      let default = {data= Structural.Record []} in
      Array.create ~len:defs_len default
    in
    let names =
      let default = (("", Spanned.Span.made_up), User_defined (-1)) in
      Array.create ~len:names_len default
    in
    let rec exists_duplicate idx end_idx name =
      if idx = end_idx then false
      else
        let (name_idx, _), _ = names.(idx) in
        if String.equal name_idx name then true
        else exists_duplicate (idx + 1) end_idx name
    in
    let rec fill_defs index = function
      | [] ->
          assert (index = defs_len) ;
          return ()
      | ((name, name_sp), def) :: defs ->
          let module Data = PType.Data in
          if exists_duplicate 0 index name then
            return_err (Error.Type_defined_multiple_times name)
          else
            let%bind data =
              match def with Data.Record lst ->
                let rec helper = function
                  | [] -> return []
                  | ((name, ty), _) :: xs ->
                      let%bind first = get_ast_type ty in
                      let%bind rest = helper xs in
                      return ((name, first) :: rest)
                in
                let%bind members = helper lst in
                return (Structural.Record members)
            in
            user_defined.(index) <- {data} ;
            names.(index) <- ((name, name_sp), User_defined index) ;
            fill_defs (index + 1) defs
    in
    let%bind () = fill_defs 0 defs in
    let rec fill_aliases index = function
      | [] ->
          assert (index = names_len) ;
          return ()
      | (name, ty) :: xs ->
          let%bind ty = get_ast_type ty in
          names.(index) <- (name, ty) ;
          fill_aliases (index + 1) xs
    in
    let%bind () = fill_aliases defs_len aliases in
    return {user_defined; names}

  let empty = {user_defined= [||]; names= [||]}
end

let rec of_untyped (unt_ty : Parse.Ast.Type.t Spanned.t) ~(ctxt : Context.t) :
    t result =
  let module U = Parse.Ast.Type in
  let module D = U.Definition in
  let unt_ty, sp = unt_ty in
  let%bind _ = with_span sp in
  match unt_ty with
  | U.Named name -> (
      let f ((name', _), _) = String.equal name' name in
      match Array.find ~f ctxt.Context.names with
      | Some (_, ty) -> return ty
      | None -> (
        match name with
        | "Unit" -> return (Builtin Unit)
        | "Bool" -> return (Builtin Bool)
        | "Int32" -> return (Builtin Int32)
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
  | Builtin Int32, Builtin Int32 -> true
  | Builtin (Function f1), Builtin (Function f2) ->
      equal f1.ret_ty f2.ret_ty && List.equal f1.params f2.params ~equal
  | User_defined u1, User_defined u2 -> u1 = u2
  | _ -> false

let structural ty ~(ctxt : Context.t) =
  match ty with
  | Builtin b -> Structural.Builtin b
  | User_defined idx -> Context.(ctxt.user_defined.(idx).data)

let rec to_string ty ~(ctxt : Context.t) =
  match ty with
  | Builtin Unit -> "Unit"
  | Builtin Bool -> "Bool"
  | Builtin Int32 -> "Int32"
  | Builtin (Function {params; ret_ty}) ->
      let params =
        let f ty = to_string ty ~ctxt in
        String.concat ~sep:", " (List.map ~f params)
      in
      String.concat ["func"; params; " -> "; to_string ret_ty ~ctxt]
  | User_defined idx ->
      let (name, _), _ = ctxt.Context.names.(idx) in
      name
