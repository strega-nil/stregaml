module Context : sig
  type t

  type index

  type error = Duplicate_definitions of string

  val empty : t

  val make :
    Cafec_parse.Ast.Type.Definition.t Cafec_containers.Spanned.t list
    -> (t, error) Result.t
end

type t =
  | Unit
  | Bool
  | Int
  | Record of (string * t) list
  | Function of {params: t list; ret_ty: t}
  | User_defined of Context.index

type make_error = Type_not_found of string

val structural : t -> ctxt:Context.t -> t

val equal : t -> t -> bool

val to_string : t -> ctxt:Context.t -> string

val type_untyped :
  Cafec_parse.Ast.Type.t -> ctxt:Context.t -> (t, make_error) Result.t
