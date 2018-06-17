module Context : sig
  type t

  type index

  type error = Duplicate_definitions of string

  val empty : t

  val make :
    Cafec_parse.Ast.Type.Definition.t Cafec_containers.Spanned.t list
    -> (t, error) Result.t
end

type builtin = Unit | Bool | Int | Function of {params: t list; ret_ty: t}

and t = Builtin of builtin | User_defined of Context.index

module Structural : sig
  type nonrec t = Builtin of builtin | Record of (string * t) list
end

type make_error = Type_not_found of string

val structural : t -> ctxt:Context.t -> Structural.t

val equal : t -> t -> bool

val to_string : t -> ctxt:Context.t -> string

val type_untyped :
  Cafec_parse.Ast.Type.t -> ctxt:Context.t -> (t, make_error) Result.t
