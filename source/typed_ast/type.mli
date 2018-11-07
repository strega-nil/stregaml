open! Types.Pervasives

include module type of struct
    include Types.Type
end

module Context : sig
  type t

  val empty : t

  val make :
       Cafec_parse.Ast.Type.Definition.t Cafec_containers.Spanned.t list
    -> t result
end

module Structural = Types.Type_Structural

val structural : t -> ctxt:Context.t -> Structural.t

val equal : t -> t -> bool

val to_string : t -> ctxt:Context.t -> string

val of_untyped : Cafec_parse.Ast.Type.t Spanned.t -> ctxt:Context.t -> t result
