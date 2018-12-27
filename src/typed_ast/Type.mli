open! Types.Pervasives

include module type of struct
    include Types.Type
end

module Context : sig
  type t

  val empty : t

  val make : Cafec_Parse.Type.Definition.t Spanned.t list -> t result
end

module Structural : sig
  include module type of struct
      include Types.Type_Structural
  end

  module Kind = Types.Type_Structural_Kind
end

val structural : value t -> ctxt:Context.t -> Structural.t

val equal : _ t -> _ t -> bool

val to_string : _ t -> ctxt:Context.t -> string

val of_untyped :
  'a Cafec_Parse.Type.t Spanned.t -> ctxt:Context.t -> 'a t result
