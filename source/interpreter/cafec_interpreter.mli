type t

module Immediate :
  sig
    include module type of struct
        include Types.Immediate
    end

    type function_index = Types.Function_index.t

    type ctxt

    val to_string : t -> ctxt -> string
  end
  with type ctxt := t

val make : Cafec_typed_ast.t -> t

val get_function : t -> name:string -> Immediate.function_index option

val call : t -> Immediate.function_index -> Immediate.t list -> Immediate.t
