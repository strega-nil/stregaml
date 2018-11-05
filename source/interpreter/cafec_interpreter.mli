type t

module Value :
  sig
    include module type of struct
        include Types.Value
    end

    type function_index = Types.Function_index.t

    type ctxt

    val to_string : t -> ctxt -> string
  end
  with type ctxt := t

val make : Cafec_typed_ast.t -> t

val get_function : t -> name:string -> Value.function_index option

val call : t -> Value.function_index -> Value.t list -> Value.t
