type t

module Value :
  sig
    include module type of struct include Types.Value end

    type function_index = Types.Function_index.t

    type ctxt

    val to_string :
      t -> ctxt -> lang:(module Cafec_Parse.Language) -> string
  end
  with type ctxt := t

val make : Cafec_Typed_ast.t -> t

val entrypoint : t -> Value.function_index option

val get_function : t -> name:_ Name.t -> Value.function_index option

val call : t -> Value.function_index -> Value.t list -> Value.t
