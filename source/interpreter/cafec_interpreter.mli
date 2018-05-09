type t

module Value :
  sig
    type ctxt

    type function_index

    type t =
      | Unit
      | Bool of bool
      | Integer of int
      | Function of function_index
      | Record of (string * t) list
      | Builtin of Cafec_typed_ast.Expr.Builtin.t

    val equal : t -> t -> bool

    val to_string : t -> ctxt -> string
  end
  with type ctxt := t

val make : Cafec_typed_ast.t -> t

val get_function : t -> name:string -> Value.function_index option

val call : t -> Value.function_index -> Value.t list -> Value.t
