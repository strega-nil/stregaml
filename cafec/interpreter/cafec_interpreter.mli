type t

module Value : sig
  type ctxt = t
  type function_index

  type t =
    | Unit
    | Bool of bool
    | Integer of int
    | Function of function_index
    | Struct of t array
    | Builtin of Cafec_typed_ast.Expr.builtin

  val output : Stdio.Out_channel.t -> t -> ctxt -> unit
end

val make : Cafec_typed_ast.t -> t

val get_function : t -> name:string -> Value.function_index option

val call : t -> Value.function_index -> Value.t list -> Value.t
