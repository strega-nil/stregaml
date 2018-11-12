module rec Value : sig
  type t =
    | Unit
    | Bool of bool
    | Integer of int
    | Function of Function_index.t
    | Reference of Expr_result.place
    | Record of (Ident.t * t ref) list
    | Builtin of Cafec_typed_ast.Expr.Builtin.t
end =
  Value

and Function_index : sig
  type t = private int

  val of_int : int -> t
end = struct
  type t = int

  let of_int x = x
end

and Expr_result : sig
  type object_header = {mutable in_scope: bool}

  type place = {header: object_header; is_mut: bool; value: Value.t ref}

  type t = Value of Value.t | Place of place
end =
  Expr_result
