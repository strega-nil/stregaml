module rec Immediate : sig
  type t =
    | Unit
    | Bool of bool
    | Integer of int
    | Function of Function_index.t
    | Record of (string * t ref) list
    | Builtin of Cafec_typed_ast.Expr.Builtin.t
end =
  Immediate

and Function_index : sig
  type t = private int

  val of_int : int -> t
end = struct
  type t = int

  let of_int x = x
end

and Value : sig
  type obj_header =
    { is_mut : bool
    ; mutable in_scope: bool }

  type obj =
    { header: obj_header
    ; immediate: Immediate.t ref }

  type t = Immediate of Immediate.t | Object of obj
end =
  Value
