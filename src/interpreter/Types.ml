module rec Value : sig
  type t =
    | Unit : t
    | Bool : bool -> t
    | Integer : int -> t
    | Function : Function_index.t -> t
    | Reference : Expr_result.Place.t -> t
    | Constructor : int -> t
    | Variant : int * t ref -> t
    | Record : t ref array -> t
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
  module Place : sig
    type t = Place : {is_mut: bool; value: Value.t ref} -> t
  end

  type t = Value : Value.t -> t | Place : Place.t -> t
end =
  Expr_result
