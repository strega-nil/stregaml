include module type of struct
    include Types.Type
end

val mutability_equal : mutability -> mutability -> bool

val mutability_to_string : mutability -> string

val to_string : _ t -> string

module Data : sig
  include module type of struct
      include Types.Type_Data
  end

  val to_string : ?name:string -> t -> string
end

module Definition = Types.Type_Definition
