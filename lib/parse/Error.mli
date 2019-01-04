module Expected : sig
  include module type of struct include Types.Error_Expected end

  val to_string : t -> string
end

include module type of struct include Types.Error end

val to_string : t -> string
