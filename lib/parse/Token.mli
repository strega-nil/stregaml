include module type of struct include Types.Token end

module Keyword : sig
  include module type of struct include Types.Token_Keyword end

  val equal : t -> t -> bool

  val to_string : t -> lang:(module Types.Language) -> string
end

val equal : t -> t -> bool

val to_string : t -> lang:(module Types.Language) -> string
