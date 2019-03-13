include module type of struct include Types.Token end

module Keyword : sig
  include module type of struct include Types.Token_Keyword end

  module Contextual = Types.Token_Keyword_Contextual

  val equal : t -> t -> bool

  val to_string : t -> lang:(module Types.Language) -> string
end

module Attribute : sig
  include module type of struct include Types.Token_Attribute end

  val equal : t -> t -> bool

  val to_string : t -> lang:(module Types.Language) -> string
end

val equal : t -> t -> bool

val to_string : t -> lang:(module Types.Language) -> string
