include module type of struct include Types.Token end

module Keyword : sig
  include module type of struct include Types.Token_Keyword end

  module Contextual : sig
    include module type of struct
        include Types.Token_Keyword_Contextual
    end

    val equal : t -> t -> bool

    val to_string : t -> lang:(module Types.Language) -> string
  end

  val equal : t -> t -> bool

  val to_string : t -> lang:(module Types.Language) -> string
end

module Attribute : sig
  include module type of struct include Types.Token_Attribute end

  val equal : t -> t -> bool

  val to_string : t -> lang:(module Types.Language) -> string
end

module Builtin_name : sig
  include module type of struct include Types.Token_Builtin_name end

  val equal : t -> t -> bool

  val to_string : t -> lang:(module Types.Language) -> string
end

val equal : t -> t -> bool

val to_string : t -> lang:(module Types.Language) -> string
