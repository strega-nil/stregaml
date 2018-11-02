module Keyword : sig
  include module type of struct
      include Types.Token_keyword
  end

  val equal : t -> t -> bool

  val to_string : t -> string
end

include module type of struct
    include Types.Token
end

val equal : t -> t -> bool

val to_string : t -> string
