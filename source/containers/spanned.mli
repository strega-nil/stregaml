module Span : sig
  type t = {start_line: int; start_column: int; end_line: int; end_column: int}

  val equal : t -> t -> bool

  val union : t -> t -> t

  val made_up : t

  val is_made_up : t -> bool

  val to_string : t -> string
end

type 'a t = 'a * Span.t

val to_string : 'a t -> f:('a -> string) -> string

module Result :
  sig
    type 'a spanned

    type ('o, 'e) t = ('o, 'e) Result.t spanned

    module Monad_implementation : sig
      include Monad.S2 with type ('o, 'e) t := ('o, 'e) t
    end

    module Monad : sig
      include module type of Monad_implementation.Let_syntax

      val spanned_bind : ('o, 'e) t -> ('o spanned, 'e) t

      val return_err : 'e -> (_, 'e) t

      val with_span : Span.t -> (unit, _) t

      val span_of : (_, _) t -> Span.t

      val return_map : 'a list -> f:('a -> ('b, 'e) t) -> ('b list, 'e) t

      val return_fold :
        'a list -> init:'b -> f:('b -> 'a -> ('b, 'e) t) -> ('b, 'e) t

      val return_iteri :
        'a list -> f:(int -> 'a -> (unit, 'e) t) -> (unit, 'e) t
    end
  end
  with type 'a spanned := 'a t
