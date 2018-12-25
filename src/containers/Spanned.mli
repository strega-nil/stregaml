module Span : sig
  type t =
    | Span :
        { start_line: int
        ; start_column: int
        ; end_line: int
        ; end_column: int }
        -> t

  val start_line : t -> int

  val start_column : t -> int

  val end_line : t -> int

  val end_column : t -> int

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

      val spanned_lift : 'o spanned -> ('o, 'e) t

      val return_err : 'e -> (_, 'e) t

      val with_span : Span.t -> (unit, _) t

      val span_of : (_, _) t -> Span.t

      module Return : sig
        module List : sig
          val map : 'a list -> f:('a -> ('b, 'e) t) -> ('b list, 'e) t

          val fold :
            'a list -> init:'b -> f:('b -> 'a -> ('b, 'e) t) -> ('b, 'e) t

          val iter : 'a list -> f:('a -> (unit, 'e) t) -> (unit, 'e) t

          val iteri : 'a list -> f:(int -> 'a -> (unit, 'e) t) -> (unit, 'e) t
        end

        module Array : sig
          val of_sequence :
            len:int -> ('a, 'e) t Sequence.t -> ('a Array.t, 'e) t

          val of_sequence_unordered :
               len:int
            -> (int * 'a, 'e) t Sequence.t
            -> (('a Array.t, 'e) t, Array.unordered_error) Result.t

          val of_list_map :
            'a list -> f:('a -> ('b, 'e) t) -> ('b Array.t, 'e) t

          val of_list_map_unordered :
               'a list
            -> f:('a -> (int * 'b, 'e) t)
            -> (('b Array.t, 'e) t, Array.unordered_error) Result.t
        end
      end
    end
  end
  with type 'a spanned := 'a t
