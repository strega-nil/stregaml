module Prelude : sig
  type span =
    {start_line: int; start_column: int; end_line: int; end_column: int}

  type 't spanned = 't * span

  type ('o, 'e) spanned_result = ('o spanned, 'e spanned) result
end

include module type of struct
    include Prelude
end

val made_up : span

val is_made_up : span -> bool

val union : span -> span -> span

val print_span : span -> unit

module Monad (E : Interfaces.Type) : sig
  include Interfaces.Result_monad.Interface
          with type 'o t = ('o, E.t) spanned_result
           and type error = E.t
           and type 'a comonad = 'a spanned

  val with_span : span -> unit t
end
