module Prelude : sig
  type span =
    {start_line: int; start_column: int; end_line: int; end_column: int}

  type 't spanned = 't * span

  type ('o, 'e) spanned_result = ('o, 'e) Result.t spanned
end

include module type of struct
    include Prelude
end

val made_up : span

val is_made_up : span -> bool

val union : span -> span -> span

val output_span : Stdio.Out_channel.t -> span -> unit

module Monad_implementation : sig
  include Monad.S2 with type ('o, 'e) t := ('o, 'e) spanned_result
end

module Monad : sig
  include module type of Monad_implementation.Let_syntax

  val spanned_bind : ('o, 'e) spanned_result -> ('o spanned, 'e) spanned_result

  val return_err : 'e -> (_, 'e) spanned_result

  val with_span : span -> (unit, _) spanned_result

  val span_of : (_, _) spanned_result -> span
end
