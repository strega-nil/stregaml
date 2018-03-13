module Prelude = struct
  type span =
    {start_line: int; start_column: int; end_line: int; end_column: int}

  type 't spanned = 't * span

  type ('o, 'e) spanned_result = ('o, 'e) Result.t spanned
end

include Prelude

let made_up = {start_line= 0; start_column= 0; end_line= 0; end_column= 0}

let is_made_up sp = Polymorphic_compare.equal sp made_up

let union fst snd =
  if is_made_up fst then snd
  else if is_made_up snd then fst
  else
    { start_line= fst.start_line
    ; start_column= fst.start_column
    ; end_line= snd.end_line
    ; end_column= snd.end_column }


let output_span f {start_line; start_column; end_line; end_column} =
  Stdio.Out_channel.fprintf f "(%d, %d) to (%d, %d)" start_line start_column
    end_line end_column


module Monad_implementation = struct
  include Monad.Make2 (struct
    type ('o, 'e) t = ('o, 'e) spanned_result

    let bind (self, sp) ~f =
      match self with
      | Error e -> (Error e, sp)
      | Ok o ->
        match f o with
        | Ok o', sp' -> (Ok o', union sp sp')
        | Error e', sp' -> (Error e', if is_made_up sp' then sp else sp')


    let map = `Define_using_bind

    let return o = (Ok o, made_up)
  end)
end

module Monad = struct
  include Monad_implementation.Let_syntax

  let spanned_bind (t, sp) =
    match t with Error e -> (Error e, sp) | Ok o -> (Ok (o, sp), sp)


  let return_err e = (Error e, made_up)

  let with_span sp = (Ok (), sp)

  let span_of (_, sp) = sp
end
