module Prelude = struct
  type span =
    {start_line: int; start_column: int; end_line: int; end_column: int}

  type 't spanned = 't * span

  type ('o, 'e) spanned_result = ('o spanned, 'e spanned) result
end

include Prelude

let made_up = {start_line= 0; start_column= 0; end_line= 0; end_column= 0}

let is_made_up sp = sp = made_up

let union fst snd =
  if is_made_up fst then snd
  else if is_made_up snd then fst
  else
    { start_line= fst.start_line
    ; start_column= fst.start_column
    ; end_line= snd.end_line
    ; end_column= snd.end_column }


let print_span {start_line; start_column; end_line; end_column} =
  Printf.printf "(%d, %d) to (%d, %d)" start_line start_column end_line
    end_column


module Monad (E : Interfaces.Type) = struct
  include Interfaces.Result_monad.Make (struct
    type error = E.t

    type 'o t = ('o, E.t) spanned_result
    type 'a comonad = 'a spanned

    let ( >>= ) self f =
      match self with
      | Error e -> Error e
      | Ok (o, sp) ->
        match f (o, sp) with
        | Ok (o', sp') -> Ok (o', union sp sp')
        | Error (e', sp') -> Error (e', if is_made_up sp' then sp else sp')


    let wrap o = Ok (o, made_up)

    let wrap_err e = Error (e, made_up)
  end)

  let with_span sp = Ok ((), sp)
end
