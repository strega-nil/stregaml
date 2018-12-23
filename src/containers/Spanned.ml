module Span = struct
  type t =
    | Span :
        { start_line: int
        ; start_column: int
        ; end_line: int
        ; end_column: int }
        -> t

  let start_line (Span r) = r.start_line

  let start_column (Span r) = r.start_column

  let end_line (Span r) = r.end_line

  let end_column (Span r) = r.end_column

  let equal t1 t2 =
    start_line t1 = start_line t2
    && start_column t1 = start_column t2
    && end_line t1 = end_line t2
    && end_column t1 = end_column t2

  let made_up =
    Span {start_line= 0; start_column= 0; end_line= 0; end_column= 0}

  let is_made_up sp = equal sp made_up

  let union fst snd =
    if is_made_up fst then snd
    else if is_made_up snd then fst
    else
      Span
        { start_line= start_line fst
        ; start_column= start_column fst
        ; end_line= end_line snd
        ; end_column= end_column snd }

  let to_string (Span {start_line; start_column; end_line; end_column}) =
    Printf.sprintf "(%d, %d) to (%d, %d)" start_line start_column end_line
      end_column
end

type 'a t = 'a * Span.t

let to_string (el, sp) ~f =
  Printf.sprintf "%s at %s" (f el) (Span.to_string sp)

module Result = struct
  type 'a spanned = 'a t

  type ('o, 'e) t = ('o, 'e) Result.t spanned

  module Monad_implementation = struct
    include Monad.Make2 (struct
      type nonrec ('o, 'e) t = ('o, 'e) t

      let bind (self, sp) ~f =
        match self with
        | Error e -> (Error e, sp)
        | Ok o -> (
          match f o with
          | Ok o', sp' -> (Ok o', Span.union sp sp')
          | Error e', sp' -> (Error e', if Span.is_made_up sp' then sp else sp')
          )

      let map = `Define_using_bind

      let return o = (Ok o, Span.made_up)
    end)
  end

  module Monad = struct
    include Monad_implementation.Let_syntax

    let spanned_bind (t, sp) =
      match t with Error e -> (Error e, sp) | Ok o -> (Ok (o, sp), sp)

    let spanned_lift (o, sp) = (Ok o, sp)

    let return_err e = (Error e, Span.made_up)

    let with_span sp = (Ok (), sp)

    let span_of (_, sp) = sp

    module Return = struct
      module List = struct
        let map lst ~f =
          let open! Let_syntax in
          let rec helper f = function
            | [] -> return []
            | x :: xs ->
                let%bind x = f x in
                let%bind rest = helper f xs in
                return (x :: rest)
          in
          helper f lst

        let fold lst ~init ~f =
          let rec helper f folded sp lst =
            match lst with
            | [] ->
                let%bind () = with_span sp in
                folded
            | x :: xs -> (
              match folded with
              | Result.Ok o, sp ->
                  let folded = f o x in
                  helper f folded sp xs
              | Result.Error e, sp -> (Result.Error e, sp) )
          in
          helper f (return init) Span.made_up lst

        let iter lst ~f =
          let open! Let_syntax in
          let rec helper f = function
            | [] -> return ()
            | x :: xs ->
                let%bind () = f x in
                helper f xs
          in
          helper f lst

        let iteri lst ~f =
          let open! Let_syntax in
          let rec helper f index = function
            | [] -> return ()
            | x :: xs ->
                let%bind () = f index x in
                helper f (index + 1) xs
          in
          helper f 0 lst
      end
    end
  end
end
