open Pred;

module Prelude = {
  type span = {
    start_line: int,
    start_column: int,
    end_line: int,
    end_column: int
  };
  type spanned('t) = ('t, span);
  type spanned_result('o, 'e) = result(spanned('o), spanned('e));
};

include Prelude;

let made_up: span = {start_line: 0, start_column: 0, end_line: 0, end_column: 0};

let is_made_up: span => bool = (sp) => sp == made_up;

let union: (span, span) => span =
  (fst, snd) =>
    if (is_made_up(fst)) {
      snd;
    } else if (is_made_up(snd)) {
      fst;
    } else {
      {
        start_line: fst.start_line,
        start_column: fst.start_column,
        end_line: snd.end_line,
        end_column: snd.end_column
      };
    };

let print_span
  : span => unit
  = ({start_line, start_column, end_line, end_column})
  => {
    Printf.printf(
      "(%d, %d) to (%d, %d)",
      start_line,
      start_column,
      end_line,
      end_column)
  };


module Result_monad = {
  let (>>=): (spanned_result('o, 'e), 'o => spanned_result('o2, 'e)) => spanned_result('o2, 'e) =
    (self, f) =>
      switch self {
      | Ok((o, sp)) =>
        switch (f(o)) {
        | Ok((o', sp')) => Ok((o', union(sp, sp')))
        | Err((e', sp')) =>
          Err((
            e',
            if (is_made_up(sp')) {
              sp;
            } else {
              sp';
            }
          ))
        }
      | Err((e, sp)) => Err((e, sp))
      };
  let pure: 'o => spanned_result('o, 'e) = (o) => Ok((o, made_up));
  let pure_err: 'e => spanned_result('o, 'e) = (e) => Err((e, made_up));
  let with_span: span => spanned_result(unit, 'e) = (sp) => Ok(((), sp));
};
