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

let made_up = {start_line: 0, start_column: 0, end_line: 0, end_column: 0};

let is_made_up = (sp) => sp == made_up;

let union = (fst, snd) =>
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

let print_span = ({start_line, start_column, end_line, end_column}) => {
    Printf.printf(
      "(%d, %d) to (%d, %d)",
      start_line,
      start_column,
      end_line,
      end_column)
  };

module Monad(E: Interfaces.Type) = {
  type t('o) = spanned_result('o, E.t);
  type error = E.t;

  let (>>=) = (self, f) =>
    switch self {
    | Ok((o, sp)) =>
      switch (f(o)) {
      | Ok((o', sp')) => Ok((o', union(sp, sp')))
      | Error((e', sp')) =>
        Error((
          e',
          if (is_made_up(sp')) {
            sp;
          } else {
            sp';
          }
        ))
      }
    | Error((e, sp)) => Error((e, sp))
    };
  let wrap = (o) => Ok((o, made_up));
  let wrap_err = (e) => Error((e, made_up));
  let with_span = (sp) => Ok(((), sp));

  module Let_syntax = {
    let bind = (x, ~f) => x >>= f;
  };
};
