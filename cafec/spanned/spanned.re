module Prelude = {
  type span = {
    start_line: int,
    start_column: int,
    end_line: int,
    end_column: int
  };
  type spanned('o, 'e) =
    | SOk('o, span)
    | SErr('e, span);
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

module Monad = {
  let (>>=): (spanned('o, 'e), 'o => spanned('o2, 'e)) => spanned('o2, 'e) =
    (self, f) =>
      switch self {
      | SOk(o, sp) =>
        switch (f(o)) {
        | SOk(o', sp') => SOk(o', union(sp, sp'))
        | SErr(e', sp') =>
          SErr(
            e',
            if (is_made_up(sp')) {
              sp;
            } else {
              sp';
            }
          )
        }
      | SErr(e, sp) => SErr(e, sp)
      };
  let pure: 'o => spanned('o, 'e) = (o) => SOk(o, made_up);
  let with_span: span => spanned(unit, 'e) = (sp) => SOk((), sp);
};
