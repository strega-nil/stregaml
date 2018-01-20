module Prelude: {
  type span = {
    start_line: int,
    start_column: int,
    end_line: int,
    end_column: int
  };
  type spanned('t) = ('t, span);
  type spanned_result('o, 'e) = result(spanned('o), spanned('e));
};

include (module type of { include Prelude; });

let made_up: span;

let is_made_up: span => bool;

let union: (span, span) => span;

let print_span : span => unit;

module Monad(E: Interfaces.Type): {
  include (
    Interfaces.Result_monad.Interface
      with type t('o) = spanned_result('o, E.t)
      and type error = E.t);

  let with_span: span => t(unit);
};
