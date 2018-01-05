type t('o, 'e) =
  | Ok('o)
  | Err('e);

let map: ('o => 'o2, t('o, 'e)) => t('o2, 'e);

let map_err: ('e => 'e2, t('o, 'e)) => t('o, 'e2);

let and_then: ('o => t('o2, 'e), t('o, 'e)) => t('o2, 'e);

module Monad(E: Interfaces.Type) {
  type nonrec t('o) = t('o, E.t);
  type error = E.t;
  include (
    Interfaces.Monad_result
      with type error := error
      and type t('o) := t('o));
};
