type t('o, 'e) =
  | Ok('o)
  | Err('e);

let map: ('o => 'o2, t('o, 'e)) => t('o2, 'e);

let map_err: ('e => 'e2, t('o, 'e)) => t('o, 'e2);

let and_then: ('o => t('o2, 'e), t('o, 'e)) => t('o2, 'e);

module Monad: {
  let (>>=): (t('o, 'e), 'o => t('o2, 'e)) => t('o2, 'e);
  let pure: 'o => t('o, 'e);
  let pure_err: 'e => t('o, 'e);
};
