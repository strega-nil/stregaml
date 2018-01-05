type t('a);

let for_each: ('a => unit, t('a)) => unit;

/**
  if the inner closure returns Some(v), then for_each_break returns Some(v).
  else, if the inner closure returns None for all iterated values, return None
 */
let for_each_break: ('a => option('b), t('a)) => option('b);

let make: ('state, 'state => option(('state, 'a))) => t('a);

exception Iter_zipped_iterators_of_different_lengths;
let zip: (t('a), t('b)) => t(('a, 'b));
let enumerate: t('a) => t((int, 'a));
let range: (int, int) => t(int);
