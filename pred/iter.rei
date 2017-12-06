type t('a);

let for_each: ('a => unit) => t('a) => unit;

/**
  if the inner closure returns Some(v), then for_each_break returns Some(v).
  else, if the inner closure returns None for all iterated values, return None
 */
let for_each_break: ('a => option('b)) => t('a) => option('b);

let from_next: (unit => option('a)) => t('a);
