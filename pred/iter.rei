type t('a, 'b);

let for_each: ('a => unit) => t('a, 'b) => 'b;

/**
  for each left value, calls the function passed to for_each with the inner
  value.
  for_each returns the first right value
*/
let from_next: (unit => Result.t('a, 'b)) => t('a, 'b);
