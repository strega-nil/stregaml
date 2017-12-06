type t('a);

let for_each: ('a => unit) => t('a) => unit;
let for_each_break: ('a => option('b)) => t('a) => option('b);

let from_next: 'a => ('a => option(('a, 'b))) => t('b);
