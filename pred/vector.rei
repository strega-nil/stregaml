type t('a);

let make: unit => t('a);
let with_capacity: int => t('a);
let push: 'a => t('a) => unit;
let pop: t('a) => 'a;
let idx: t('a) => int => 'a;
let len: t('a) => int;
