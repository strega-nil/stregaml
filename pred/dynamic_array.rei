type t('a);

let make: unit => t('a);

let with_capacity: int => t('a);

let clone: t('a) => t('a);

let length: t('a) => int;

let capacity: t('a) => int;

let push: (t('a), 'a) => unit;

let pop: t('a) => 'a;

let get: (t('a), int) => 'a;

let set: (t('a), int, 'a) => unit;

let to_array: t('a) => array('a);
