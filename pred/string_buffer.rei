/* TODO(ubsan): support unicode */

type t;

let make: unit => t;
let with_capacity: int => t;
let clone: t => t;

let length: t => int;
let capacity: t => int;

let push: t => char => unit;
let pop: t => char;

let get: (t, int) => char;
let set: (t, int, char) => unit;

let to_string: t => string;
