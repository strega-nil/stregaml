type t;
type buffer;

let from_string: string => t;
let from_buffer: buffer => t;

let hash: t => int;
let print: t => unit;
