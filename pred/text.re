type t = Buffer.t;

/* NOTE(ubsan): this is mutable for safety, after from_buffer */
type buffer = {mutable contents: Buffer.t};

let from_string = (s) => Unimplemented.unimplemented();

let from_buffer = (buff) => Unimplemented.unimplemented();

let hash = (self) => Unimplemented.unimplemented();

let print = (self) => Unimplemented.unimplemented();
