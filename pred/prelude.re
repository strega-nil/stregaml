type result('t, 'e) =
  | Ok('t)
  | Err('e);

exception Unimplemented;

let unimplemented = () => raise(Unimplemented);
