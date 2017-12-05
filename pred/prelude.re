type result('t, 'e) =
  | Ok('t)
  | Err('e);

type text = Text.t;

type text_buffer = Text.buffer;

type vector('a) = Vector.t('a);

let print_text = Text.print;

let unimplemented = Unimplemented.unimplemented;
