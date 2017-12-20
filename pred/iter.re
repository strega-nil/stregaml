type t('a) = unit => option('a);

let rec for_each = (f, self) =>
  switch (self()) {
  | Some(el) =>
    f(el);
    for_each(f, self);
  | None => ()
  };

let rec for_each_break = (f, self) =>
  switch (self()) {
  | Some(el) =>
    switch (f(el)) {
    | Some(ret) => Some(ret)
    | None => for_each_break(f, self)
    }
  | None => None
  };

let from_next = (func) => func;

exception Iter_zipped_iterators_of_different_lengths;
let zip = (fst, snd) => () => {
  switch ((fst(), snd())) {
  | (Some(f), Some(s)) => Some((f, s))
  | (None, None) => None
  | _ => raise(Iter_zipped_iterators_of_different_lengths)
  }
};
