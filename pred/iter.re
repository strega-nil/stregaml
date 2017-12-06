type t('a, 'b) = unit => Result.t('a, 'b);

let rec for_each = (f, self) =>
  switch (self()) {
  | Result.Ok(el) =>
    f(el);
    for_each(f, self);
  | Result.Err(ret) => ret
  };

let from_next = (func) => func;
