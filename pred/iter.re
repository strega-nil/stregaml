type hold_type('a, 'b) = {
  data: 'a,
  func: 'a => option(('a, 'b))
};

type t(_) =
  | Mk_t(hold_type('a, 'b)): t('b);

let rec for_each = (f, Mk_t(self)) =>
  switch (self.func(self.data)) {
  | Some((data, el)) =>
    f(el);
    for_each(f, Mk_t({data, func: self.func}));
  | None => ()
  };

let rec for_each_break = (f, Mk_t(self)) =>
  switch (self.func(self.data)) {
  | Some((data, el)) =>
    switch (f(el)) {
    | Some(ret) => Some(ret)
    | None => for_each_break(f, Mk_t({data, func: self.func}))
    }
  | None => None
  };

let from_next = (data, func) => Mk_t({data, func});
