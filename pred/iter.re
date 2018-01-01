type t('a) =
  | Iter('state, 'state => option(('state, 'a))): t('a);

let for_each = (f, Iter(state, iter)) => {
  let rec helper = (state) =>
    switch (iter(state)) {
    | Some((state, el)) =>
      f(el);
      helper(state);
    | None => ()
    };
  helper(state);
};

let for_each_break = (f, Iter(state, iter)) => {
  let rec helper = (state) =>
    switch (iter(state)) {
    | Some((state, el)) =>
      switch (f(el)) {
      | Some(ret) => Some(ret)
      | None => helper(state)
      }
    | None => None
    };
  helper(state);
};

let from_next = (state, func) => Iter(state, func);

exception Iter_zipped_iterators_of_different_lengths;
let zip = (Iter(state1, iter1), Iter(state2, iter2)) => {
  let state = (state1, state2);
  let iter = ((state1, state2)) =>
    switch ((iter1(state1), iter2(state2))) {
    | (Some((state1, el1)), Some((state2, el2))) =>
      Some(((state1, state2), (el1, el2)))
    | (None, None) => None
    | _ => raise(Iter_zipped_iterators_of_different_lengths)
    };
  Iter(state, iter)
};

let enumerate = (_iter) => {
  assert false;
  /*
  let i = ref(0);
  () => {
    let old = i^;
    i := old + 1;
    switch (iter()) {
    | Some(el) => Some((old, el))
    | None => None
    }
  };
  */
};

let range = (_init, _fin) => {
  assert false;
  /*
  let i = ref(init);
  () => {
    let old = i^;
    if (old >= fin) {
      None
    } else {
      i := old + 1;
      Some(old)
    }
  }
  */
};
