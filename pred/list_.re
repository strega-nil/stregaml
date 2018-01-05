include List;

let iter = (init) => {
  Iter.make(init, (lst) => {
    switch lst {
    | [x, ...xs] => Some((xs, x))
    | [] => None
    }
  })
};

module Monad = {
  type t('a) = list('a);

  let pure = (x) => [x];
  let (>>=) = (lst, f) => {
    /* impl note - uses a ref in order to be tail recursive */
    let rec helper = (old, ret, f) =>
      switch old {
      | [x, ...xs] =>
        /* second impl note - not tail recursive */
        ret := append(f(x), ret^);
        helper(xs, ret, f);
      | [] => ()
      };

    let ret = ref([]);
    helper(lst, ret, f);
    ret^;
  };
};
