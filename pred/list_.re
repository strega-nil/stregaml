include List;

let iter = (init) => {
  Iter.from_next(init, (lst) => {
    switch lst {
    | [x, ...xs] => Some((xs, x))
    | [] => None
    }
  })
};
