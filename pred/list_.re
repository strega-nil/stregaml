include List;

let iter = (init) => {
  let curr = ref(init);
  Iter.from_next(() => {
    switch (curr^) {
    | [x, ...xs] => curr := xs; Some(x)
    | [] => None
    }
  })
};
