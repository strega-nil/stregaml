let array_of_rev_list = (lst) =>
  switch lst {
  | [] => [||]
  | [x, ...xs] =>
    let len = List.length(xs);
    let ret = Array.make(len + 1, x);
    let rec helper = (lst, idx) =>
      switch lst {
      | [x, ...xs] =>
        ret[idx] = x;
        helper(xs, idx - 1);
      | [] => ()
      };
    helper(xs, len - 1);
    ret;
  };
