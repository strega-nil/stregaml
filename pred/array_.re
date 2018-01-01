include Array;

let iter = (arr) => {
  Iter.from_next(0, (idx) => {
    if (idx < length(arr)) {
      Some((idx + 1, arr[idx]))
    } else {
      None
    }
  })
};
