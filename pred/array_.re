include Array;

let iter = (arr) => {
  Iter.make(0, (idx) => {
    if (idx < length(arr)) {
      Some((idx + 1, arr[idx]))
    } else {
      None
    }
  })
};
