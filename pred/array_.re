include Array;

let iter = (arr) => {
  let idx = ref(0);
  Iter.from_next(() => {
    if (idx^ < length(arr)) {
      let ret = Some(arr[idx^]);
      idx := idx^ + 1;
      ret
    } else {
      None
    }
  })
};
