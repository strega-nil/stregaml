type t('o, 'e) =
  | Ok('o)
  | Err('e);

let map = (f, self) =>
  switch self {
  | Ok(ok) => Ok(f(ok))
  | Err(e) => Err(e)
  };

let map_err = (f, self) =>
  switch self {
  | Ok(ok) => Ok(ok)
  | Err(e) => Err(f(e))
  };

let and_then = (f, self) =>
  switch self {
  | Ok(ok) => f(ok)
  | Err(e) => Err(e)
  };

module Monad = {
  let (>>=) = (self, f) => and_then(f, self);
  let pure = (ok) => Ok(ok);
};
