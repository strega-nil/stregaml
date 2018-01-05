include (module type of List);

let iter: list('a) => Iter.t('a);

module Monad: {
  type t('a) = list('a);
  include Interfaces.Monad with type t('a) := t('a);
};
