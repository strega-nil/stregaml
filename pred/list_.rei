include (module type of List);

let iter: list('a) => Iter.t('a);

module Monad: Interfaces.Monad with type t('a) = list('a);
