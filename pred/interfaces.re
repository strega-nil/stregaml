module Prelude = {
  module type Type = {
    type t
  };
};

include Prelude;

module type Monad = {
  type t('a);
  let (>>=): (t('a), 'a => t('b)) => t('b);
  let pure: 'a => t('a);
};

module type Monad_result = {
  include Monad;
  type error;
  let pure_err: error => t('a);
};
