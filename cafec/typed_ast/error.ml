module Spanned = Cafec_spanned

type t

module Monad_spanned = Spanned.Monad (struct
  type nonrec t = t
end)

let print _ = assert false

let print_spanned _ = assert false
