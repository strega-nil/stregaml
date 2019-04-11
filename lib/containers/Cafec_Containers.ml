module Array = Array
module Mutable_array = Array.Mutable
module String = String
module Spanned = Spanned
module Nfc_string = Nfc_string
module Name = Name

type 'a array = 'a Array.t

type 'a mutable_array = 'a Mutable_array.t

exception Unimplemented [@@deriving_inline sexp]

[@@@end]
