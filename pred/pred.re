module Iter = Iter;

module String_buffer = String_buffer;

module Result = Result;

type result('o, 'e) = Result.t('o, 'e) = | Ok('o) | Err('e);

type iter('a) = Iter.t('a);

type string_buffer = String_buffer.t;

let unimplemented = Unimplemented.unimplemented;
