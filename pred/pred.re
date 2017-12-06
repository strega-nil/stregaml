module Iter = Iter;

module String_buffer = String_buffer;

module Result = Result;

type result('t, 'e) = Result.t('t, 'e);

type iter('a) = Iter.t('a);

type string_buffer = String_buffer.t;

let unimplemented = Unimplemented.unimplemented;
