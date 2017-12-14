module Iter = Iter;

module String_buffer = String_buffer;

module Result = Result;

module Dynamic_array = Dynamic_array;

type result('o, 'e) = Result.t('o, 'e) = | Ok('o) | Err('e);

type iter('a) = Iter.t('a);

type string_buffer = String_buffer.t;

type dynamic_array('a) = Dynamic_array.t('a);

let unimplemented = Errors.unimplemented;
