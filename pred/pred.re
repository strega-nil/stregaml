module Array = Array_;

module List = List_;

module Iter = Iter;

module String_buffer = String_buffer;

module Result = Result;

module Option = Option;

module Dynamic_array = Dynamic_array;

module Util = Util;

module Interfaces = Interfaces;

include Interfaces.Prelude;

type result('o, 'e) = Result.t('o, 'e) = | Ok('o) | Err('e);

type iter('a) = Iter.t('a);

type string_buffer = String_buffer.t;

type dynamic_array('a) = Dynamic_array.t('a);

let unimplemented = Errors.unimplemented;

let array_of_rev_list = Util.array_of_rev_list;
