module Mutable = Base.Array

type +'a t

type unordered_error = [`Duplicate of int | `Empty of int]

type 'e unordered_user_error = [unordered_error | `User_error of 'e]

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

include Container.S1 with type 'a t := 'a t

val max_length : int

external get : 'a t -> int -> 'a = "%array_safe_get"

external unsafe_get : 'a t -> int -> 'a = "%array_unsafe_get"

val create : len:int -> 'a -> 'a t

val init : int -> f:(int -> 'a) -> 'a t

val append : 'a t -> 'a t -> 'a t

val concat : 'a t list -> 'a t

val to_mutable : 'a t -> 'a Mutable.t

val of_list : 'a list -> 'a t

val of_list_map : 'a list -> f:('a -> 'b) -> 'b t

val of_list_map_unordered :
  'a list -> f:('a -> int * 'b) -> ('b t, unordered_error) Result.t

val of_list_map_result :
  'a list -> f:('a -> ('b, 'e) Result.t) -> ('b t, 'e) Result.t

val of_list_map_unordered_result :
     'a list
  -> f:('a -> (int * 'b, 'e) Result.t)
  -> ('b t, 'e unordered_user_error) Result.t

val of_mutable : 'a Mutable.t -> 'a t

val of_mutable_map : 'a Mutable.t -> f:('a -> 'b) -> 'b t

val of_mutable_map_unordered :
  'a Mutable.t -> f:('a -> int * 'b) -> ('b t, unordered_error) Result.t

val of_mutable_map_result :
  'a Mutable.t -> f:('a -> ('b, 'e) Result.t) -> ('b t, 'e) Result.t

val of_mutable_map_unordered_result :
     'a Mutable.t
  -> f:('a -> (int * 'b, 'e) Result.t)
  -> ('b t, 'e unordered_user_error) Result.t

val map : 'a t -> f:('a -> 'b) -> 'b t

val map_unordered :
  'a t -> f:('a -> int * 'b) -> ('b t, unordered_error) Result.t

val map_result : 'a t -> f:('a -> ('b, 'e) Result.t) -> ('b t, 'e) Result.t

val map_unordered_result :
     'a t
  -> f:('a -> (int * 'b, 'e) Result.t)
  -> ('b t, 'e unordered_user_error) Result.t

val iteri : 'a t -> f:(int -> 'a -> unit) -> unit

val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t

val foldi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b) -> 'b

val fold_right : 'a t -> f:('a -> 'b -> 'b) -> init:'b -> 'b

val findi : 'a t -> f:(int -> 'a -> bool) -> (int * 'a) option
