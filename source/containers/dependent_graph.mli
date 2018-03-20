type 'a t

module Error: sig
  (* this'll eventually include all the 'bs in the cyclic dep *)
  type 'b t =
    | Cyclic_dependency of 'b
    | Out_of_bounds of {bound: int; by: int}
end

val make :
  'b list -> 
  ('b -> (int -> ('a, 'b Error.t) Result.t) -> ('a, 'b Error.t) Result.t)
  -> ('a t, 'b Error.t) Result.t

val to_seq : 'a t -> 'a Sequence.t
