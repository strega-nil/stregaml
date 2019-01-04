include module type of struct include Base.String end

val concat_array : ?sep:t -> t Array.t -> t

val concat_mutable_array : ?sep:t -> t Array.Mutable.t -> t

val concat_sequence : ?sep:t -> t Sequence.t -> t
