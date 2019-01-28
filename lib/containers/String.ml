include Base.String

let concat_mutable_array = Base.String.concat_array

let concat_array ?(sep : t option) (arr : t Array.t) : t =
  concat_mutable_array ?sep (Array.to_mutable_inplace arr)

let concat_sequence ?(sep : t option) (arr : t Sequence.t) : t =
  concat ?sep (Sequence.to_list arr)
