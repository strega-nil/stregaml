type 'a tristate = Not_started | Started | Finished of 'a

(* just so we don't have to copy to a new array *)
type 'a t = 'a tristate array

module Error = struct
  type 'b t = Cyclic_dependency of 'b | Out_of_bounds of {bound: int; by: int}
end

let empty len = Array.create ~len Not_started

let make init f =
  let init = Array.of_list init in
  let size = Array.length init in
  let arr = empty size in
  let rec get idx =
    if idx < size then
      match arr.(idx) with
      | Finished f -> Ok f
      | Started -> Error (Error.Cyclic_dependency init.(idx))
      | Not_started ->
          arr.(idx) <- Started ;
          match f init.(idx) get with
          | Ok o -> arr.(idx) <- Finished o ; Ok o
          | Error e -> Error e
    else Error (Error.Out_of_bounds {bound= size; by= idx})
  in
  let rec helper idx =
    if idx < size then
      match get idx with Ok _ -> helper (idx + 1) | Error e -> Error e
    else Ok arr
  in
  helper 0


let to_seq self =
  let len = Array.length self in
  let init = 0 in
  let f state =
    if state < len then
      match self.(state) with
      | Finished f -> Some (f, state + 1)
      | _ -> assert false
    else None
  in
  Sequence.unfold ~init ~f
