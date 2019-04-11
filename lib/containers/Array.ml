module Mutable = Base.Array

module Impl : sig
  type +'a t

  val empty : 'a t

  val to_mutable : 'a t -> 'a Mutable.t

  val of_mutable : 'a Mutable.t -> 'a t
end = struct
  type +'a t = Caml.Obj.t

  (* it doesn't matter which runtime type an empty array has *)
  let empty = Caml.Obj.repr ([||] : int array)

  let to_mutable (type a) (arr : a t) : a Mutable.t = Caml.Obj.obj arr

  let of_mutable (type a) (mut : a Mutable.t) : a t = Caml.Obj.repr mut
end

type +'a t = 'a Impl.t

type unordered_error =
  | Duplicate of int
  | Empty_cell of int

let compare f lhs rhs =
  Mutable.compare f (Impl.to_mutable lhs) (Impl.to_mutable rhs)

let equal equal lhs rhs =
  Mutable.equal equal (Impl.to_mutable lhs) (Impl.to_mutable rhs)

(* Container *)
let mem (type a) (arr : a t) (el : a) ~(equal : a -> a -> bool) : bool
    =
  Mutable.mem (Impl.to_mutable arr) el ~equal

let length (type a) (arr : a t) : int =
  Mutable.length (Impl.to_mutable arr)

let is_empty (type a) (arr : a t) : bool =
  Mutable.is_empty (Impl.to_mutable arr)

let iter (type a) (arr : a t) ~(f : a -> unit) : unit =
  Mutable.iter (Impl.to_mutable arr) ~f

let fold (type a b) (arr : a t) ~(init : b) ~(f : b -> a -> b) : b =
  Mutable.fold (Impl.to_mutable arr) ~init ~f

let fold_result (type a b e) (arr : a t) ~(init : b)
    ~(f : b -> a -> (b, e) Result.t) : (b, e) Result.t =
  Mutable.fold_result (Impl.to_mutable arr) ~init ~f

let fold_until (type a b f) (arr : a t) ~(init : b)
    ~(f : b -> a -> (b, f) Container.Continue_or_stop.t)
    ~(finish : b -> f) : f =
  Mutable.fold_until (Impl.to_mutable arr) ~init ~f ~finish

let exists (type a) (arr : a t) ~(f : a -> bool) : bool =
  Mutable.exists (Impl.to_mutable arr) ~f

let for_all (type a) (arr : a t) ~(f : a -> bool) : bool =
  Mutable.for_all (Impl.to_mutable arr) ~f

let count (type a) (arr : a t) ~(f : a -> bool) : int =
  Mutable.count (Impl.to_mutable arr) ~f

let sum (type a sum)
    (ag : (module Container.Summable with type t = sum)) (arr : a t)
    ~(f : a -> sum) : sum =
  Mutable.sum ag (Impl.to_mutable arr) ~f

let find (type a) (arr : a t) ~(f : a -> bool) : a option =
  Mutable.find (Impl.to_mutable arr) ~f

let find_map (type a b) (arr : a t) ~(f : a -> b option) : b option =
  Mutable.find_map (Impl.to_mutable arr) ~f

let to_list (type a) (arr : a t) : a list =
  Mutable.to_list (Impl.to_mutable arr)

let to_array (type a) (arr : a t) : a array =
  Mutable.copy (Impl.to_mutable arr)

let min_elt (type a) (arr : a t) ~(compare : a -> a -> int) : a option
    =
  Mutable.min_elt (Impl.to_mutable arr) ~compare

let max_elt (type a) (arr : a t) ~(compare : a -> a -> int) : a option
    =
  Mutable.max_elt (Impl.to_mutable arr) ~compare

(* array specific functions *)

let max_length = Mutable.max_length

external get : 'a t -> int -> 'a = "%array_safe_get"

external unsafe_get : 'a t -> int -> 'a = "%array_unsafe_get"

let empty = Impl.empty

let unary (type a) (el : a) : a t = Impl.of_mutable [|el|]

let binary (type a) (el1 : a) (el2 : a) : a t =
  Impl.of_mutable [|el1; el2|]

let create ~len el = Impl.of_mutable (Mutable.create ~len el)

let init len ~f = Impl.of_mutable (Mutable.init len ~f)

let of_sequence (type a) ~(len : int) (seq : a Sequence.t) : a t =
  match Sequence.next seq with
  | None ->
      assert (len = 0) ;
      empty
  | Some (el, seq) ->
      let ret = Mutable.create el ~len in
      let length_init = ref 1 in
      let f idx el =
        Mutable.set ret (idx + 1) el ;
        length_init := idx + 2
      in
      Sequence.iteri seq ~f ;
      if !length_init = len
      then Impl.of_mutable ret
      else
        let error =
          Printf.sprintf "length of `seq` (%d) != `len` (%d)"
            !length_init len
        in
        raise (Invalid_argument error)

let of_sequence_unordered (type a) ~(len : int)
    (seq : (int * a) Sequence.t) : (a t, unordered_error) Result.t =
  let rec helper ret ret_some seq =
    match Sequence.next seq with
    | Some ((idx, el), seq) ->
        if Mutable.get ret_some idx
        then Result.Error (Duplicate idx)
        else
          let () = Mutable.set ret idx el in
          let () = Mutable.set ret_some idx true in
          helper ret ret_some seq
    | None -> (
      match Mutable.findi ret_some ~f:(fun _ x -> not x) with
      | None -> Result.Ok (Impl.of_mutable ret)
      | Some (idx, _) -> Result.Error (Empty_cell idx) )
  in
  match Sequence.next seq with
  | None ->
      assert (len = 0) ;
      Result.Ok empty
  | Some ((idx, el), seq) ->
      let ret = Mutable.create el ~len in
      let ret_some = Mutable.create false ~len in
      let () = Mutable.set ret_some idx true in
      helper ret ret_some seq

let to_sequence (type a) (arr : a t) : a Sequence.t =
  (*
    since nobody can get mutable access to these `a` outside of this function,
    there's no problem with using this
  *)
  Mutable.to_sequence_mutable (Impl.to_mutable arr)

let append fst snd =
  Impl.of_mutable
    (Mutable.append (Impl.to_mutable fst) (Impl.to_mutable snd))

let concat (type a) (lst : a t list) : a t =
  let mut : a Mutable.t list = Caml.Obj.magic lst in
  Impl.of_mutable (Mutable.concat mut)

let to_mutable = to_array

let to_mutable_inplace = Impl.to_mutable

let of_list lst = Impl.of_mutable (Mutable.of_list lst)

let of_mutable (type a) (mut : a Mutable.t) : a t =
  Impl.of_mutable (Mutable.copy mut)

let of_mutable_inplace = Impl.of_mutable

let iteri (type a) (arr : a t) ~(f : int -> a -> unit) : unit =
  Mutable.iteri (Impl.to_mutable arr) ~f

let map (type a b) (arr : a t) ~(f : a -> b) =
  Impl.of_mutable (Mutable.map (Impl.to_mutable arr) ~f)

let mapi (type a b) (arr : a t) ~(f : int -> a -> b) : b t =
  Impl.of_mutable (Mutable.mapi (Impl.to_mutable arr) ~f)

let foldi (type a b) (arr : a t) ~(init : b) ~(f : int -> b -> a -> b)
    : b =
  Mutable.foldi (Impl.to_mutable arr) ~init ~f

let fold_right (type a b) (arr : a t) ~(f : a -> b -> b) ~(init : b) :
    b =
  Mutable.fold_right (Impl.to_mutable arr) ~f ~init

let findi (type a) (arr : a t) ~(f : int -> a -> bool) :
    (int * a) option =
  Mutable.findi (Impl.to_mutable arr) ~f

let find_from (type a) (arr : a t) (idx : int) ~(f : a -> bool) :
    a option =
  let rec check_after_idx idx =
    if idx = length arr
    then None
    else
      let cur = get arr idx in
      if f cur then Some cur else check_after_idx (idx + 1)
  in
  check_after_idx idx

let findi_from (type a) (arr : a t) (idx : int) ~(f : int -> a -> bool)
    : (int * a) option =
  let rec check_after_idx idx =
    if idx = length arr
    then None
    else
      let cur = get arr idx in
      if f idx cur then Some (idx, cur) else check_after_idx (idx + 1)
  in
  check_after_idx idx

let mem_from (type a) (arr : a t) (el : a) (idx : int)
    ~(equal : a -> a -> bool) : bool =
  let rec check_after_idx idx =
    if idx = length arr
    then false
    else
      let cur = get arr idx in
      if equal el cur then true else check_after_idx (idx + 1)
  in
  check_after_idx idx

let find_nonconsecutive_duplicates (type a) (arr : a t)
    ~(equal : a -> a -> bool) : (a * a) option =
  let rec check_for_dups idx =
    if idx = length arr
    then None
    else
      let cur = get arr idx in
      let f el = equal cur el in
      match find_from ~f arr (idx + 1) with
      | Some el -> Some (cur, el)
      | None -> check_for_dups (idx + 1)
  in
  check_for_dups 0

let findi_nonconsecutive_duplicates (type a) (arr : a t)
    ~(equal : int * a -> int * a -> bool) :
    ((int * a) * (int * a)) option =
  let rec check_for_dups idx =
    if idx = length arr
    then None
    else
      let cur = get arr idx in
      let f el_idx el = equal (idx, cur) (el_idx, el) in
      match findi_from ~f arr (idx + 1) with
      | Some latter -> Some ((idx, cur), latter)
      | None -> check_for_dups (idx + 1)
  in
  check_for_dups 0
