module Mutable = Base.Array

module Impl : sig
  type +'a t

  type -'a maybe_initialized

  val to_mutable : 'a t -> 'a Mutable.t

  val of_mutable : 'a Mutable.t -> 'a t

  val create_uninitialized : int -> 'a maybe_initialized Mutable.t

  val initialized_elt : 'a -> 'a maybe_initialized

  val assert_initialized : 'a maybe_initialized Mutable.t -> 'a t
end = struct
  type +'a t = Caml.Obj.t Mutable.t

  type -'a maybe_initialized = Caml.Obj.t

  let to_mutable (type a) (arr : a t) : a Mutable.t = Caml.Obj.magic arr

  let of_mutable (type a) (mut : a Mutable.t) : a t = Caml.Obj.magic mut

  let create_uninitialized (type a) (len : int) : a maybe_initialized Mutable.t
      =
    Mutable.create ~len (Caml.Obj.repr 0)

  let initialized_elt (type a) (el : a) : a maybe_initialized =
    Caml.Obj.repr el

  let assert_initialized (type a) (mut : a maybe_initialized Mutable.t) : a t =
    of_mutable mut
end

type +'a t = 'a Impl.t

type unordered_error = [`Duplicate of int | `Empty of int]

type 'e unordered_user_error = [unordered_error | `User_error of 'e]

let compare f lhs rhs =
  Mutable.compare f (Impl.to_mutable lhs) (Impl.to_mutable rhs)

(* Container *)
let mem (type a) (arr : a t) (el : a) ~(equal : a -> a -> bool) : bool =
  Mutable.mem (Impl.to_mutable arr) el ~equal

let length (type a) (arr : a t) : int = Mutable.length (Impl.to_mutable arr)

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
    ~(f : b -> a -> (b, f) Container.Continue_or_stop.t) ~(finish : b -> f) : f
    =
  Mutable.fold_until (Impl.to_mutable arr) ~init ~f ~finish

let exists (type a) (arr : a t) ~(f : a -> bool) : bool =
  Mutable.exists (Impl.to_mutable arr) ~f

let for_all (type a) (arr : a t) ~(f : a -> bool) : bool =
  Mutable.for_all (Impl.to_mutable arr) ~f

let count (type a) (arr : a t) ~(f : a -> bool) : int =
  Mutable.count (Impl.to_mutable arr) ~f

let sum (type a sum) (ag : (module Commutative_group.S with type t = sum))
    (arr : a t) ~(f : a -> sum) : sum =
  Mutable.sum ag (Impl.to_mutable arr) ~f

let find (type a) (arr : a t) ~(f : a -> bool) : a option =
  Mutable.find (Impl.to_mutable arr) ~f

let find_map (type a b) (arr : a t) ~(f : a -> b option) : b option =
  Mutable.find_map (Impl.to_mutable arr) ~f

let to_list (type a) (arr : a t) : a list =
  Mutable.to_list (Impl.to_mutable arr)

let to_array (type a) (arr : a t) : a array =
  Mutable.copy (Impl.to_mutable arr)

let min_elt (type a) (arr : a t) ~(compare : a -> a -> int) : a option =
  Mutable.min_elt (Impl.to_mutable arr) ~compare

let max_elt (type a) (arr : a t) ~(compare : a -> a -> int) : a option =
  Mutable.max_elt (Impl.to_mutable arr) ~compare

(* array specific functions *)

let max_length = Mutable.max_length

external get : 'a t -> int -> 'a = "%array_safe_get"

external unsafe_get : 'a t -> int -> 'a = "%array_unsafe_get"

let create ~len el = Impl.of_mutable (Mutable.create ~len el)

let init len ~f = Impl.of_mutable (Mutable.init len ~f)

let append fst snd =
  Impl.of_mutable (Mutable.append (Impl.to_mutable fst) (Impl.to_mutable snd))

let concat (type a) (lst : a t list) : a t =
  let mut : a Mutable.t list = Caml.Obj.magic lst in
  Impl.of_mutable (Mutable.concat mut)

let to_mutable = to_array

let of_list lst = Impl.of_mutable (Mutable.of_list lst)

let of_list_map (type a b) (lst : a list) ~(f : a -> b) : b t =
  let len = List.length lst in
  let ret = Impl.create_uninitialized len in
  let f index x = Mutable.set ret index (Impl.initialized_elt (f x)) in
  List.iteri ~f lst ;
  Impl.assert_initialized ret

let of_list_map_unordered (type a b) (lst : a list) ~(f : a -> int * b) :
    (b t, unordered_error) Result.t =
  let len = List.length lst in
  let ret = Impl.create_uninitialized len in
  let ret_some = Mutable.create ~len false in
  let rec helper = function
    | [] -> (
      match Mutable.findi ret_some ~f:(fun _ x -> not x) with
      | None -> Result.Ok ()
      | Some (idx, _) -> Result.Error (`Empty idx) )
    | x :: xs ->
        let idx, el = f x in
        if Mutable.get ret_some idx then Result.Error (`Duplicate idx)
        else (
          Mutable.set ret idx (Impl.initialized_elt el) ;
          Mutable.set ret_some idx true ;
          helper xs )
  in
  match helper lst with
  | Result.Ok () -> Result.Ok (Impl.assert_initialized ret)
  | Result.Error e -> Result.Error e

let of_list_map_result (type a b e) (lst : a list) ~(f : a -> (b, e) Result.t)
    : (b t, e) Result.t =
  let len = List.length lst in
  let ret = Impl.create_uninitialized len in
  let rec helper index = function
    | [] -> Result.Ok ()
    | x :: xs -> (
      match f x with
      | Result.Ok el ->
          Mutable.set ret index (Impl.initialized_elt el) ;
          helper (index + 1) xs
      | Result.Error e -> Result.Error e )
  in
  match helper 0 lst with
  | Result.Ok () -> Result.Ok (Impl.assert_initialized ret)
  | Result.Error e -> Result.Error e

let of_list_map_unordered_result (type a b e) (lst : a list)
    ~(f : a -> (int * b, e) Result.t) : (b t, e unordered_user_error) Result.t
    =
  let len = List.length lst in
  let ret = Impl.create_uninitialized len in
  let ret_some = Mutable.create ~len false in
  let rec helper = function
    | [] -> (
      match Mutable.findi ret_some ~f:(fun _ x -> not x) with
      | None -> Result.Ok ()
      | Some (idx, _) -> Result.Error (`Empty idx) )
    | x :: xs -> (
      match f x with
      | Result.Ok (idx, el) ->
          if Mutable.get ret_some idx then Result.Error (`Duplicate idx)
          else (
            Mutable.set ret idx (Impl.initialized_elt el) ;
            Mutable.set ret_some idx true ;
            helper xs )
      | Result.Error e -> Result.Error (`User_error e) )
  in
  match helper lst with
  | Result.Ok () -> Result.Ok (Impl.assert_initialized ret)
  | Result.Error e -> Result.Error e

let of_mutable (type a) (mut : a Mutable.t) : a t =
  Impl.of_mutable (Mutable.copy mut)

let of_mutable_map (type a b) (mut : a Mutable.t) ~(f : a -> b) : b t =
  Impl.of_mutable (Mutable.map mut ~f)

let of_mutable_map_unordered (type a b) (mut : a Mutable.t) ~(f : a -> int * b)
    : (b t, unordered_error) Result.t =
  let len = Mutable.length mut in
  let ret = Impl.create_uninitialized len in
  let ret_some : bool Mutable.t = Mutable.create ~len false in
  let rec helper index len mut =
    if index = len then
      match Mutable.findi ret_some ~f:(fun _ x -> not x) with
      | None -> Result.Ok ()
      | Some (idx, _) -> Result.Error (`Empty idx)
    else
      let idx, el = f (Mutable.get mut index) in
      if Mutable.get ret_some idx then Result.Error (`Duplicate idx)
      else (
        Mutable.set ret idx (Impl.initialized_elt el) ;
        Mutable.set ret_some idx true ;
        helper (index + 1) len mut )
  in
  match helper 0 len mut with
  | Result.Ok () -> Result.Ok (Impl.assert_initialized ret)
  | Result.Error e -> Result.Error e

let of_mutable_map_result (type a b e) (mut : a Mutable.t)
    ~(f : a -> (b, e) Result.t) : (b t, e) Result.t =
  let len = Mutable.length mut in
  let ret = Impl.create_uninitialized len in
  let rec helper index len mut =
    if index = len then Result.Ok ()
    else
      match f (Mutable.get mut index) with
      | Result.Ok el ->
          Mutable.set ret index (Impl.initialized_elt el) ;
          helper (index + 1) len mut
      | Result.Error e -> Result.Error e
  in
  match helper 0 len mut with
  | Result.Ok () -> Result.Ok (Impl.assert_initialized ret)
  | Result.Error e -> Result.Error e

let of_mutable_map_unordered_result (type a b e) (mut : a Mutable.t)
    ~(f : a -> (int * b, e) Result.t) : (b t, e unordered_user_error) Result.t
    =
  let len = Mutable.length mut in
  let ret = Impl.create_uninitialized len in
  let ret_some : bool Mutable.t = Mutable.create ~len false in
  let rec helper index len mut =
    if index = len then
      match Mutable.findi ret_some ~f:(fun _ x -> not x) with
      | None -> Result.Ok ()
      | Some (idx, _) -> Result.Error (`Empty idx)
    else
      match f (Mutable.get mut index) with
      | Result.Ok (idx, el) ->
          if Mutable.get ret_some idx then Result.Error (`Duplicate idx)
          else (
            Mutable.set ret idx (Impl.initialized_elt el) ;
            Mutable.set ret_some idx true ;
            helper (index + 1) len mut )
      | Result.Error e -> Result.Error (`User_error e)
  in
  match helper 0 len mut with
  | Result.Ok () -> Result.Ok (Impl.assert_initialized ret)
  | Result.Error e -> Result.Error e

let map (type a) (arr : a t) = of_mutable_map (Impl.to_mutable arr)

let map_unordered (type a) (arr : a t) =
  of_mutable_map_unordered (Impl.to_mutable arr)

let map_result (type a) (arr : a t) =
  of_mutable_map_result (Impl.to_mutable arr)

let map_unordered_result (type a) (arr : a t) =
  of_mutable_map_unordered_result (Impl.to_mutable arr)

let iteri (type a) (arr : a t) ~(f : int -> a -> unit) : unit =
  Mutable.iteri (Impl.to_mutable arr) ~f

let mapi (type a b) (arr : a t) ~(f : int -> a -> b) : b t =
  Impl.of_mutable (Mutable.mapi (Impl.to_mutable arr) ~f)

let foldi (type a b) (arr : a t) ~(init : b) ~(f : int -> b -> a -> b) : b =
  Mutable.foldi (Impl.to_mutable arr) ~init ~f

let fold_right (type a b) (arr : a t) ~(f : a -> b -> b) ~(init : b) : b =
  Mutable.fold_right (Impl.to_mutable arr) ~f ~init

let findi (type a) (arr : a t) ~(f : int -> a -> bool) : (int * a) option =
  Mutable.findi (Impl.to_mutable arr) ~f
