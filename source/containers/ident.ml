type t = string

let of_uchar_list lst =
  let buff = Buffer.create 32 in
  let n = Uunf.create `NFC in
  let rec add v =
    match Uunf.add n v with
    | `Uchar u ->
        Uutf.Buffer.add_utf_8 buff u ;
        add `Await
    | `Await | `End -> ()
  in
  let rec helper = function
    | [] -> Buffer.contents buff
    | x :: xs ->
        add (`Uchar x) ;
        helper xs
  in
  helper lst

let uchar_to_string ch =
  let buff = Buffer.create 4 in
  Uutf.Buffer.add_utf_8 buff ch ;
  Buffer.contents buff

let of_string_unsafe s = s

let empty = ""

let equal = String.equal
