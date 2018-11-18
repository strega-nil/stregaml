type t = string

let uchar_to_string ch =
  let buff = Buffer.create 4 in
  Uutf.Buffer.add_utf_8 buff ch ;
  Buffer.contents buff

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
    | [] ->
        add `End ;
        Buffer.contents buff
    | x :: xs ->
        add (`Uchar x) ;
        helper xs
  in
  helper lst

let of_string_unsafe s = s

let first_codepoint id =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String id) in
  match Uutf.decode decoder with
  | `End -> None
  | `Uchar u -> Some u
  | _ -> assert false

let first_codepoint_exn id =
  match first_codepoint id with
  | Some u -> u
  | None -> failwith "empty string"

let empty = ""

let equal = String.equal
