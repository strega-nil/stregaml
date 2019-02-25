type t = string

let uchar_to_string ch =
  let buff = Buffer.create 4 in
  Uutf.Buffer.add_utf_8 buff ch ;
  Buffer.contents buff

let rec normalized_add_uchar norm buffer value =
  match Uunf.add norm value with
  | `Uchar u ->
      Uutf.Buffer.add_utf_8 buffer u ;
      normalized_add_uchar norm buffer `Await
  | `Await -> ()
  | `End -> ()

let of_uchar_list lst =
  let buffer = Buffer.create 32 in
  let norm = Uunf.create `NFC in
  let rec helper = function
    | [] ->
        normalized_add_uchar norm buffer `End ;
        Buffer.contents buffer
    | x :: xs ->
        normalized_add_uchar norm buffer (`Uchar x) ;
        helper xs
  in
  helper lst

let of_string s =
  let buffer = Buffer.create (String.length s) in
  let norm = Uunf.create `NFC in
  let decoder =
    Uutf.decoder
      ~nln:(`NLF (Uchar.of_char '\n'))
      ~encoding:`UTF_8 (`String s)
  in
  let rec helper () =
    match Uutf.decode decoder with
    | `Uchar _ as u ->
        normalized_add_uchar norm buffer u ;
        helper ()
    | `End ->
        normalized_add_uchar norm buffer `End ;
        Buffer.contents buffer
    | `Malformed _ ->
        failwith
          "malformed utf-8 string passed to Nfc_string.of_string"
    | `Await -> assert false
  in
  helper ()

let of_string_unsafe s = s

let empty = ""

let equal = String.equal
