type kind =
  | Identifier
  | Infix
  | Prefix

type t = {string: Nfc_string.t; kind: kind}

let to_string {string; kind} =
  match kind with
  | Identifier -> (string :> string)
  | Infix -> String.concat ["("; (string :> string); ")"]
  | Prefix -> String.concat ["(prefix "; (string :> string); ")"]


let equal {string= ls; kind= lk} {string= rs; kind= rk} =
  let equal_kind =
    match lk, rk with
    | Identifier, Identifier -> true
    | Infix, Infix -> true
    | Prefix, Prefix -> true
    | _ -> false
  in
  equal_kind && Nfc_string.equal ls rs

