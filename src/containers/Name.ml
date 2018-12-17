type fixity = Normal | Prefix

type kind = Identifier | Operator

type t = {string: Nfc_string.t; fixity: fixity; kind: kind}

let to_ident_string {string; fixity; kind} =
  match (fixity, kind) with
  | Normal, Identifier -> (string :> string)
  | Normal, Operator -> String.concat ["("; (string :> string); ")"]
  | Prefix, Identifier -> String.concat ["(prefix \\"; (string :> string); ")"]
  | Prefix, Operator -> String.concat ["(prefix "; (string :> string); ")"]

let equal_fixity lf rf =
  match (lf, rf) with
  | Normal, Normal -> true
  | Prefix, Prefix -> true
  | _ -> false

let equal_kind lk rk =
  match (lk, rk) with
  | Identifier, Identifier -> true
  | Operator, Operator -> true
  | _ -> false

let equal {string= ls; kind= lk; fixity= lf} {string= rs; kind= rk; fixity= rf}
    =
  equal_fixity lf rf && equal_kind lk rk && Nfc_string.equal ls rs
