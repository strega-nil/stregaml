type fixity = Nonfix | Infix | Prefix

type kind = Identifier | Operator

type t = {string: Nfc_string.t; fixity: fixity; kind: kind}

let to_ident_string {string; fixity; kind} =
  match (fixity, kind) with
  | Nonfix, Identifier -> (string :> string)
  | Nonfix, Operator -> String.concat ["("; (string :> string); ")"]
  | Infix, Identifier -> String.concat ["(infix \\"; (string :> string); ")"]
  | Infix, Operator -> String.concat ["(infix "; (string :> string); ")"]
  | Prefix, Identifier -> String.concat ["(prefix \\"; (string :> string); ")"]
  | Prefix, Operator -> String.concat ["(prefix "; (string :> string); ")"]

let equal_fixity lf rf =
  match (lf, rf) with
  | Nonfix, Nonfix -> true
  | Infix, Infix -> true
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
