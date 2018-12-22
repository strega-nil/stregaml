type fixity = Nonfix : fixity | Infix : fixity | Prefix : fixity

type kind = Identifier : kind | Operator : kind

type t = Name : {string: Nfc_string.t; fixity: fixity; kind: kind} -> t

let string (Name {string; _}) = string

let fixity (Name {fixity; _}) = fixity

let kind (Name {kind; _}) = kind

let to_ident_string (Name {string; fixity; kind}) =
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

let equal lhs rhs =
  equal_fixity (fixity lhs) (fixity rhs)
  && equal_kind (kind lhs) (kind rhs)
  && Nfc_string.equal (string lhs) (string rhs)
