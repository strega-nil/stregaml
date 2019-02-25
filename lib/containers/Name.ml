module Fixity = struct
  type anyfix = Any_fixity

  type nonfix = Nonfix_fixity

  type infix = Infix_fixity

  type prefix = Prefix_fixity

  type 'f t =
    | Nonfix : nonfix t
    | Infix : infix t
    | Prefix : prefix t
    | Anyfix : _ t -> anyfix t

  let erase (type f) (fix : f t) : anyfix t =
    match fix with Anyfix _ -> fix | _ -> Anyfix fix
end

type anyfix = Fixity.anyfix

type nonfix = Fixity.nonfix

type infix = Fixity.infix

type prefix = Fixity.prefix

type 'f fixity = 'f Fixity.t =
  | Nonfix : nonfix fixity
  | Infix : infix fixity
  | Prefix : prefix fixity
  | Anyfix : _ fixity -> anyfix fixity

type kind =
  | Identifier : kind
  | Operator : kind

type _ t =
  | Name :
      { string : Nfc_string.t
      ; fixity : 'f fixity
      ; kind : kind }
      -> 'f t

let erase (type f) (Name {string; fixity; kind} : f t) : anyfix t =
  Name {string; kind; fixity = Fixity.erase fixity}

let rec nonfix : type f. f t -> nonfix t option = function
  | Name {string; fixity = Anyfix fixity; kind} ->
      nonfix (Name {string; fixity; kind})
  | Name {fixity = Nonfix; _} as n -> Some n
  | _ -> None

let string (Name {string; _}) = string

let fixity (Name {fixity; _}) = fixity

let kind (Name {kind; _}) = kind

let rec to_ident_string : type f. f t -> string =
 fun (Name {string; fixity; kind}) ->
  match (fixity, kind) with
  | Anyfix fixity, kind ->
      to_ident_string (Name {string; fixity; kind})
  | Nonfix, Identifier -> (string :> string)
  | Nonfix, Operator -> String.concat ["("; (string :> string); ")"]
  | Infix, Identifier ->
      String.concat ["(infix \\"; (string :> string); ")"]
  | Infix, Operator ->
      String.concat ["(infix "; (string :> string); ")"]
  | Prefix, Identifier ->
      String.concat ["(prefix \\"; (string :> string); ")"]
  | Prefix, Operator ->
      String.concat ["(prefix "; (string :> string); ")"]

let rec equal_fixity : type f1 f2. f1 fixity -> f2 fixity -> bool =
 fun lf rf ->
  match (lf, rf) with
  | Nonfix, Nonfix -> true
  | Infix, Infix -> true
  | Prefix, Prefix -> true
  | Anyfix lf, Anyfix rf -> equal_fixity lf rf
  | Anyfix lf, rf -> equal_fixity lf rf
  | lf, Anyfix rf -> equal_fixity lf rf
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
