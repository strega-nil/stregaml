open Cafec
module Attribute = Parse.Token.Attribute
module Keyword = Parse.Token.Keyword

module type Language = Parse.Language

type contextual_keywords =
  | Contextual_keywords :
      { associativity : Nfc_string.t
      ; precedence : Nfc_string.t
      ; start : Nfc_string.t
      ; end_ : Nfc_string.t
      ; none : Nfc_string.t
      ; bits : Nfc_string.t }
      -> contextual_keywords

type keywords =
  | Keywords :
      { match_ : Nfc_string.t
      ; if_ : Nfc_string.t
      ; else_ : Nfc_string.t
      ; infix : Nfc_string.t
      ; prefix : Nfc_string.t
      ; group : Nfc_string.t
      ; func : Nfc_string.t
      ; type_ : Nfc_string.t
      ; data : Nfc_string.t
      ; record : Nfc_string.t
      ; variant : Nfc_string.t
      ; integer : Nfc_string.t
      ; alias : Nfc_string.t
      ; let_ : Nfc_string.t
      ; ref : Nfc_string.t
      ; mut : Nfc_string.t
      ; builtin : Nfc_string.t
      ; underscore : Nfc_string.t }
      -> keywords

type attributes =
  | Attributes : {entrypoint : Nfc_string.t} -> attributes

module type Interface_Language = sig
  val contextual_keywords : contextual_keywords

  val keywords : keywords

  val attributes : attributes
end

module Make_Language (L : Interface_Language) : Language = struct
  let contextual_keyword_of_string s =
    let (Contextual_keywords r) = L.contextual_keywords in
    if Nfc_string.equal s r.associativity
    then Some Keyword.Contextual.Associativity
    else if Nfc_string.equal s r.precedence
    then Some Keyword.Contextual.Precedence
    else if Nfc_string.equal s r.start
    then Some Keyword.Contextual.Start
    else if Nfc_string.equal s r.end_
    then Some Keyword.Contextual.End
    else if Nfc_string.equal s r.none
    then Some Keyword.Contextual.None
    else if Nfc_string.equal s r.bits
    then Some Keyword.Contextual.Bits
    else None

  let contextual_keyword_to_string c =
    let (Contextual_keywords r) = L.contextual_keywords in
    let s =
      match c with
      | Keyword.Contextual.Associativity -> r.associativity
      | Keyword.Contextual.Precedence -> r.precedence
      | Keyword.Contextual.Start -> r.start
      | Keyword.Contextual.End -> r.end_
      | Keyword.Contextual.None -> r.none
      | Keyword.Contextual.Bits -> r.bits
    in
    (s :> string)

  let keyword_of_string s =
    let (Keywords r) = L.keywords in
    if Nfc_string.equal s r.match_
    then Some Keyword.Match
    else if Nfc_string.equal s r.if_
    then Some Keyword.If
    else if Nfc_string.equal s r.else_
    then Some Keyword.Else
    else if Nfc_string.equal s r.infix
    then Some Keyword.Infix
    else if Nfc_string.equal s r.prefix
    then Some Keyword.Prefix
    else if Nfc_string.equal s r.group
    then Some Keyword.Group
    else if Nfc_string.equal s r.func
    then Some Keyword.Func
    else if Nfc_string.equal s r.type_
    then Some Keyword.Type
    else if Nfc_string.equal s r.data
    then Some Keyword.Data
    else if Nfc_string.equal s r.record
    then Some Keyword.Record
    else if Nfc_string.equal s r.variant
    then Some Keyword.Variant
    else if Nfc_string.equal s r.integer
    then Some Keyword.Integer
    else if Nfc_string.equal s r.alias
    then Some Keyword.Alias
    else if Nfc_string.equal s r.let_
    then Some Keyword.Let
    else if Nfc_string.equal s r.ref
    then Some Keyword.Ref
    else if Nfc_string.equal s r.mut
    then Some Keyword.Mut
    else if Nfc_string.equal s r.builtin
    then Some Keyword.Builtin
    else if Nfc_string.equal s r.underscore
    then Some Keyword.Underscore
    else None

  let keyword_to_string k =
    let (Keywords r) = L.keywords in
    match k with
    | Keyword.Match -> (r.match_ :> string)
    | Keyword.If -> (r.if_ :> string)
    | Keyword.Else -> (r.else_ :> string)
    | Keyword.Infix -> (r.infix :> string)
    | Keyword.Prefix -> (r.prefix :> string)
    | Keyword.Group -> (r.group :> string)
    | Keyword.Func -> (r.func :> string)
    | Keyword.Type -> (r.type_ :> string)
    | Keyword.Data -> (r.data :> string)
    | Keyword.Record -> (r.record :> string)
    | Keyword.Variant -> (r.variant :> string)
    | Keyword.Integer -> (r.integer :> string)
    | Keyword.Alias -> (r.alias :> string)
    | Keyword.Let -> (r.let_ :> string)
    | Keyword.Ref -> (r.ref :> string)
    | Keyword.Mut -> (r.mut :> string)
    | Keyword.Builtin -> (r.builtin :> string)
    | Keyword.Underscore -> (r.underscore :> string)

  let attribute_of_string s =
    let (Attributes r) = L.attributes in
    if Nfc_string.equal s r.entrypoint
    then Some Attribute.Entrypoint
    else None

  let attribute_to_string att =
    let (Attributes r) = L.attributes in
    match att with Attribute.Entrypoint -> (r.entrypoint :> string)
end

module English = Make_Language (struct
  let contextual_keywords =
    Contextual_keywords
      { associativity = Nfc_string.of_string "associativity"
      ; precedence = Nfc_string.of_string "precedence"
      ; start = Nfc_string.of_string "start"
      ; end_ = Nfc_string.of_string "end"
      ; none = Nfc_string.of_string "none"
      ; bits = Nfc_string.of_string "bits" }

  let keywords =
    Keywords
      { match_ = Nfc_string.of_string "match"
      ; if_ = Nfc_string.of_string "if"
      ; else_ = Nfc_string.of_string "else"
      ; infix = Nfc_string.of_string "infix"
      ; prefix = Nfc_string.of_string "prefix"
      ; group = Nfc_string.of_string "group"
      ; func = Nfc_string.of_string "func"
      ; type_ = Nfc_string.of_string "type"
      ; data = Nfc_string.of_string "data"
      ; record = Nfc_string.of_string "record"
      ; variant = Nfc_string.of_string "variant"
      ; integer = Nfc_string.of_string "integer"
      ; alias = Nfc_string.of_string "alias"
      ; let_ = Nfc_string.of_string "let"
      ; ref = Nfc_string.of_string "ref"
      ; mut = Nfc_string.of_string "mut"
      ; builtin = Nfc_string.of_string "__builtin"
      ; underscore = Nfc_string.of_string "_" }

  let attributes =
    Attributes {entrypoint = Nfc_string.of_string "entrypoint"}
end)

module Yiddish = Make_Language (struct
  let contextual_keywords =
    Contextual_keywords
      { associativity = Nfc_string.of_string "קאָמפּאַניר"
      ; precedence = Nfc_string.of_string "בכורה"
      ; start = Nfc_string.of_string "סטאַרט"
      ; end_ = Nfc_string.of_string "ענד"
      ; none = Nfc_string.of_string "קײן"
      ; bits = Nfc_string.of_string "ביטס" }

  let keywords =
    Keywords
      { match_ = Nfc_string.of_string "צוזוך"
      ; if_ = Nfc_string.of_string "אױב"
      ; else_ = Nfc_string.of_string "אַזיסט"
      ; infix = Nfc_string.of_string "אינפֿיקס"
      ; prefix = Nfc_string.of_string "פּריפֿיקס"
      ; group = Nfc_string.of_string "גרופּע"
      ; func = Nfc_string.of_string "פֿונק"
      ; type_ = Nfc_string.of_string "סאָרט"
      ; data = Nfc_string.of_string "דאַט"
      ; (* might also be געגעבענע *)
        record = Nfc_string.of_string "דיסק"
      ; variant = Nfc_string.of_string "גירסא"
      ; integer = Nfc_string.of_string "גאַנץ"
      ; (* might also be װאַריאַנט *)
        alias = Nfc_string.of_string "אַליאַס"
      ; let_ = Nfc_string.of_string "לאָז"
      ; ref = Nfc_string.of_string "רעף"
      ; mut = Nfc_string.of_string "פֿאַר"
      ; builtin = Nfc_string.of_string "__בילטין"
      ; underscore = Nfc_string.of_string "_" }

  let attributes =
    Attributes {entrypoint = Nfc_string.of_string "אײַנגאַנג"}
end)

let get_lang s =
  match s with
  | "english" -> Ok (module English : Language)
  | "yiddish" | "ייִדיש" -> Ok (module Yiddish : Language)
  | s ->
      Error
        (String.concat
           [ "Unknown language: "
           ; s
           ; ". Known languages are:\n"
           ; "  - english\n"
           ; "  - yiddish | ייִדיש\n" ])
