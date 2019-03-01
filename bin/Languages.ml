open Cafec
module Keyword = Parse.Token.Keyword

module type Language = Parse.Language

module type Interface_Language = sig
  val ckw_Associativity : Nfc_string.t

  val ckw_Precedence : Nfc_string.t

  val ckw_Start : Nfc_string.t

  val ckw_End : Nfc_string.t

  val ckw_None : Nfc_string.t

  val kw_True : Nfc_string.t

  val kw_False : Nfc_string.t

  val kw_Match : Nfc_string.t

  val kw_If : Nfc_string.t

  val kw_Else : Nfc_string.t

  val kw_Infix : Nfc_string.t

  val kw_Prefix : Nfc_string.t

  val kw_Group : Nfc_string.t

  val kw_Func : Nfc_string.t

  val kw_Type : Nfc_string.t

  val kw_Data : Nfc_string.t

  val kw_Record : Nfc_string.t

  val kw_Alias : Nfc_string.t

  val kw_Let : Nfc_string.t

  val kw_Ref : Nfc_string.t

  val kw_Mut : Nfc_string.t

  val kw_Builtin : Nfc_string.t

  val kw_Underscore : Nfc_string.t

  val kw_Variant : Nfc_string.t
end

module Make_Language (L : Interface_Language) : Language = struct
  let contextual_keyword_of_string s =
    if Nfc_string.equal s L.ckw_Associativity
    then Some Keyword.Contextual.Associativity
    else if Nfc_string.equal s L.ckw_Precedence
    then Some Keyword.Contextual.Precedence
    else if Nfc_string.equal s L.ckw_Start
    then Some Keyword.Contextual.Start
    else if Nfc_string.equal s L.ckw_End
    then Some Keyword.Contextual.End
    else if Nfc_string.equal s L.ckw_None
    then Some Keyword.Contextual.None
    else None

  let contextual_keyword_to_string c =
    match c with
    | Keyword.Contextual.Associativity -> L.ckw_Associativity
    | Keyword.Contextual.Precedence -> L.ckw_Precedence
    | Keyword.Contextual.Start -> L.ckw_Start
    | Keyword.Contextual.End -> L.ckw_End
    | Keyword.Contextual.None -> L.ckw_None

  let keyword_of_string s =
    if Nfc_string.equal s L.kw_True
    then Some Keyword.True
    else if Nfc_string.equal s L.kw_False
    then Some Keyword.False
    else if Nfc_string.equal s L.kw_Match
    then Some Keyword.Match
    else if Nfc_string.equal s L.kw_If
    then Some Keyword.If
    else if Nfc_string.equal s L.kw_Else
    then Some Keyword.Else
    else if Nfc_string.equal s L.kw_Infix
    then Some Keyword.Infix
    else if Nfc_string.equal s L.kw_Prefix
    then Some Keyword.Prefix
    else if Nfc_string.equal s L.kw_Group
    then Some Keyword.Group
    else if Nfc_string.equal s L.kw_Func
    then Some Keyword.Func
    else if Nfc_string.equal s L.kw_Type
    then Some Keyword.Type
    else if Nfc_string.equal s L.kw_Data
    then Some Keyword.Data
    else if Nfc_string.equal s L.kw_Record
    then Some Keyword.Record
    else if Nfc_string.equal s L.kw_Alias
    then Some Keyword.Alias
    else if Nfc_string.equal s L.kw_Let
    then Some Keyword.Let
    else if Nfc_string.equal s L.kw_Ref
    then Some Keyword.Ref
    else if Nfc_string.equal s L.kw_Mut
    then Some Keyword.Mut
    else if Nfc_string.equal s L.kw_Builtin
    then Some Keyword.Builtin
    else if Nfc_string.equal s L.kw_Underscore
    then Some Keyword.Underscore
    else if Nfc_string.equal s L.kw_Variant
    then Some Keyword.Variant
    else None

  let keyword_to_string k =
    match k with
    | Keyword.True -> (L.kw_True :> string)
    | Keyword.False -> (L.kw_False :> string)
    | Keyword.Match -> (L.kw_Match :> string)
    | Keyword.If -> (L.kw_If :> string)
    | Keyword.Else -> (L.kw_Else :> string)
    | Keyword.Infix -> (L.kw_Infix :> string)
    | Keyword.Prefix -> (L.kw_Prefix :> string)
    | Keyword.Group -> (L.kw_Group :> string)
    | Keyword.Func -> (L.kw_Func :> string)
    | Keyword.Type -> (L.kw_Type :> string)
    | Keyword.Data -> (L.kw_Data :> string)
    | Keyword.Record -> (L.kw_Record :> string)
    | Keyword.Alias -> (L.kw_Alias :> string)
    | Keyword.Let -> (L.kw_Let :> string)
    | Keyword.Ref -> (L.kw_Ref :> string)
    | Keyword.Mut -> (L.kw_Mut :> string)
    | Keyword.Builtin -> (L.kw_Builtin :> string)
    | Keyword.Underscore -> (L.kw_Underscore :> string)
    | Keyword.Variant -> (L.kw_Variant :> string)
end

module English = Make_Language (struct
  let ckw_Associativity = Nfc_string.of_string "associativity"

  let ckw_Precedence = Nfc_string.of_string "precedence"

  let ckw_Start = Nfc_string.of_string "start"

  let ckw_End = Nfc_string.of_string "end"

  let ckw_None = Nfc_string.of_string "none"

  let kw_True = Nfc_string.of_string "true"

  let kw_False = Nfc_string.of_string "false"

  let kw_Match = Nfc_string.of_string "match"

  let kw_If = Nfc_string.of_string "if"

  let kw_Else = Nfc_string.of_string "else"

  let kw_Infix = Nfc_string.of_string "infix"

  let kw_Prefix = Nfc_string.of_string "prefix"

  let kw_Group = Nfc_string.of_string "group"

  let kw_Func = Nfc_string.of_string "func"

  let kw_Type = Nfc_string.of_string "type"

  let kw_Data = Nfc_string.of_string "data"

  let kw_Record = Nfc_string.of_string "record"

  let kw_Alias = Nfc_string.of_string "alias"

  let kw_Let = Nfc_string.of_string "let"

  let kw_Ref = Nfc_string.of_string "ref"

  let kw_Mut = Nfc_string.of_string "mut"

  let kw_Builtin = Nfc_string.of_string "__builtin"

  let kw_Underscore = Nfc_string.of_string "_"

  let kw_Variant = Nfc_string.of_string "variant"
end)

module Yiddish = Make_Language (struct
  let ckw_Associativity = Nfc_string.of_string "קאָמפּאַניר"

  let ckw_Precedence = Nfc_string.of_string "בכורה"

  let ckw_Start = Nfc_string.of_string "סטאַרט"

  let ckw_End = Nfc_string.of_string "ענד"

  let ckw_None = Nfc_string.of_string "קײן"

  let kw_True = Nfc_string.of_string "אמת"

  let kw_False = Nfc_string.of_string "פֿאַלש"

  let kw_Match = Nfc_string.of_string "צוזוך"

  let kw_If = Nfc_string.of_string "אױב"

  let kw_Else = Nfc_string.of_string "אַזיסט"

  let kw_Infix = Nfc_string.of_string "אינפֿיקס"

  let kw_Prefix = Nfc_string.of_string "פּריפֿיקס"

  let kw_Group = Nfc_string.of_string "גרופּע"

  let kw_Func = Nfc_string.of_string "מאַפּע"

  let kw_Type = Nfc_string.of_string "סאָרט"

  let kw_Data = Nfc_string.of_string "דאַט"

  (* might also be געגעבענע *)
  let kw_Record = Nfc_string.of_string "דיסק"

  let kw_Variant = Nfc_string.of_string "גירסא"

  (* might also be װאַריאַנט *)
  let kw_Alias = Nfc_string.of_string "אַליאַס"

  let kw_Let = Nfc_string.of_string "לאָז"

  let kw_Ref = Nfc_string.of_string "רעף"

  let kw_Mut = Nfc_string.of_string "פֿאַר"

  let kw_Builtin = Nfc_string.of_string "__בילטין"

  (* unknown what to do for this *)
  let kw_Underscore = Nfc_string.of_string "_"
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
