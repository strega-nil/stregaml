open Cafec
module Keyword = Parse.Token.Keyword

module type Language = Parse.Language

module type Interface_Language = sig
  val numbers_are_big_endian : bool

  val base_16 : Nfc_string.t list
  val base_8 : Nfc_string.t list
  val base_2 : Nfc_string.t list

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

module Make_Language(L : Interface_Language) : Language = struct
  let numbers_are_big_endian = L.numbers_are_big_endian

  let number_base (s : Nfc_string.t) =
    let check_base lst =
      List.exists lst ~f:(fun s' -> Nfc_string.equal s s')
    in
    if check_base L.base_16 then Some 16
    else if check_base L.base_8 then Some 8
    else if check_base L.base_2 then Some 2
    else None

  let keyword_of_string s =
    if Nfc_string.equal s L.kw_True then Some Keyword.True
    else if Nfc_string.equal s L.kw_False then Some Keyword.False
    else if Nfc_string.equal s L.kw_Match then Some Keyword.Match
    else if Nfc_string.equal s L.kw_If then Some Keyword.If
    else if Nfc_string.equal s L.kw_Else then Some Keyword.Else
    else if Nfc_string.equal s L.kw_Infix then Some Keyword.Infix
    else if Nfc_string.equal s L.kw_Prefix then Some Keyword.Prefix
    else if Nfc_string.equal s L.kw_Group then Some Keyword.Group
    else if Nfc_string.equal s L.kw_Func then Some Keyword.Func
    else if Nfc_string.equal s L.kw_Type then Some Keyword.Type
    else if Nfc_string.equal s L.kw_Data then Some Keyword.Data
    else if Nfc_string.equal s L.kw_Record then Some Keyword.Record
    else if Nfc_string.equal s L.kw_Alias then Some Keyword.Alias
    else if Nfc_string.equal s L.kw_Let then Some Keyword.Let
    else if Nfc_string.equal s L.kw_Ref then Some Keyword.Ref
    else if Nfc_string.equal s L.kw_Mut then Some Keyword.Mut
    else if Nfc_string.equal s L.kw_Builtin then Some Keyword.Builtin
    else if Nfc_string.equal s L.kw_Underscore then Some Keyword.Underscore
    else if Nfc_string.equal s L.kw_Variant then Some Keyword.Variant
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

module English = Make_Language(
  struct
    let numbers_are_big_endian = true

    let base_16 = [
      Nfc_string.of_string "x";
      Nfc_string.of_string "X"]
    let base_8 =
      [ Nfc_string.of_string "o"
      ; Nfc_string.of_string "O" ]
    let base_2 =
      [ Nfc_string.of_string "b"
      ; Nfc_string.of_string "B" ]

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

module Yiddish = Make_Language(
  struct
    let numbers_are_big_endian = false

    let base_16 = [Nfc_string.of_string "ה"]
    let base_8 = [
      Nfc_string.of_string "א"; Nfc_string.of_string "אַ"]
    let base_2 = [Nfc_string.of_string "ב"]

    let kw_True = Nfc_string.of_string "אמת"
    let kw_False = Nfc_string.of_string "פֿאַלש"
    let kw_Match = Nfc_string.of_string "צוזוכ"
    let kw_If = Nfc_string.of_string "אױב"
    let kw_Else = Nfc_string.of_string "אַזיסט"
    let kw_Infix = Nfc_string.of_string "אינפֿיקס"
    let kw_Prefix = Nfc_string.of_string "פּריפֿיקס"
    let kw_Group = Nfc_string.of_string "גרופּ"
    let kw_Func = Nfc_string.of_string "פֿונק"
    let kw_Type = Nfc_string.of_string "סאָרט"
    let kw_Data = Nfc_string.of_string "דאַט" (* might also be געגעבענע *)
    let kw_Record = Nfc_string.of_string "דיסק"
    let kw_Variant = Nfc_string.of_string "גירסא" (* might also be װאַריאַנט *)
    let kw_Alias = Nfc_string.of_string "אַליאַס"
    let kw_Let = Nfc_string.of_string "לאָז"
    let kw_Ref = Nfc_string.of_string "רעף"
    let kw_Mut = Nfc_string.of_string "פֿאַר"
    let kw_Builtin = Nfc_string.of_string "__בילטין" (* unknown what to do for this *)
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
