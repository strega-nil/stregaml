module type Language = Types.Language

let numbers_are_big_endian ~lang =
  let module Lang = (val lang : Language) in
  Lang.numbers_are_big_endian

let number_base s ~lang =
  let module Lang = (val lang : Language) in
  Lang.number_base s

let contextual_keyword_of_string s ~lang =
  let module Lang = (val lang : Language) in
  Lang.contextual_keyword_of_string s

let keyword_of_string s ~lang =
  let module Lang = (val lang : Language) in
  Lang.keyword_of_string s

let keyword_to_string kw ~lang =
  let module Lang = (val lang : Language) in
  Lang.keyword_to_string kw
