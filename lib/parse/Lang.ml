module type Language = Types.Language

let contextual_keyword_of_string s ~lang =
  let module Lang = (val lang : Language) in
  Lang.contextual_keyword_of_string s

let contextual_keyword_to_string c ~lang =
  let module Lang = (val lang : Language) in
  Lang.contextual_keyword_to_string c

let keyword_of_string s ~lang =
  let module Lang = (val lang : Language) in
  Lang.keyword_of_string s

let keyword_to_string kw ~lang =
  let module Lang = (val lang : Language) in
  Lang.keyword_to_string kw
