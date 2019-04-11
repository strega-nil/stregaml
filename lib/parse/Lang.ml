module type Language = Types.Language

type t = (module Language)

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

let builtin_name_of_string s ~lang =
  let module Lang = (val lang : Language) in
  Lang.builtin_name_of_string s

let builtin_name_to_string kw ~lang =
  let module Lang = (val lang : Language) in
  Lang.builtin_name_to_string kw

let attribute_of_string s ~lang =
  let module Lang = (val lang : Language) in
  Lang.attribute_of_string s

let attribute_to_string kw ~lang =
  let module Lang = (val lang : Language) in
  Lang.attribute_to_string kw
