include module type of struct
    include Types.Error
end

val to_string : t -> ctxt:Types.Type_Context.t -> string
