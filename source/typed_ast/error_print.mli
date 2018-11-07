include module type of struct
    include Types.Error
end

val to_string : t -> ctxt:Type.Context.t -> string
