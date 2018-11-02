module Spanned = Cafec_containers.Spanned

module Type : sig
  include module type of struct
      include Types.Ast_type
  end

  val to_string : t -> string

  module Data : sig
    include module type of struct
        include Types.Ast_type_data
    end

    val to_string : t -> string
  end

  module Definition = Types.Ast_type_definition
end

module Stmt : sig
  include module type of struct
      include Types.Ast_stmt
  end

  val to_string : t -> indent:int -> string
end

module Expr : sig
  include module type of struct
      include Types.Ast_expr
  end

  val to_string : t -> indent:int -> string

  val block_to_string : block -> indent:int -> string
end

module Func : sig
  include module type of struct
      include Types.Ast_func
  end

  val to_string : t -> string
end

include module type of struct
    include Types.Ast
end

val to_string : t -> string
