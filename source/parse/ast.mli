module Spanned = Cafec_containers.Spanned

module Type : sig
  include module type of struct
      include Types.Ast_Type
  end

  val to_string : t -> string

  module Data : sig
    include module type of struct
        include Types.Ast_Type_Data
    end

    val to_string : t -> string
  end

  module Definition = Types.Ast_Type_Definition
end

module Stmt : sig
  include module type of struct
      include Types.Ast_Stmt
  end

  val to_string : t -> indent:int -> string
end

module Expr : sig
  include module type of struct
      include Types.Ast_Expr
  end

  val to_string : t -> indent:int -> string

  val block_to_string : block -> indent:int -> string
end

module Func : sig
  include module type of struct
      include Types.Ast_Func
  end

  val to_string : t -> string
end

module Association : sig
  include module type of struct
      include Types.Ast_Association
  end

  val to_string : t -> string
end

include module type of struct
    include Types.Ast
end

val to_string : t -> string
