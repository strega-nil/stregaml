module Stmt = Types.Ast_stmt
module Binding = Types.Binding
module Value_type = Types.Value_type

module Expr : sig
  include module type of struct
      include Types.Ast_expr
  end

  val base_type : t -> Types.Type.t

  val base_type_sp : t Spanned.t -> Types.Type.t

  val value_type : t -> Value_type.t

  val value_type_sp : t Spanned.t -> Value_type.t

  module Local = Types.Ast_expr_local

  module Builtin : sig
    include module type of struct
        include Types.Ast_expr_builtin
    end

    val equal : t -> t -> bool
  end
end
