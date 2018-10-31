module Stmt = Types.Ast_stmt

module Expr : sig
  include module type of struct
      include Types.Ast_expr
  end

  module Builtin : sig
    include module type of struct
        include Types.Ast_expr_builtin
    end

    val equal : t -> t -> bool
  end
end
