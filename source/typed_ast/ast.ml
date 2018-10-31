module Stmt = Types.Ast_stmt

module Expr = struct
  include Types.Ast_expr

  module Builtin = struct
    include Types.Ast_expr_builtin

    let equal lhs rhs =
      match (lhs, rhs) with
      | Less_eq, Less_eq -> true
      | Add, Add -> true
      | Sub, Sub -> true
      | Mul, Mul -> true
      | _ -> false
  end
end
