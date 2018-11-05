module Stmt = Types.Ast_stmt
module Binding = Types.Binding
module Value_type = Types.Value_type

module Expr = struct
  include Types.Ast_expr

  let base_type {ty= {Value_type.ty; _}; _} = ty

  let base_type_sp ({ty= {Value_type.ty; _}; _}, _) = ty

  let value_type {ty; _} = ty

  let value_type_sp ({ty; _}, _) = ty

  module Local = Types.Ast_expr_local

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
