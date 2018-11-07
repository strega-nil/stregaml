module Stmt = Types.Ast_Stmt
module Binding = Types.Ast_Binding

module Expr = struct
  include Types.Ast_Expr
  module Type = Types.Ast_Expr_Type

  let base_type {ty= {Type.ty; _}; _} = ty

  let base_type_sp ({ty= {Type.ty; _}; _}, _) = ty

  let full_type {ty; _} = ty

  let full_type_sp ({ty; _}, _) = ty

  module Local = Types.Ast_Expr_Local

  module Builtin = struct
    include Types.Ast_Expr_Builtin

    let equal lhs rhs =
      match (lhs, rhs) with
      | Less_eq, Less_eq -> true
      | Add, Add -> true
      | Sub, Sub -> true
      | Mul, Mul -> true
      | _ -> false
  end
end
