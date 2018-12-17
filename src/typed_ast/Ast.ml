module Stmt = Types.Ast_Stmt
module Binding = Types.Ast_Binding

module Expr = struct
  include Types.Ast_Expr
  module Type = Types.Ast_Expr_Type

  let base_type {ty= {Type.ty; _}; _} = ty

  let base_type_sp ({ty= {Type.ty; _}; _}, _) = ty

  let full_type {ty; _} = ty

  let full_type_sp ({ty; _}, _) = ty

  let block_base_type = function
    | {expr= Some expr; _} -> base_type_sp expr
    | {expr= None; _} -> Types.Type.Builtin Types.Type.Unit

  let block_base_type_sp (blk, _) = block_base_type blk

  let block_full_type = function
    | {expr= Some expr; _} -> full_type_sp expr
    | {expr= None; _} ->
        {Type.category= Type.Value; Type.ty= Types.Type.Builtin Types.Type.Unit}

  let block_full_type_sp (blk, _) = block_full_type blk

  let unit_value =
    { variant= Unit_literal
    ; ty=
        {Type.ty= Types.Type.Builtin Types.Type.Unit; Type.category= Type.Value}
    }

  module Local = Types.Ast_Expr_Local
  module Builtin = Types.Ast_Expr_Builtin
end
