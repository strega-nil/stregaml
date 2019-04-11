module Stmt = Types.Ast_Stmt

module Binding = struct
  include Types.Ast_Binding

  let name (Binding r) = r.name

  let is_mut (Binding r) = r.is_mut

  let ty (Binding r) = r.ty
end

module Expr = struct
  include Types.Ast_Expr

  module Builtin = struct
    include Types.Ast_Expr_Builtin

    let to_string b ~lang =
      let module Token = Cafec_Parse.Token.Builtin_name in
      let b =
        match b with
        | Less_eq -> Token.Less_eq
        | Add -> Token.Add
        | Sub -> Token.Sub
        | Mul -> Token.Mul
      in
      Token.to_string b ~lang
  end

  module Local = struct
    include Types.Ast_Expr_Local

    let binding (Types.Ast_Expr_Local.Local r) = r.binding
  end

  let full_type (Expr {ty; _}) = ty

  let full_type_sp (Expr {ty; _}, _) = ty

  let base_type e = Type.value_type (full_type e)

  let base_type_sp e = Type.value_type (full_type_sp e)

  module Block = struct
    include Types.Ast_Expr_Block

    let expr (Types.Ast_Expr_Block.Block r) = r.expr

    let stmts (Types.Ast_Expr_Block.Block r) = r.stmts

    let base_type blk =
      match expr blk with
      | Some expr -> base_type_sp expr
      | None -> Type.unit

    let base_type_sp (blk, _) = base_type blk

    let full_type blk =
      match expr blk with
      | Some expr -> full_type_sp expr
      | None -> Type.Any Type.unit

    let full_type_sp (blk, _) = full_type blk
  end

  let unit_value =
    Expr {variant = Tuple_literal Array.empty; ty = Type.Any Type.unit}
end
