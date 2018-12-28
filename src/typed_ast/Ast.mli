module Stmt = Types.Ast_Stmt

module Binding : sig
  include module type of struct
      include Types.Ast_Binding
  end

  val name : t -> Name.anyfix Name.t Spanned.t

  val is_mut : t -> bool

  val ty : t -> Type.any Type.t
end

module Expr : sig
  module Builtin = Types.Ast_Expr_Builtin

  module Local : sig
    include module type of struct
        include Types.Ast_Expr_Local
    end

    val binding : t -> Binding.t
  end

  module Block : sig
    include module type of struct
        include Types.Ast_Expr_Block
    end

    val expr : t -> Types.Ast_Expr.t Spanned.t option

    val stmts : t -> Types.Ast_Stmt.t Spanned.t Array.t

    val base_type : t -> Type.value Type.t

    val base_type_sp : t Spanned.t -> Type.value Type.t

    val full_type : t -> Type.any Type.t

    val full_type_sp : t Spanned.t -> Type.any Type.t
  end

  include module type of struct
      include Types.Ast_Expr
  end

  val base_type : t -> Type.value Type.t

  val base_type_sp : t Spanned.t -> Type.value Type.t

  val full_type : t -> Type.any Type.t

  val full_type_sp : t Spanned.t -> Type.any Type.t

  val unit_value : t
end
