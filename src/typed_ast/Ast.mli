module Stmt = Types.Ast_Stmt

module Binding : sig
  include module type of struct
      include Types.Ast_Binding
  end

  val name : t -> Name.t Spanned.t

  val mutability : t -> Type.mutability

  val ty : t -> Type.t
end

module Expr : sig
  module Builtin = Types.Ast_Expr_Builtin

  module Type : sig
    include module type of struct
        include Types.Ast_Expr_Type
    end

    val category : t -> category

    val ty : t -> Type.t
  end

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

    val stmts : t -> Types.Ast_Stmt.t Spanned.t list

    val base_type : t -> Types.Type.t

    val base_type_sp : t Spanned.t -> Types.Type.t

    val full_type : t -> Type.t

    val full_type_sp : t Spanned.t -> Type.t
  end

  include module type of struct
      include Types.Ast_Expr
  end

  val base_type : t -> Types.Type.t

  val base_type_sp : t Spanned.t -> Types.Type.t

  val full_type : t -> Type.t

  val full_type_sp : t Spanned.t -> Type.t

  val unit_value : t
end
