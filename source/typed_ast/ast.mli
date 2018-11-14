module Stmt = Types.Ast_Stmt
module Binding = Types.Ast_Binding

module Expr : sig
  include module type of struct
      include Types.Ast_Expr
  end

  module Type = Types.Ast_Expr_Type

  val base_type : t -> Types.Type.t

  val base_type_sp : t Spanned.t -> Types.Type.t

  val full_type : t -> Type.t

  val full_type_sp : t Spanned.t -> Type.t

  module Local = Types.Ast_Expr_Local
  module Builtin = Types.Ast_Expr_Builtin
end
