module Attribute : sig
  include module type of struct include Types.Ast_Attribute end

  val to_string : t -> string
end

module Stmt : sig
  include module type of struct include Types.Ast_Stmt end

  val to_string : t -> indent:int -> string
end

module Expr : sig
  include module type of struct include Types.Ast_Expr end

  module Block : sig
    include module type of struct include Types.Ast_Expr_Block end

    val to_string : t -> indent:int -> string

    val stmts : t -> Types.Ast_Stmt.t Spanned.t list

    val expr : t -> Types.Ast_Expr.t Spanned.t option

    val with_stmt : t -> Types.Ast_Stmt.t Spanned.t -> t
  end

  val to_string : t -> indent:int -> string

  val qualified_path : _ qualified -> Nfc_string.t Spanned.t list

  val qualified_name : 'f qualified -> 'f Name.t Spanned.t

  val pattern_constructor : pattern -> Name.nonfix qualified Spanned.t

  val pattern_binding : pattern -> Name.anyfix Name.t Spanned.t
end

module Func : sig
  include module type of struct include Types.Ast_Func end

  val to_string : t -> string

  val name : t -> Name.anyfix Name.t

  val params : t -> params

  val ret_ty : t -> Type.any Type.t Spanned.t option

  val body : t -> Expr.Block.t Spanned.t

  val attributes : t -> Attribute.t Spanned.t list
end

module Infix_group : sig
  include module type of struct include Types.Ast_Infix_group end

  val to_string : t -> string
end

module Infix_declaration : sig
  include module type of struct include Types.Ast_Infix_declaration end

  val to_string : t -> string
end

include module type of struct include Types.Ast end

val to_string : t -> string

val funcs : t -> Func.t Spanned.t list

val infix_decls : t -> Infix_declaration.t Spanned.t list

val infix_groups : t -> Infix_group.t Spanned.t list

val types : t -> Type.Definition.t Spanned.t list

val with_func : t -> Func.t Spanned.t -> t

val with_infix_decl : t -> Infix_declaration.t Spanned.t -> t

val with_infix_group : t -> Infix_group.t Spanned.t -> t

val with_type : t -> Type.Definition.t Spanned.t -> t
