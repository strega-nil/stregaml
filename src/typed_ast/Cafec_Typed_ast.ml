module Error = Error_print
module Binding = Ast.Binding
module Expr = Ast.Expr
module Stmt = Ast.Stmt
module Type = Type
module Internal = Internal
module Function_declaration = Internal.Function_declaration

type t = Internal.t

let make unt_ast = Internal.make unt_ast

let number_of_functions ast =
  List.length (Internal.function_context ast)

let function_seq ast =
  Sequence.zip
    (Sequence.of_list (Internal.function_context ast))
    (Sequence.of_list (Internal.function_definitions ast))
