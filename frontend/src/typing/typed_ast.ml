open Ast.Ast_types

type identifier =
    | Variable of type_expr * Var_name.t
