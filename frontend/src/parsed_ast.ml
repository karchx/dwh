open Ast_types

type expr =
    | Integer of loc * int
    | Let of loc * type_expr option * Var_name.t * expr
    | Assign of loc * identifer * expr
    | BinOp of loc * bin_op * expr * expr
and block_expr = Block of loc * expr list

type program = Prog of block_expr

