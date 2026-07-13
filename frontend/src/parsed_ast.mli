open Ast_types

type identifier = Variable of Var_name.t

type expr =
    | Integer of loc * int
    | Identifier of loc * identifier
    | Let of loc * Var_name.t * expr
    | Assign of loc * identifier * expr
    | BinOp of loc * bin_op * expr * expr
    | Printf of loc * string * expr list
and block_expr = Block of loc * expr list

type program = Prog of block_expr

