open Ast_types

type identifier = Variable of Var_name.t

type expr =
    | Integer of loc * int
    | StringLit of loc * string
    | Identifier of loc * identifier
    | Let of loc * Var_name.t * expr
    | Task of loc * Task_name.t * Var_name.t list * task_prop list
    | Assign of loc * identifier * expr
    | If of loc * expr * block_expr * block_expr
    | BinOp of loc * bin_op * expr * expr
and block_expr = Block of loc * expr list
and task_prop =
    | TaskProp of loc * string * expr

type program = Prog of block_expr

