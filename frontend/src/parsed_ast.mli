open Ast_types

type identifier = Variable of Var_name.t
type task_prop = TaskProp of loc * string * string

type expr =
    | Integer of loc * int
    | Identifier of loc * identifier
    | Let of loc * Var_name.t * expr
    | Task of loc * Task_name.t * task_prop list
    | Assign of loc * identifier * expr
    | BinOp of loc * bin_op * expr * expr
and block_expr = Block of loc * expr list

type program = Prog of block_expr

