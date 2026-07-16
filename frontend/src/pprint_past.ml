open Ast_types
open Parsed_ast
open Core

let indent_space = "   "

let rec pprint_expr ppf ~indent expr =
    let print_expr = Fmt.pf ppf "%sExpr: %s@." indent in
    let new_indent = indent_space ^ indent in
    match expr with
    | Integer (_, i) -> print_expr (Fmt.str "Int:%d" i)
    | Let (_, var_name, bound_expr) ->
            print_expr (Fmt.str "Let var: %s" (Var_name.to_string var_name));
            pprint_expr ppf ~indent:new_indent bound_expr
    | Task (_, task_name, _) ->
            print_expr (Fmt.str "Task: %s" (Task_name.to_string task_name))
    | Identifier (_, id) -> (
        match id with
            | Variable var_name ->
                    print_expr (Fmt.str "Variable: %s" (Var_name.to_string var_name))
    )
    | Assign (loc, id, assigned_expr) ->
            print_expr "Assign";
            pprint_expr ppf ~indent:new_indent (Identifier (loc, id));
            pprint_expr ppf ~indent:new_indent assigned_expr
    | BinOp (_, bin_op, expr1, expr2) ->
            print_expr (Fmt.str "Bin Op: %s" (string_of_bin_op bin_op));
            pprint_expr ppf ~indent:new_indent expr1 ;
            pprint_expr ppf ~indent:new_indent expr2

and pprint_args ppf ~indent = function
    | []   -> Fmt.pf ppf "%s()@." indent
    | args -> List.iter ~f:(pprint_expr ppf ~indent) args

and pprint_block_expr ppf ~indent ~block_name (Block (_, exprs)) = 
    let new_indent = indent_space ^ indent in
    Fmt.pf ppf "%s%s block@." indent block_name;
    List.iter ~f:(pprint_expr ppf ~indent:new_indent) exprs

let pprint_program ppf (Prog (main_expr)) =
    Fmt.pf ppf "Program@.";
    let indent = "└──" in
    pprint_block_expr ppf ~indent ~block_name:"Main" main_expr
