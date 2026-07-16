%{
  [@@@coverage exclude_file]
  open Ast_types
  open Parsed_ast
%}


/* Token definitions */

%token <int> INT
%token <string> ID
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token COLON
%token SEMICOLON
%token PLUS
%token MINUS
%token MULT
%token DIV
%token EQUAL
%token LET
%token TASK
%token <string> STRING
%token EOF

%right EQUAL
%left PLUS MINUS
%left MULT DIV

%start program

%type <Parsed_ast.program> program
%type <task_prop> task_property
%type <block_expr> block_expr
%type <expr> expr
%type <bin_op> bin_op

%%

program:
| exprs=list(statements); EOF { Prog(Block($startpos, exprs)) }

statements:
| e=expr { e }
| t=decl { t }

decl:
| TASK; task_name=ID; LBRACE; props=list(task_property); RBRACE {Task($startpos, Task_name.of_string task_name, props)};

block_expr:
| LBRACE; exprs=separated_list(SEMICOLON, expr); RBRACE {Block($startpos, exprs)}

task_property:
| key=ID; COLON; value=STRING { TaskProp($startpos, key, value) }

identifier:
| variable=ID {Variable(Var_name.of_string variable)}

expr:
| LPAREN e=expr RPAREN {e}
| i=INT {Integer($startpos, i)}
| id=identifier { Identifier($startpos, id)}
| e1=expr op=bin_op e2=expr {BinOp($startpos, op, e1, e2)}
| LET; var_name=ID; EQUAL; bound_expr=expr {Let($startpos, Var_name.of_string var_name, bound_expr)}


%inline bin_op:
| PLUS { BinOpPlus }
| MINUS { BinOpMinus }
| MULT { BinOpMult }
| DIV { BinOpDiv }

