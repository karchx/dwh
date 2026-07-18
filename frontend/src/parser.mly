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
%token COMMA
%token RBRACE
%token COLON
%token SEMICOLON
%token PLUS
%token MINUS
%token MULT
%token DIV
%token EQUAL
%token CONN
%token LET
%token IF
%token ELSE 
%token TASK
%token PIPELINE
%token <string> STRING
%token EOF

%right CONN
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
| stmts=list(statement); EOF { Prog(Block($startpos, stmts)) }


decl:
| TASK; task_name=ID;
  params=loption(delimited(LPAREN, separated_list(COMMA, ID), RPAREN)); 
  LBRACE;
  props=list(task_property);
  RBRACE 
  { Task($startpos, Task_name.of_string task_name, List.map Var_name.of_string params, props) }
| PIPELINE; pipeline_name=ID;
 LBRACE;
 exprs=expr;
 RBRACE;
 { Pipeline($startpos, Pipeline_name.of_string pipeline_name, exprs) }

block_expr:
| LBRACE; stmts=list(statement); RBRACE {Block($startpos, stmts)}

task_property:
| key=ID; COLON; value=expr { TaskProp($startpos, key, value) }

identifier:
| variable=ID {Variable(Var_name.of_string variable)}

statement:
| LET; var_name=ID; EQUAL; bound_expr=expr; SEMICOLON; {Let($startpos, Var_name.of_string var_name, bound_expr)}
| IF; cond_expr=expr; then_expr=block_expr; ELSE; else_expr=block_expr {If($startpos, cond_expr, then_expr, else_expr)}
| e=expr; SEMICOLON { e }
| d=decl { d }

expr:
| LPAREN e=expr RPAREN {e}
| i=INT {Integer($startpos, i)}
| id=identifier { Identifier($startpos, id)}
| e1=expr op=bin_op e2=expr {BinOp($startpos, op, e1, e2)}
| s=STRING; {StringLit($startpos, s)}


%inline bin_op:
| PLUS { BinOpPlus }
| MINUS { BinOpMinus }
| MULT { BinOpMult }
| DIV { BinOpDiv }
| CONN { BinOpConn }
| EQUAL EQUAL {BinOpEq}

