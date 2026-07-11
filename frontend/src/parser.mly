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
%token TYPE_VOID 
%token SEMICOLON
%token PLUS
%token MINUS
%token MULT
%token DIV
%token PRINTF
%token EOF

%start program

%type <Parsed_ast.program> program
%type <block_expr> main_expr
%type <block_expr> block_expr
%type <expr> expr

%%

program:
| main=main_expr; EOF {Prog(main)}

main_expr:
| TYPE_VOID; exprs=block_expr {exprs}

block_expr:
| LBRACE; exprs=separated_list(SEMICOLON, expr); RBRACE {Block($startpos, exprs)}

identifier:
| variable=ID {Variable(Var_name.of_string variable)}

expr:
| LPAREN e=expr RPAREN {e}
| i=INT {Integer($startpos, i)}
| id=identifier { Identifier($startpos, id)}

