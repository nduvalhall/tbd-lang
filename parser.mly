/* parser.mly */
%{
  (* OCaml prologue: types and helper functions *)
  open Ast
%}

/* token definitions (must match your lexer) */
%token <int>  INT
%token <string> ID
%token PLUS MINUS TIMES DIVIDE LPAREN RPAREN EOF

%start <expr> main
%%

main:
  | expr EOF { $1 }

expr:
  | INT                { IntLit $1 }
  | ID                 { Var   $1 }
  | expr PLUS expr    { Binop (Add, $1, $3) }
  | expr MINUS expr   { Binop (Sub, $1, $3) }
  | LPAREN expr RPAREN { $2 }
