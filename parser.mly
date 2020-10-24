
(* Identifiers *)
%token <string> ID

(* Literal *)
%token <int> INT

(* Special *)
%token RETURN EOF

(* Punctuation *)
%token COMMA COLON SEMICOLON
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token DOT

%token PLUS MINUS TIMES DIVIDE

%token EQ NEQ LT LTE GT GTE
%token AND OR

%token ASSIGN

%start <string> main

%%

main:
| stmt = ID EOF { stmt }
| stmt = ID RETURN { stmt }
