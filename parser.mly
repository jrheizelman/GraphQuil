%{ open Ast %}

%token LPAREN LBRACE SEMI  RPAREN RBRACE COMMA EOF
%token PLUS TIMES MINUS DIVIDE ASSIGN PERIOD
%token EQ NEQ LEQ GEQ LT GT OR AND NOT
%token IF ELSE WHILE FOR RETURN
%token GRAPH NODE BOOL STRING STRUCT PRINT NEW ID
%token <int> NUM

/* state precedence of tokens - need this to avoid shift/reduce conflicts */
%right ASSIGN
%left OR
%left AND 
%left EQ NEQ
%left LEQ GEQ LT GT
%left PLUS MINUS
%left TIMES DIVIDE
%right NEW
%right NOT

%start expr
%type <unit> expr


%%


expr:
expr PLUS expr { Binop ($1, Add, $3)}
|expr TIMES expr {}
|expr MINUS expr {}
|expr DIVIDE expr {}
|expr LT expr {}
|expr GT expr {}
|expr LEQ expr {}
|expr GEQ expr {}
|expr EQ expr {}
|expr NEQ expr {}
|expr OR expr {}
|expr AND expr {}
|expr ASSIGN expr {} 
|NUM { Lit($1)}
/*|expr ID LPAREN expr RPAREN {} function call*/


