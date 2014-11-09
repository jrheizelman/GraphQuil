%{ open Ast %}

%token LPAREN LBRACE SEMI COLON RPAREN RBRACE MOD COMMA PERIOD EOF
%token PLUS TIMES LINK BILINK MINUS DIVIDE EQ ASSIGN PERIOD
%token NEQ LEQ GEQ LT GT NOT AND OR
%token IF ELSE WHILE FOR RETURN GRAPH NODETYPE EDGETYPE
%token NODE BOOL STRING STRUCT PRINT NEW CONTINUE DOUBLE
%token FALSE TRUE INT VOID DEST EDGES STATIC CHAR DO IN
%token <int> NUM
%token <string> ID

/* state precedence of tokens - need this to avoid shift/reduce conflicts */
%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND 
%left EQ NEQ
%left LEQ GEQ LT GT
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NEW
%right NOT

%start program
%type <unit> program


%%

actuals_opt:
  /* nothing */ { [] }
  | actuals_list { List.rev $1 }

actuals_list:
  expr { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

expr:
expr PLUS expr 		{ Binop ($1, Add, $3) }
|expr MINUS expr 	{ Binop ($1, Sub, $3) }
|expr TIMES expr 	{ Binop ($1, Mult, $3) }
|expr DIVIDE expr 	{ Binop ($1, Div, $3) }
|expr MOD expr 		{ Binop ($1, Mod, $1) }
|expr LT expr 		{ Binop ($1, Less, $3) }
|expr GT expr 		{ Binop ($1, Greater, $3) }
|expr LEQ expr 		{ Binop ($1, Leq, $3) }
|expr GEQ expr 		{ Binop ($1, Geq, $3) }
|expr EQ expr 		{ Binop ($1, Equal, $3) }
|expr NEQ expr 		{ Binop ($1, Neq, $3) }
|expr OR expr 		{ Binop ($1, Or, $3) }
|expr AND expr 		{ Binop ($1, And, $3) }
|NOT expr		{ Not($2) } 
|ID ASSIGN expr { Assign($1, $3) }
|ID LPAREN actuals_opt RPAREN { Call($1, $3) }
|LPAREN expr RPAREN { $2 }
|NUM { Lit($1)}
/*|expr ID LPAREN expr RPAREN {} function call*/

expr_opt:
  /* nothing */ {Noexpr }
  | expr { $1 }

formal_list:
  ID { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

formals_opt:
  /* nothing */ { [] }
  | formal_list { List.rev $1 }

stmt_list:
  /* nothing */ { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
  expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt { If ($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
  	{ For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

vdecl:
  INT ID SEMI { $2 }
  | BOOL ID SEMI { $2 }
  | DOUBLE ID SEMI { $2 }

vdecl_list:
  /* nothing */ { [] }
  | vdecl_list vdecl { $2 :: $1 }

fdecl:
  ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
    {{  fname = $1;
    	formals = $3;
    	locals = List.rev $6;
    	body = List.rev $7; }}

program:
	/* nothing */ { [], [] }
	| program vdecl { ($2 :: fst $1), snd $1 }
	| program fdecl { fst $1, ($2 :: snd $1) }
