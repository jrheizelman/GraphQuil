%{ open Ast %}

%token LPAREN LBRACE SEMI RPAREN RBRACE MOD COMMA PERIOD EOF
%token PLUS TIMES LINK BILINK MINUS DIVIDE EQ ASSIGN PERIOD
%token NEQ LEQ GEQ LT GT NOT AND OR
%token IF ELSE WHILE FOR RETURN GRAPH NODETYPE EDGETYPE
%token NODE BOOL STRING STRUCT PRINT NEW CONTINUE DOUBLE
%token FALSE TRUE INT VOID DEST EDGES STATIC CHR DO IN
%token <int> NUM
%token <string> ID

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

%start program
%type <unit> program


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

program:
	/* nothing */ { [], [] }
	/*| program vdecl { ($2 :: fst $1), snd $1 }
	| program fdecl { fst $1, ($2 :: snd $1) }*/


