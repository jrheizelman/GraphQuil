%{ open Ast %}

%token LPAREN LBRACE SEMI COLON RPAREN RBRACE MOD COMMA EOF
%token PLUS TIMES LINK BILINK MINUS DIVIDE EQ ASSIGN PERIOD
%token NEQ LEQ GEQ LT GT NOT AND OR RBRACK LBRACK
%token IF ELSE WHILE FOR RETURN NODETYPE EDGETYPE
%token GRAPH NODE BOOL STRING PRINT NEW CONTINUE DOUBLE EDGE
%token FALSE TRUE INT VOID DEST EDGES STATIC CHAR DO IN
%token <int> LITERAL
%token <string> ID TYPEID ARRID CHARLIT STRINGLIT

/* state precedence of tokens - need this to avoid shift/reduce conflicts */
/* goes from least to most important in precedence */
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
%left LPAREN RPAREN

%start program
%type <Ast.program> program

%%

expr:
 LITERAL          { Literal($1) }
|CHARLIT          { Char($1) }
|STRINGLIT        { String($1) }
|expr PLUS expr 	{ Binop ($1, Add, $3) }
|expr MINUS expr 	{ Binop ($1, Sub, $3) }
|expr TIMES expr 	{ Binop ($1, Mult, $3) }
|expr DIVIDE expr { Binop ($1, Div, $3) }
|expr MOD expr 		{ Binop ($1, Mod, $1) }
|expr LT expr 		{ Binop ($1, Less, $3) }
|expr GT expr 		{ Binop ($1, Greater, $3) }
|expr LEQ expr 		{ Binop ($1, Leq, $3) }
|expr GEQ expr 		{ Binop ($1, Geq, $3) }
|expr EQ expr 		{ Binop ($1, Equal, $3) }
|expr NEQ expr 		{ Binop ($1, Neq, $3) }
|expr OR expr 		{ Binop ($1, Or, $3) }
|expr AND expr 		{ Binop ($1, And, $3) }
|var ASSIGN expr  { Assign($1, $3) }
|NOT expr		      { Not($2) } 
|ID LPAREN actuals_opt RPAREN { Call($1, $3) }
|LPAREN expr RPAREN { $2 }
|var  { $1 }
|NEW obj_type LPAREN actuals_opt RPAREN { Construct($2, $4) }
|NEW any_type LBRACK expr RBRACK { MakeArr($2, $4) }

var:
        ID      { Id($1) }
        | arr   { Array( fst $1, snd $1) }
        | ID PERIOD ID { Access($1, $3) }

arr:
        ID LBRACK expr RBRACK { Id($1),$3 }

any_type:
INT { Int}
| CHAR { Char }
| STRING { String }
| DOUBLE { Double }
| BOOL { Bool }
| ARRID { Arr } 
| obj_type { $1 }

obj_type:
NODE { Node }
| NODETYPE { NodeType }
| EDGE { Edge }
| EDGETYPE { EdgeType }
| GRAPH { Graph }
| TYPEID { Userdef }

actuals_opt:
  /* nothing */ { [] }
  | actuals_list { List.rev $1 }

actuals_list:
  expr { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

expr_opt:
  /* nothing */ {Noexpr }
  | expr { $1 }

formals_list:
  type_decl ID { [$2] }
  | formals_list COMMA type_decl ID { $4 :: $1 }

formals_opt:
  /* nothing */ { [] }
  | formals_list { List.rev $1 }

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
  type_decl SEMI { $1 }
  | type_decl ASSIGN expr SEMI { $1 }

vdecl_list:
  /* nothing */ { [] }
  | vdecl_list vdecl { $2 :: $1 }

fdecl:
   retval formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { fname = snd $1;
         formals = $2; 
         locals = List.rev $5;
         body = List.rev $6;
         ret = fst $1
         } }

retval:
        any_type ID LPAREN { [$1], $2  }
        | VOID ID LPAREN { [Void], $2  } 

type_decl:
 any_type ID { $2 }

program:
	/* nothing */ { [], [] }
	| program vdecl { ($2 :: fst $1), snd $1 }
	| program fdecl { fst $1, ($2 :: snd $1) }
