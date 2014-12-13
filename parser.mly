/*
Authors: Gemma Ragozzine
         John HeizelMan
*/

%{ open Ast 

let scope = ref 1 (*contents of scope == 1*)

let inc_block_num
 (u:unit) =
    let x = scope.contents in
    scope := x + 1; x (*set the contents of scope to x+1, increments it by 1*)


let parse_error s = (* Called by the parser function on error *)
  print_endline s;
  flush stdout   

%}

%token LPAREN LBRACE SEMI COLON RPAREN RBRACE MOD COMMA EOF
%token PLUS TIMES LINK BILINK MINUS DIVIDE EQ ASSIGN PERIOD
%token NEQ LEQ GEQ LT GT NOT AND OR RBRACK LBRACK
%token IF ELSE WHILE FOR RETURN ADD
%token GRAPH NODE BOOL STRING PRINT NEW CONTINUE DOUBLE EDGE
%token FALSE TRUE INT VOID DEST EDGES STATIC CHAR DO IN
%token <int> LITERAL
%token <bool> BOOLLIT
%token <string> ID TYPEID ARRID STRINGLIT
%token <string> CHARLIT

/* state precedence of tokens - need this to avoid shift/reduce conflicts */
/* goes from least to most important in precedence */
%nonassoc NOELSE
%nonassoc ELSE
%nonassoc NOASSIGN
%right ASSIGN
%left OR
%left AND 
%left EQ NEQ
%left LEQ GEQ LT GT
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NEW
%right NOT
%right NEG
%left LPAREN RPAREN

%start program
%type <Ast.program> program

%%

expr:
 LITERAL                                 { Literal($1) }
| CHARLIT                                { Char($1) }
| ID                                     { Id($1)}
| STRINGLIT                              { String_Lit($1) }
| BOOLLIT                                { Bool_Lit($1)}
| expr PLUS expr 	                       { Binop ($1, Add, $3) }
| expr MINUS expr 	                     { Binop ($1, Sub, $3) }
| expr TIMES expr 	                     { Binop ($1, Mult, $3) }
| expr DIVIDE expr                       { Binop ($1, Div, $3) }
| expr MOD expr 		                     { Binop ($1, Mod, $1) }
| expr LT expr 		                       { Binop ($1, Less, $3) }
| expr GT expr 		                       { Binop ($1, Greater, $3) }
| expr LEQ expr 		                     { Binop ($1, Leq, $3) }
| expr GEQ expr 		                     { Binop ($1, Geq, $3) }
| expr EQ expr 	                         { Binop ($1, Equal, $3) }
| expr NEQ expr 		                     { Binop ($1, Neq, $3) }
| expr OR expr 		                       { Binop ($1, Or, $3) }
| expr AND expr 		                     { Binop ($1, And, $3) }
| expr ADD expr                          { Add($1, $3) }
| NOT expr		                           { Unop(Not, $2) } 
| MINUS expr %prec NEG                   { Unop(Neg, $2) }
| expr ASSIGN expr                       { Assign($1, $3) }
| ID LPAREN actuals_opt RPAREN           { Call($1, $3) }
| LPAREN expr RPAREN                     { $2 }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

any_type:
INT        { Int }
| CHAR     { Char }
| STRING   { String }
| BOOL     { Bool }
| obj_type { $1 } 
| attr_type { $1 }

obj_type:
NODE       { Node }
| NODETYPE { NodeType }
| EDGE     { Edge }
| EDGETYPE { EdgeType }
| GRAPH    { Graph }
| TYPEID   { UserDef }

attr_type:
INTAT      { Int_at}

actuals_opt:
  /* nothing */  { [] }
  | actuals_list { List.rev $1 }

actuals_list:
  expr                      { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

formals_list:
    vdecl                    { [$1]}
  | formals_list COMMA vdecl { $3 :: $1 }

formals_opt:
  /* nothing */  { [] }
  | formals_list { List.rev $1 }

stmt_list:
  /* nothing */    { [] }
  | stmt_list stmt { $2 :: $1 }

block:
  LBRACE stmt_list RBRACE { {locals = []; statements = List.rev $2; block_num = inc_block_num ()} }

stmt:
  block                                                           { Block($1)}
  /*| vdecl SEMI                                                    { Vdecl(snd $1, fst $1)}*/
  | expr SEMI                                                     { Expr($1) }
  | RETURN expr SEMI                                              { Return($2) }
  | IF LPAREN expr RPAREN block %prec NOELSE                      { If($3, $5, {locals = []; statements = []; block_num = inc_block_num ()}) }
  | IF LPAREN expr RPAREN block ELSE block                        { If ($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN block  { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN block                                { While($3, $5) }

vdecl:
  any_type ID {$2, $1}

glb_vdecl:
  vdecl SEMI { $1 }


vdecl_list:
  /* nothing */           { [] }
  | vdecl_list vdecl SEMI { $2 :: $1 }

fdecl:
   any_type ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { fname = $2;
         formals = $4; 
         body_block = {locals = List.rev $7; statements = List.rev $8; block_num = inc_block_num()} ;
         ret = $1 } }
  | VOID ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { fname = $2;
         formals = $4; 
         body_block = {locals = List.rev $7; statements = List.rev $8; block_num = inc_block_num()} ;
         ret = Void } }

program:
	/* nothing */   { [], [] }
	| program glb_vdecl { ($2 :: fst $1), snd $1 }
	| program fdecl { fst $1, ($2 :: snd $1) }
