type token =
  | LPAREN
  | LBRACE
  | SEMI
  | COLON
  | RPAREN
  | RBRACE
  | MOD
  | COMMA
  | EOF
  | PLUS
  | TIMES
  | LINK
  | BILINK
  | MINUS
  | DIVIDE
  | EQ
  | ASSIGN
  | PERIOD
  | NEQ
  | LEQ
  | GEQ
  | LT
  | GT
  | NOT
  | AND
  | OR
  | RBRACK
  | LBRACK
  | IF
  | ELSE
  | WHILE
  | FOR
  | RETURN
  | ADD
  | INTAT
  | STRINGAT
  | CHARAT
  | BOOLAT
  | NODE
  | GRAPH
  | BOOL
  | STRING
  | PRINT
  | NEW
  | CONTINUE
  | DOUBLE
  | EDGE
  | FALSE
  | TRUE
  | INT
  | VOID
  | DEST
  | EDGES
  | STATIC
  | CHAR
  | DO
  | IN
  | LITERAL of (int)
  | BOOLLIT of (bool)
  | ID of (string)
  | TYPEID of (string)
  | ARRID of (string)
  | STRINGLIT of (string)
  | CHARLIT of (string)

open Parsing;;
let _ = parse_error;;
# 6 "parser.mly"
 open Ast 

let scope = ref 1 (*contents of scope == 1*)

let inc_block_num
 (u:unit) =
    let x = scope.contents in
    scope := x + 1; x (*set the contents of scope to x+1, increments it by 1*)


let parse_error s = (* Called by the parser function on error *)
  print_endline s;
  flush stdout   

# 85 "parser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* LBRACE *);
  259 (* SEMI *);
  260 (* COLON *);
  261 (* RPAREN *);
  262 (* RBRACE *);
  263 (* MOD *);
  264 (* COMMA *);
    0 (* EOF *);
  265 (* PLUS *);
  266 (* TIMES *);
  267 (* LINK *);
  268 (* BILINK *);
  269 (* MINUS *);
  270 (* DIVIDE *);
  271 (* EQ *);
  272 (* ASSIGN *);
  273 (* PERIOD *);
  274 (* NEQ *);
  275 (* LEQ *);
  276 (* GEQ *);
  277 (* LT *);
  278 (* GT *);
  279 (* NOT *);
  280 (* AND *);
  281 (* OR *);
  282 (* RBRACK *);
  283 (* LBRACK *);
  284 (* IF *);
  285 (* ELSE *);
  286 (* WHILE *);
  287 (* FOR *);
  288 (* RETURN *);
  289 (* ADD *);
  290 (* INTAT *);
  291 (* STRINGAT *);
  292 (* CHARAT *);
  293 (* BOOLAT *);
  294 (* NODE *);
  295 (* GRAPH *);
  296 (* BOOL *);
  297 (* STRING *);
  298 (* PRINT *);
  299 (* NEW *);
  300 (* CONTINUE *);
  301 (* DOUBLE *);
  302 (* EDGE *);
  303 (* FALSE *);
  304 (* TRUE *);
  305 (* INT *);
  306 (* VOID *);
  307 (* DEST *);
  308 (* EDGES *);
  309 (* STATIC *);
  310 (* CHAR *);
  311 (* DO *);
  312 (* IN *);
    0|]

let yytransl_block = [|
  313 (* LITERAL *);
  314 (* BOOLLIT *);
  315 (* ID *);
  316 (* TYPEID *);
  317 (* ARRID *);
  318 (* STRINGLIT *);
  319 (* CHARLIT *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\005\000\005\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\003\000\004\000\004\000\007\000\007\000\008\000\
\008\000\010\000\010\000\011\000\011\000\013\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\009\000\014\000\015\000\
\015\000\016\000\016\000\001\000\001\000\001\000\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000\002\000\003\000\004\000\003\000\
\004\000\000\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\006\000\000\000\001\000\001\000\003\000\001\000\
\003\000\000\000\001\000\000\000\002\000\003\000\001\000\002\000\
\003\000\005\000\007\000\009\000\005\000\002\000\002\000\000\000\
\003\000\009\000\009\000\000\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\060\000\000\000\000\000\032\000\034\000\031\000\030\000\033\000\
\028\000\000\000\029\000\000\000\000\000\061\000\062\000\000\000\
\000\000\055\000\000\000\000\000\000\000\000\000\040\000\000\000\
\000\000\054\000\000\000\000\000\000\000\041\000\056\000\056\000\
\000\000\000\000\000\000\000\000\000\000\057\000\000\000\044\000\
\059\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\005\000\000\000\004\000\002\000\000\000\045\000\047\000\058\000\
\000\000\000\000\021\000\020\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\048\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\024\000\046\000\000\000\000\000\000\000\000\000\
\049\000\000\000\000\000\000\000\000\000\000\000\019\000\010\000\
\000\000\008\000\000\000\009\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\023\000\000\000\025\000\000\000\000\000\053\000\000\000\000\000\
\000\000\000\000\000\000\000\000\051\000\000\000\000\000\000\000\
\035\000\052\000"

let yydgoto = "\002\000\
\003\000\053\000\095\000\091\000\088\000\021\000\092\000\022\000\
\023\000\024\000\036\000\054\000\055\000\014\000\033\000\015\000"

let yysindex = "\011\000\
\000\000\000\000\166\255\000\000\000\000\000\000\000\000\000\000\
\000\000\213\254\000\000\214\254\043\255\000\000\000\000\046\255\
\051\255\000\000\057\001\057\001\251\254\055\255\000\000\061\255\
\062\255\000\000\057\001\069\255\081\255\000\000\000\000\000\000\
\057\001\057\001\084\255\047\255\067\255\000\000\151\255\000\000\
\000\000\151\255\151\255\083\255\087\255\088\255\151\255\000\000\
\000\000\018\255\000\000\000\000\185\000\000\000\000\000\000\000\
\226\000\110\255\000\000\000\000\151\255\151\255\151\255\205\000\
\151\255\029\255\065\255\000\000\151\255\151\255\151\255\151\255\
\151\255\151\255\151\255\151\255\151\255\151\255\151\255\151\255\
\151\255\151\255\000\000\000\000\247\000\012\001\051\001\090\255\
\000\000\051\001\089\255\092\255\070\255\039\255\000\000\000\000\
\008\255\000\000\008\255\000\000\111\001\051\001\111\001\137\255\
\137\255\137\255\137\255\095\001\070\001\100\255\100\255\151\255\
\000\000\151\255\000\000\099\255\078\255\000\000\105\255\051\001\
\057\001\100\255\151\255\151\255\000\000\108\255\031\001\100\255\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\114\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\112\255\000\000\114\255\114\255\000\000\115\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\126\255\126\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\225\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\118\255\000\000\
\117\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\025\255\000\000\
\000\000\050\255\000\000\129\255\000\000\000\000\000\000\000\000\
\249\255\000\000\017\000\000\000\137\000\003\255\141\000\041\000\
\065\000\089\000\113\000\165\000\056\255\000\000\000\000\118\255\
\000\000\000\000\000\000\000\000\164\255\000\000\000\000\057\255\
\000\000\000\000\130\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\218\255\000\000\000\000\154\255\253\255\000\000\000\000\
\023\000\097\000\229\255\000\000\148\255\000\000\099\000\000\000"

let yytablesize = 645
let yytable = "\012\000\
\057\000\117\000\118\000\059\000\060\000\022\000\037\000\022\000\
\064\000\119\000\022\000\001\000\058\000\125\000\069\000\016\000\
\017\000\071\000\065\000\130\000\126\000\073\000\085\000\086\000\
\087\000\013\000\090\000\027\000\022\000\027\000\096\000\097\000\
\098\000\099\000\100\000\101\000\102\000\103\000\104\000\105\000\
\106\000\107\000\108\000\109\000\066\000\018\000\019\000\039\000\
\040\000\030\000\067\000\020\000\041\000\026\000\038\000\035\000\
\035\000\038\000\017\000\042\000\017\000\039\000\027\000\017\000\
\039\000\028\000\029\000\039\000\040\000\043\000\031\000\017\000\
\056\000\087\000\044\000\120\000\045\000\046\000\047\000\042\000\
\017\000\017\000\032\000\061\000\087\000\127\000\038\000\062\000\
\063\000\043\000\093\000\094\000\112\000\113\000\044\000\115\000\
\045\000\046\000\047\000\114\000\116\000\040\000\121\000\048\000\
\049\000\050\000\122\000\123\000\051\000\052\000\039\000\040\000\
\128\000\063\000\054\000\084\000\025\000\124\000\042\000\043\000\
\026\000\036\000\042\000\048\000\049\000\050\000\044\000\044\000\
\051\000\052\000\034\000\044\000\043\000\037\000\026\000\000\000\
\000\000\044\000\044\000\045\000\046\000\047\000\000\000\069\000\
\000\000\070\000\071\000\000\000\044\000\072\000\073\000\039\000\
\000\000\044\000\000\000\044\000\044\000\044\000\000\000\000\000\
\000\000\000\000\000\000\042\000\050\000\050\000\048\000\049\000\
\050\000\050\000\000\000\051\000\052\000\043\000\000\000\000\000\
\050\000\000\000\000\000\000\000\000\000\000\000\044\000\044\000\
\044\000\000\000\050\000\044\000\044\000\000\000\000\000\050\000\
\000\000\050\000\050\000\050\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\000\005\000\006\000\007\000\048\000\
\049\000\050\000\000\000\008\000\051\000\052\000\009\000\010\000\
\000\000\000\000\000\000\011\000\050\000\050\000\050\000\000\000\
\000\000\050\000\050\000\003\000\000\000\003\000\000\000\003\000\
\003\000\003\000\003\000\000\000\000\000\003\000\003\000\003\000\
\003\000\000\000\003\000\003\000\003\000\003\000\003\000\000\000\
\003\000\003\000\003\000\006\000\000\000\006\000\000\000\000\000\
\006\000\006\000\000\000\000\000\000\000\006\000\000\000\006\000\
\006\000\000\000\006\000\006\000\006\000\006\000\006\000\000\000\
\006\000\006\000\006\000\007\000\000\000\007\000\000\000\000\000\
\007\000\007\000\000\000\000\000\000\000\007\000\000\000\007\000\
\007\000\000\000\007\000\007\000\007\000\007\000\007\000\000\000\
\007\000\007\000\007\000\013\000\000\000\013\000\000\000\000\000\
\013\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\013\000\000\000\013\000\013\000\013\000\013\000\013\000\000\000\
\013\000\013\000\013\000\014\000\000\000\014\000\000\000\000\000\
\014\000\000\000\000\000\000\000\000\000\000\000\000\000\014\000\
\014\000\000\000\014\000\014\000\014\000\014\000\014\000\000\000\
\014\000\014\000\014\000\011\000\000\000\011\000\000\000\000\000\
\011\000\000\000\000\000\000\000\000\000\000\000\000\000\011\000\
\011\000\000\000\011\000\011\000\011\000\011\000\011\000\000\000\
\011\000\011\000\011\000\012\000\000\000\012\000\000\000\000\000\
\012\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000\
\012\000\000\000\012\000\012\000\012\000\012\000\012\000\000\000\
\012\000\012\000\012\000\015\000\000\000\015\000\000\000\016\000\
\015\000\016\000\000\000\000\000\016\000\000\000\000\000\015\000\
\015\000\000\000\015\000\016\000\016\000\000\000\016\000\000\000\
\015\000\015\000\015\000\000\000\016\000\016\000\016\000\018\000\
\000\000\018\000\000\000\000\000\018\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\018\000\000\000\000\000\000\000\
\000\000\000\000\000\000\068\000\018\000\018\000\018\000\069\000\
\000\000\070\000\071\000\000\000\000\000\072\000\073\000\074\000\
\075\000\000\000\076\000\077\000\078\000\079\000\080\000\089\000\
\081\000\082\000\000\000\069\000\000\000\070\000\071\000\000\000\
\000\000\072\000\073\000\074\000\075\000\000\000\076\000\077\000\
\078\000\079\000\080\000\000\000\081\000\082\000\083\000\000\000\
\069\000\000\000\070\000\071\000\000\000\000\000\072\000\073\000\
\074\000\075\000\000\000\076\000\077\000\078\000\079\000\080\000\
\000\000\081\000\082\000\110\000\000\000\069\000\000\000\070\000\
\071\000\000\000\000\000\072\000\073\000\074\000\075\000\000\000\
\076\000\077\000\078\000\079\000\080\000\000\000\081\000\082\000\
\111\000\000\000\069\000\000\000\070\000\071\000\000\000\000\000\
\072\000\073\000\074\000\075\000\000\000\076\000\077\000\078\000\
\079\000\080\000\000\000\081\000\082\000\069\000\000\000\070\000\
\071\000\000\000\000\000\072\000\073\000\074\000\075\000\000\000\
\076\000\077\000\078\000\079\000\080\000\000\000\081\000\082\000\
\129\000\069\000\000\000\070\000\071\000\000\000\000\000\072\000\
\073\000\074\000\075\000\000\000\076\000\077\000\078\000\079\000\
\080\000\000\000\081\000\082\000\069\000\000\000\070\000\071\000\
\000\000\000\000\072\000\073\000\074\000\000\000\000\000\076\000\
\077\000\078\000\079\000\080\000\000\000\081\000\004\000\005\000\
\006\000\007\000\000\000\000\000\000\000\069\000\008\000\070\000\
\071\000\009\000\000\000\072\000\073\000\074\000\011\000\000\000\
\076\000\077\000\078\000\079\000\080\000\069\000\000\000\070\000\
\071\000\000\000\000\000\072\000\073\000\000\000\000\000\000\000\
\000\000\077\000\078\000\079\000\080\000"

let yycheck = "\003\000\
\039\000\110\000\111\000\042\000\043\000\003\001\034\000\005\001\
\047\000\112\000\008\001\001\000\040\000\122\000\007\001\059\001\
\059\001\010\001\001\001\128\000\123\000\014\001\061\000\062\000\
\063\000\003\000\065\000\003\001\026\001\005\001\069\000\070\000\
\071\000\072\000\073\000\074\000\075\000\076\000\077\000\078\000\
\079\000\080\000\081\000\082\000\027\001\003\001\001\001\001\001\
\002\001\027\000\033\001\001\001\006\001\059\001\005\001\033\000\
\034\000\008\001\003\001\013\001\005\001\005\001\008\001\008\001\
\008\001\005\001\005\001\001\001\002\001\023\001\002\001\016\001\
\006\001\112\000\028\001\114\000\030\001\031\001\032\001\013\001\
\025\001\026\001\002\001\001\001\123\000\124\000\003\001\001\001\
\001\001\023\001\062\001\027\001\003\001\005\001\028\001\026\001\
\030\001\031\001\032\001\008\001\062\001\002\001\004\001\057\001\
\058\001\059\001\029\001\003\001\062\001\063\001\001\001\002\001\
\005\001\000\000\003\001\006\001\020\000\121\000\005\001\005\001\
\003\001\005\001\013\001\057\001\058\001\059\001\001\001\002\001\
\062\001\063\001\032\000\006\001\023\001\005\001\005\001\255\255\
\255\255\028\001\013\001\030\001\031\001\032\001\255\255\007\001\
\255\255\009\001\010\001\255\255\023\001\013\001\014\001\001\001\
\255\255\028\001\255\255\030\001\031\001\032\001\255\255\255\255\
\255\255\255\255\255\255\013\001\001\001\002\001\057\001\058\001\
\059\001\006\001\255\255\062\001\063\001\023\001\255\255\255\255\
\013\001\255\255\255\255\255\255\255\255\255\255\057\001\058\001\
\059\001\255\255\023\001\062\001\063\001\255\255\255\255\028\001\
\255\255\030\001\031\001\032\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\038\001\039\001\040\001\041\001\057\001\
\058\001\059\001\255\255\046\001\062\001\063\001\049\001\050\001\
\255\255\255\255\255\255\054\001\057\001\058\001\059\001\255\255\
\255\255\062\001\063\001\003\001\255\255\005\001\255\255\007\001\
\008\001\009\001\010\001\255\255\255\255\013\001\014\001\015\001\
\016\001\255\255\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\025\001\026\001\003\001\255\255\005\001\255\255\255\255\
\008\001\009\001\255\255\255\255\255\255\013\001\255\255\015\001\
\016\001\255\255\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\025\001\026\001\003\001\255\255\005\001\255\255\255\255\
\008\001\009\001\255\255\255\255\255\255\013\001\255\255\015\001\
\016\001\255\255\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\025\001\026\001\003\001\255\255\005\001\255\255\255\255\
\008\001\255\255\255\255\255\255\255\255\255\255\255\255\015\001\
\016\001\255\255\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\025\001\026\001\003\001\255\255\005\001\255\255\255\255\
\008\001\255\255\255\255\255\255\255\255\255\255\255\255\015\001\
\016\001\255\255\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\025\001\026\001\003\001\255\255\005\001\255\255\255\255\
\008\001\255\255\255\255\255\255\255\255\255\255\255\255\015\001\
\016\001\255\255\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\025\001\026\001\003\001\255\255\005\001\255\255\255\255\
\008\001\255\255\255\255\255\255\255\255\255\255\255\255\015\001\
\016\001\255\255\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\025\001\026\001\003\001\255\255\005\001\255\255\003\001\
\008\001\005\001\255\255\255\255\008\001\255\255\255\255\015\001\
\016\001\255\255\018\001\015\001\016\001\255\255\018\001\255\255\
\024\001\025\001\026\001\255\255\024\001\025\001\026\001\003\001\
\255\255\005\001\255\255\255\255\008\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\016\001\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\024\001\025\001\026\001\007\001\
\255\255\009\001\010\001\255\255\255\255\013\001\014\001\015\001\
\016\001\255\255\018\001\019\001\020\001\021\001\022\001\003\001\
\024\001\025\001\255\255\007\001\255\255\009\001\010\001\255\255\
\255\255\013\001\014\001\015\001\016\001\255\255\018\001\019\001\
\020\001\021\001\022\001\255\255\024\001\025\001\005\001\255\255\
\007\001\255\255\009\001\010\001\255\255\255\255\013\001\014\001\
\015\001\016\001\255\255\018\001\019\001\020\001\021\001\022\001\
\255\255\024\001\025\001\005\001\255\255\007\001\255\255\009\001\
\010\001\255\255\255\255\013\001\014\001\015\001\016\001\255\255\
\018\001\019\001\020\001\021\001\022\001\255\255\024\001\025\001\
\005\001\255\255\007\001\255\255\009\001\010\001\255\255\255\255\
\013\001\014\001\015\001\016\001\255\255\018\001\019\001\020\001\
\021\001\022\001\255\255\024\001\025\001\007\001\255\255\009\001\
\010\001\255\255\255\255\013\001\014\001\015\001\016\001\255\255\
\018\001\019\001\020\001\021\001\022\001\255\255\024\001\025\001\
\026\001\007\001\255\255\009\001\010\001\255\255\255\255\013\001\
\014\001\015\001\016\001\255\255\018\001\019\001\020\001\021\001\
\022\001\255\255\024\001\025\001\007\001\255\255\009\001\010\001\
\255\255\255\255\013\001\014\001\015\001\255\255\255\255\018\001\
\019\001\020\001\021\001\022\001\255\255\024\001\038\001\039\001\
\040\001\041\001\255\255\255\255\255\255\007\001\046\001\009\001\
\010\001\049\001\255\255\013\001\014\001\015\001\054\001\255\255\
\018\001\019\001\020\001\021\001\022\001\007\001\255\255\009\001\
\010\001\255\255\255\255\013\001\014\001\255\255\255\255\255\255\
\255\255\019\001\020\001\021\001\022\001"

let yynames_const = "\
  LPAREN\000\
  LBRACE\000\
  SEMI\000\
  COLON\000\
  RPAREN\000\
  RBRACE\000\
  MOD\000\
  COMMA\000\
  EOF\000\
  PLUS\000\
  TIMES\000\
  LINK\000\
  BILINK\000\
  MINUS\000\
  DIVIDE\000\
  EQ\000\
  ASSIGN\000\
  PERIOD\000\
  NEQ\000\
  LEQ\000\
  GEQ\000\
  LT\000\
  GT\000\
  NOT\000\
  AND\000\
  OR\000\
  RBRACK\000\
  LBRACK\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  FOR\000\
  RETURN\000\
  ADD\000\
  INTAT\000\
  STRINGAT\000\
  CHARAT\000\
  BOOLAT\000\
  NODE\000\
  GRAPH\000\
  BOOL\000\
  STRING\000\
  PRINT\000\
  NEW\000\
  CONTINUE\000\
  DOUBLE\000\
  EDGE\000\
  FALSE\000\
  TRUE\000\
  INT\000\
  VOID\000\
  DEST\000\
  EDGES\000\
  STATIC\000\
  CHAR\000\
  DO\000\
  IN\000\
  "

let yynames_block = "\
  LITERAL\000\
  BOOLLIT\000\
  ID\000\
  TYPEID\000\
  ARRID\000\
  STRINGLIT\000\
  CHARLIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 56 "parser.mly"
                                         ( Literal(_1) )
# 485 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
                                         ( Char_e(_1) )
# 492 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parser.mly"
                                         ( Id(_1))
# 499 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "parser.mly"
                                         ( String_Lit(_1) )
# 506 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 60 "parser.mly"
                                         ( Bool_Lit(_1))
# 513 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 61 "parser.mly"
                                         ( Binop (_1, Add, _3) )
# 521 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 62 "parser.mly"
                                        ( Binop (_1, Sub, _3) )
# 529 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
                                        ( Binop (_1, Mult, _3) )
# 537 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                                         ( Binop (_1, Div, _3) )
# 545 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                                       ( Binop (_1, Mod, _1) )
# 553 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                                        ( Binop (_1, Less, _3) )
# 561 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                                        ( Binop (_1, Greater, _3) )
# 569 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                                       ( Binop (_1, Leq, _3) )
# 577 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                                       ( Binop (_1, Geq, _3) )
# 585 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                                         ( Binop (_1, Equal, _3) )
# 593 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                                       ( Binop (_1, Neq, _3) )
# 601 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                                        ( Binop (_1, Or, _3) )
# 609 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                                       ( Binop (_1, And, _3) )
# 617 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attribute) in
    Obj.repr(
# 74 "parser.mly"
                                         ( Add_at(_1, _3) )
# 625 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                                       ( Unop(Not, _2) )
# 632 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                                         ( Unop(Neg, _2) )
# 639 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                                         ( Assign(_1, _3) )
# 647 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 78 "parser.mly"
                                         ( Call(_1, _3) )
# 655 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
                                         ( _2 )
# 662 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 81 "parser.mly"
                                       ( Access(_1, _3) )
# 670 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
                  ( Noexpr )
# 676 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 85 "parser.mly"
                  ( _1 )
# 683 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
           ( Int )
# 689 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
           ( Char )
# 695 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
           ( String )
# 701 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
           ( Bool )
# 707 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
           ( Node([]) )
# 713 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
           ( Edge([]) )
# 719 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
           ( Graph )
# 725 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'any_type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                                            ( Attr(_2, _4, _5) )
# 734 "parser.ml"
               : 'attribute))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser.mly"
                 ( [] )
# 740 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 101 "parser.mly"
                 ( List.rev _1 )
# 747 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                            ( [_1] )
# 754 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                            ( _3 :: _1 )
# 762 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 108 "parser.mly"
                             ( [_1])
# 769 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 109 "parser.mly"
                             ( _3 :: _1 )
# 777 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
                 ( [] )
# 783 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 113 "parser.mly"
                 ( List.rev _1 )
# 790 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "parser.mly"
                   ( [] )
# 796 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 117 "parser.mly"
                   ( _2 :: _1 )
# 804 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 120 "parser.mly"
                          ( {locals = []; statements = List.rev _2; block_num = inc_block_num ()} )
# 811 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 123 "parser.mly"
                                                                  ( Block(_1))
# 818 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                                                                  ( Expr(_1) )
# 825 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                                                                  ( Return(_2) )
# 832 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 127 "parser.mly"
                                                                  ( If(_3, _5, {locals = []; statements = []; block_num = inc_block_num ()}) )
# 840 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'block) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 128 "parser.mly"
                                                                  ( If (_3, _5, _7) )
# 849 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 129 "parser.mly"
                                                                  ( For(_3, _5, _7, _9) )
# 859 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 130 "parser.mly"
                                                                  ( While(_3, _5) )
# 867 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'any_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 133 "parser.mly"
              (_2, _1)
# 875 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl) in
    Obj.repr(
# 136 "parser.mly"
             ( _1 )
# 882 "parser.ml"
               : 'glb_vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "parser.mly"
                          ( [] )
# 888 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl) in
    Obj.repr(
# 141 "parser.mly"
                          ( _2 :: _1 )
# 896 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'any_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 145 "parser.mly"
     ( { fname = _2;
         formals = _4; 
         body_block = {locals = List.rev _7; statements = List.rev _8; block_num = inc_block_num()} ;
         ret = _1 } )
# 910 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 150 "parser.mly"
     ( { fname = _2;
         formals = _4; 
         body_block = {locals = List.rev _7; statements = List.rev _8; block_num = inc_block_num()} ;
         ret = Void } )
# 923 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 156 "parser.mly"
                 ( [], [] )
# 929 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'glb_vdecl) in
    Obj.repr(
# 157 "parser.mly"
                     ( (_2 :: fst _1), snd _1 )
# 937 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 158 "parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 945 "parser.ml"
               : Ast.program))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
