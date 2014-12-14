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
\004\000\004\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\003\000\003\000\006\000\
\006\000\007\000\007\000\009\000\009\000\010\000\010\000\012\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\008\000\
\013\000\014\000\014\000\015\000\015\000\001\000\001\000\001\000\
\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000\002\000\003\000\004\000\003\000\
\000\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\000\000\001\000\001\000\
\003\000\001\000\003\000\000\000\001\000\000\000\002\000\003\000\
\001\000\002\000\003\000\005\000\007\000\009\000\005\000\002\000\
\002\000\000\000\003\000\009\000\009\000\000\000\002\000\002\000\
\002\000"

let yydefred = "\000\000\
\062\000\000\000\000\000\034\000\036\000\037\000\035\000\031\000\
\033\000\030\000\029\000\032\000\027\000\000\000\028\000\000\000\
\000\000\063\000\064\000\000\000\000\000\057\000\000\000\000\000\
\000\000\000\000\042\000\000\000\000\000\056\000\000\000\000\000\
\000\000\043\000\058\000\058\000\000\000\000\000\000\000\000\000\
\000\000\059\000\000\000\046\000\061\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\005\000\000\000\004\000\002\000\
\000\000\047\000\049\000\060\000\000\000\000\000\021\000\020\000\
\000\000\000\000\000\000\000\000\000\000\000\000\050\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\024\000\048\000\000\000\
\000\000\000\000\000\000\051\000\000\000\000\000\000\000\019\000\
\010\000\000\000\008\000\000\000\009\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\023\000\000\000\000\000\055\000\000\000\000\000\000\000\
\000\000\053\000\000\000\000\000\054\000"

let yydgoto = "\002\000\
\003\000\057\000\094\000\091\000\025\000\095\000\026\000\027\000\
\028\000\040\000\058\000\059\000\018\000\037\000\019\000"

let yysindex = "\005\000\
\000\000\000\000\150\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\204\254\000\000\216\254\
\024\255\000\000\000\000\046\255\058\255\000\000\222\000\222\000\
\001\255\053\255\000\000\059\255\062\255\000\000\222\000\075\255\
\079\255\000\000\000\000\000\000\222\000\222\000\080\255\043\255\
\056\255\000\000\135\255\000\000\000\000\135\255\135\255\089\255\
\091\255\092\255\135\255\000\000\000\000\010\255\000\000\000\000\
\232\255\000\000\000\000\000\000\188\000\097\255\000\000\000\000\
\135\255\135\255\135\255\252\255\135\255\036\255\000\000\135\255\
\135\255\135\255\135\255\135\255\135\255\135\255\135\255\135\255\
\135\255\135\255\135\255\135\255\135\255\000\000\000\000\209\000\
\230\000\009\001\093\255\000\000\009\001\099\255\100\255\000\000\
\000\000\002\255\000\000\002\255\000\000\060\001\009\001\060\001\
\041\255\041\255\041\255\041\255\044\001\028\001\095\255\095\255\
\135\255\000\000\135\255\078\255\000\000\106\255\009\001\095\255\
\135\255\000\000\112\255\095\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\121\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\119\255\000\000\121\255\121\255\
\000\000\125\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\110\255\110\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\212\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\128\255\000\000\127\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\077\255\000\000\000\000\012\255\000\000\129\255\000\000\
\000\000\019\000\000\000\042\000\000\000\157\000\086\255\161\000\
\065\000\088\000\111\000\134\000\175\000\060\255\000\000\000\000\
\128\255\000\000\000\000\151\255\000\000\000\000\020\255\000\000\
\130\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\213\255\000\000\148\255\136\000\000\000\000\000\015\000\
\100\000\233\255\000\000\146\255\000\000\101\000\000\000"

let yytablesize = 594
let yytable = "\061\000\
\116\000\117\000\063\000\064\000\118\000\001\000\020\000\068\000\
\072\000\122\000\069\000\074\000\123\000\125\000\041\000\076\000\
\040\000\017\000\021\000\040\000\062\000\088\000\089\000\090\000\
\041\000\093\000\022\000\041\000\097\000\098\000\099\000\100\000\
\101\000\102\000\103\000\104\000\105\000\106\000\107\000\108\000\
\109\000\110\000\070\000\043\000\044\000\034\000\023\000\072\000\
\045\000\073\000\074\000\039\000\039\000\075\000\076\000\046\000\
\043\000\044\000\024\000\030\000\031\000\060\000\017\000\032\000\
\017\000\047\000\033\000\017\000\046\000\090\000\048\000\119\000\
\049\000\050\000\051\000\017\000\035\000\090\000\047\000\026\000\
\036\000\026\000\042\000\048\000\017\000\049\000\050\000\051\000\
\022\000\065\000\022\000\066\000\067\000\022\000\096\000\113\000\
\044\000\043\000\044\000\052\000\053\000\054\000\087\000\114\000\
\055\000\056\000\120\000\115\000\121\000\046\000\046\000\046\000\
\052\000\053\000\054\000\046\000\124\000\055\000\056\000\047\000\
\065\000\056\000\046\000\029\000\048\000\044\000\049\000\050\000\
\051\000\045\000\025\000\038\000\046\000\039\000\025\000\043\000\
\038\000\046\000\016\000\046\000\046\000\046\000\000\000\000\000\
\000\000\000\000\000\000\046\000\000\000\000\000\000\000\052\000\
\052\000\052\000\053\000\054\000\052\000\047\000\055\000\056\000\
\000\000\000\000\000\000\052\000\000\000\000\000\046\000\046\000\
\046\000\000\000\000\000\046\000\046\000\052\000\000\000\000\000\
\000\000\000\000\052\000\000\000\052\000\052\000\052\000\004\000\
\005\000\006\000\007\000\008\000\009\000\010\000\011\000\052\000\
\053\000\054\000\000\000\012\000\055\000\056\000\013\000\014\000\
\000\000\000\000\000\000\015\000\000\000\000\000\000\000\052\000\
\052\000\052\000\000\000\000\000\052\000\052\000\003\000\000\000\
\003\000\000\000\003\000\003\000\003\000\003\000\000\000\000\000\
\003\000\003\000\003\000\003\000\000\000\003\000\003\000\003\000\
\003\000\003\000\071\000\003\000\003\000\000\000\072\000\000\000\
\073\000\074\000\000\000\000\000\075\000\076\000\077\000\078\000\
\000\000\079\000\080\000\081\000\082\000\083\000\092\000\084\000\
\085\000\000\000\072\000\000\000\073\000\074\000\000\000\000\000\
\075\000\076\000\077\000\078\000\000\000\079\000\080\000\081\000\
\082\000\083\000\000\000\084\000\085\000\006\000\000\000\006\000\
\000\000\000\000\006\000\006\000\000\000\000\000\000\000\006\000\
\000\000\006\000\006\000\000\000\006\000\006\000\006\000\006\000\
\006\000\000\000\006\000\006\000\007\000\000\000\007\000\000\000\
\000\000\007\000\007\000\000\000\000\000\000\000\007\000\000\000\
\007\000\007\000\000\000\007\000\007\000\007\000\007\000\007\000\
\000\000\007\000\007\000\013\000\000\000\013\000\000\000\000\000\
\013\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\013\000\000\000\013\000\013\000\013\000\013\000\013\000\000\000\
\013\000\013\000\014\000\000\000\014\000\000\000\000\000\014\000\
\000\000\000\000\000\000\000\000\000\000\000\000\014\000\014\000\
\000\000\014\000\014\000\014\000\014\000\014\000\000\000\014\000\
\014\000\011\000\000\000\011\000\000\000\000\000\011\000\000\000\
\000\000\000\000\000\000\000\000\000\000\011\000\011\000\000\000\
\011\000\011\000\011\000\011\000\011\000\000\000\011\000\011\000\
\012\000\000\000\012\000\000\000\000\000\012\000\000\000\000\000\
\000\000\000\000\000\000\000\000\012\000\012\000\000\000\012\000\
\012\000\012\000\012\000\012\000\000\000\012\000\012\000\015\000\
\000\000\015\000\000\000\016\000\015\000\016\000\000\000\000\000\
\016\000\000\000\000\000\015\000\015\000\000\000\015\000\016\000\
\016\000\018\000\016\000\018\000\015\000\015\000\018\000\000\000\
\016\000\016\000\000\000\000\000\000\000\000\000\018\000\000\000\
\086\000\000\000\072\000\000\000\073\000\074\000\018\000\018\000\
\075\000\076\000\077\000\078\000\000\000\079\000\080\000\081\000\
\082\000\083\000\000\000\084\000\085\000\111\000\000\000\072\000\
\000\000\073\000\074\000\000\000\000\000\075\000\076\000\077\000\
\078\000\000\000\079\000\080\000\081\000\082\000\083\000\000\000\
\084\000\085\000\112\000\000\000\072\000\000\000\073\000\074\000\
\000\000\000\000\075\000\076\000\077\000\078\000\000\000\079\000\
\080\000\081\000\082\000\083\000\000\000\084\000\085\000\004\000\
\005\000\006\000\007\000\008\000\009\000\010\000\011\000\000\000\
\000\000\000\000\000\000\012\000\000\000\000\000\013\000\072\000\
\000\000\073\000\074\000\015\000\000\000\075\000\076\000\077\000\
\078\000\000\000\079\000\080\000\081\000\082\000\083\000\000\000\
\084\000\085\000\072\000\000\000\073\000\074\000\000\000\000\000\
\075\000\076\000\077\000\000\000\000\000\079\000\080\000\081\000\
\082\000\083\000\072\000\084\000\073\000\074\000\000\000\000\000\
\075\000\076\000\077\000\000\000\000\000\079\000\080\000\081\000\
\082\000\083\000\072\000\000\000\073\000\074\000\000\000\000\000\
\075\000\076\000\000\000\000\000\000\000\000\000\080\000\081\000\
\082\000\083\000"

let yycheck = "\043\000\
\111\000\112\000\046\000\047\000\113\000\001\000\059\001\051\000\
\007\001\120\000\001\001\010\001\121\000\124\000\038\000\014\001\
\005\001\003\000\059\001\008\001\044\000\065\000\066\000\067\000\
\005\001\069\000\003\001\008\001\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\079\000\080\000\081\000\082\000\083\000\
\084\000\085\000\033\001\001\001\002\001\031\000\001\001\007\001\
\006\001\009\001\010\001\037\000\038\000\013\001\014\001\013\001\
\001\001\002\001\001\001\059\001\008\001\006\001\003\001\005\001\
\005\001\023\001\005\001\008\001\013\001\113\000\028\001\115\000\
\030\001\031\001\032\001\016\001\002\001\121\000\023\001\003\001\
\002\001\005\001\003\001\028\001\025\001\030\001\031\001\032\001\
\003\001\001\001\005\001\001\001\001\001\008\001\059\001\003\001\
\002\001\001\001\002\001\057\001\058\001\059\001\006\001\005\001\
\062\001\063\001\029\001\008\001\003\001\013\001\001\001\002\001\
\057\001\058\001\059\001\006\001\005\001\062\001\063\001\023\001\
\000\000\003\001\013\001\024\000\028\001\005\001\030\001\031\001\
\032\001\005\001\003\001\005\001\023\001\005\001\005\001\001\001\
\036\000\028\001\003\000\030\001\031\001\032\001\255\255\255\255\
\255\255\255\255\255\255\013\001\255\255\255\255\255\255\001\001\
\002\001\057\001\058\001\059\001\006\001\023\001\062\001\063\001\
\255\255\255\255\255\255\013\001\255\255\255\255\057\001\058\001\
\059\001\255\255\255\255\062\001\063\001\023\001\255\255\255\255\
\255\255\255\255\028\001\255\255\030\001\031\001\032\001\034\001\
\035\001\036\001\037\001\038\001\039\001\040\001\041\001\057\001\
\058\001\059\001\255\255\046\001\062\001\063\001\049\001\050\001\
\255\255\255\255\255\255\054\001\255\255\255\255\255\255\057\001\
\058\001\059\001\255\255\255\255\062\001\063\001\003\001\255\255\
\005\001\255\255\007\001\008\001\009\001\010\001\255\255\255\255\
\013\001\014\001\015\001\016\001\255\255\018\001\019\001\020\001\
\021\001\022\001\003\001\024\001\025\001\255\255\007\001\255\255\
\009\001\010\001\255\255\255\255\013\001\014\001\015\001\016\001\
\255\255\018\001\019\001\020\001\021\001\022\001\003\001\024\001\
\025\001\255\255\007\001\255\255\009\001\010\001\255\255\255\255\
\013\001\014\001\015\001\016\001\255\255\018\001\019\001\020\001\
\021\001\022\001\255\255\024\001\025\001\003\001\255\255\005\001\
\255\255\255\255\008\001\009\001\255\255\255\255\255\255\013\001\
\255\255\015\001\016\001\255\255\018\001\019\001\020\001\021\001\
\022\001\255\255\024\001\025\001\003\001\255\255\005\001\255\255\
\255\255\008\001\009\001\255\255\255\255\255\255\013\001\255\255\
\015\001\016\001\255\255\018\001\019\001\020\001\021\001\022\001\
\255\255\024\001\025\001\003\001\255\255\005\001\255\255\255\255\
\008\001\255\255\255\255\255\255\255\255\255\255\255\255\015\001\
\016\001\255\255\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\025\001\003\001\255\255\005\001\255\255\255\255\008\001\
\255\255\255\255\255\255\255\255\255\255\255\255\015\001\016\001\
\255\255\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\025\001\003\001\255\255\005\001\255\255\255\255\008\001\255\255\
\255\255\255\255\255\255\255\255\255\255\015\001\016\001\255\255\
\018\001\019\001\020\001\021\001\022\001\255\255\024\001\025\001\
\003\001\255\255\005\001\255\255\255\255\008\001\255\255\255\255\
\255\255\255\255\255\255\255\255\015\001\016\001\255\255\018\001\
\019\001\020\001\021\001\022\001\255\255\024\001\025\001\003\001\
\255\255\005\001\255\255\003\001\008\001\005\001\255\255\255\255\
\008\001\255\255\255\255\015\001\016\001\255\255\018\001\015\001\
\016\001\003\001\018\001\005\001\024\001\025\001\008\001\255\255\
\024\001\025\001\255\255\255\255\255\255\255\255\016\001\255\255\
\005\001\255\255\007\001\255\255\009\001\010\001\024\001\025\001\
\013\001\014\001\015\001\016\001\255\255\018\001\019\001\020\001\
\021\001\022\001\255\255\024\001\025\001\005\001\255\255\007\001\
\255\255\009\001\010\001\255\255\255\255\013\001\014\001\015\001\
\016\001\255\255\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\025\001\005\001\255\255\007\001\255\255\009\001\010\001\
\255\255\255\255\013\001\014\001\015\001\016\001\255\255\018\001\
\019\001\020\001\021\001\022\001\255\255\024\001\025\001\034\001\
\035\001\036\001\037\001\038\001\039\001\040\001\041\001\255\255\
\255\255\255\255\255\255\046\001\255\255\255\255\049\001\007\001\
\255\255\009\001\010\001\054\001\255\255\013\001\014\001\015\001\
\016\001\255\255\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\025\001\007\001\255\255\009\001\010\001\255\255\255\255\
\013\001\014\001\015\001\255\255\255\255\018\001\019\001\020\001\
\021\001\022\001\007\001\024\001\009\001\010\001\255\255\255\255\
\013\001\014\001\015\001\255\255\255\255\018\001\019\001\020\001\
\021\001\022\001\007\001\255\255\009\001\010\001\255\255\255\255\
\013\001\014\001\255\255\255\255\255\255\255\255\019\001\020\001\
\021\001\022\001"

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
# 57 "parser.mly"
                                         ( Literal(_1) )
# 472 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parser.mly"
                                         ( Char(_1) )
# 479 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "parser.mly"
                                         ( Id(_1))
# 486 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 60 "parser.mly"
                                         ( String_Lit(_1) )
# 493 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 61 "parser.mly"
                                         ( Bool_Lit(_1))
# 500 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 62 "parser.mly"
                                         ( Binop (_1, Add, _3) )
# 508 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
                                        ( Binop (_1, Sub, _3) )
# 516 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                                        ( Binop (_1, Mult, _3) )
# 524 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                                         ( Binop (_1, Div, _3) )
# 532 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                                       ( Binop (_1, Mod, _1) )
# 540 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                                        ( Binop (_1, Less, _3) )
# 548 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                                        ( Binop (_1, Greater, _3) )
# 556 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                                       ( Binop (_1, Leq, _3) )
# 564 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                                       ( Binop (_1, Geq, _3) )
# 572 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                                         ( Binop (_1, Equal, _3) )
# 580 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                                       ( Binop (_1, Neq, _3) )
# 588 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                                        ( Binop (_1, Or, _3) )
# 596 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
                                       ( Binop (_1, And, _3) )
# 604 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 75 "parser.mly"
                                         ( Add_at(_1, _3) )
# 612 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                                       ( Unop(Not, _2) )
# 619 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                                         ( Unop(Neg, _2) )
# 626 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "parser.mly"
                                         ( Assign(_1, _3) )
# 634 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 79 "parser.mly"
                                         ( Call(_1, _3) )
# 642 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                                         ( _2 )
# 649 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
                  ( Noexpr )
# 655 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
                  ( _1 )
# 662 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
           ( Int )
# 668 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
           ( Char )
# 674 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
           ( String )
# 680 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
           ( Bool )
# 686 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
           ( Node )
# 692 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
           ( Edge )
# 698 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
           ( Graph )
# 704 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
            ( Int_at )
# 710 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
             ( Bool_at )
# 716 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
             ( String_at )
# 722 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
             ( Char_at )
# 728 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser.mly"
                 ( [] )
# 734 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 101 "parser.mly"
                 ( List.rev _1 )
# 741 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                            ( [_1] )
# 748 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                            ( _3 :: _1 )
# 756 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 108 "parser.mly"
                             ( [_1])
# 763 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 109 "parser.mly"
                             ( _3 :: _1 )
# 771 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
                 ( [] )
# 777 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 113 "parser.mly"
                 ( List.rev _1 )
# 784 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "parser.mly"
                   ( [] )
# 790 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 117 "parser.mly"
                   ( _2 :: _1 )
# 798 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 120 "parser.mly"
                          ( {locals = []; statements = List.rev _2; block_num = inc_block_num ()} )
# 805 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 123 "parser.mly"
                                                                  ( Block(_1))
# 812 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                                                                  ( Expr(_1) )
# 819 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                                                                  ( Return(_2) )
# 826 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 127 "parser.mly"
                                                                  ( If(_3, _5, {locals = []; statements = []; block_num = inc_block_num ()}) )
# 834 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'block) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 128 "parser.mly"
                                                                  ( If (_3, _5, _7) )
# 843 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 129 "parser.mly"
                                                                  ( For(_3, _5, _7, _9) )
# 853 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 130 "parser.mly"
                                                                  ( While(_3, _5) )
# 861 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'any_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 133 "parser.mly"
              (_2, _1)
# 869 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl) in
    Obj.repr(
# 136 "parser.mly"
             ( _1 )
# 876 "parser.ml"
               : 'glb_vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "parser.mly"
                          ( [] )
# 882 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl) in
    Obj.repr(
# 141 "parser.mly"
                          ( _2 :: _1 )
# 890 "parser.ml"
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
# 904 "parser.ml"
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
# 917 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 156 "parser.mly"
                 ( [], [] )
# 923 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'glb_vdecl) in
    Obj.repr(
# 157 "parser.mly"
                     ( (_2 :: fst _1), snd _1 )
# 931 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 158 "parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 939 "parser.ml"
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
