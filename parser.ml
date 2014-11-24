type token =
  | LPAREN
  | LBRACE
  | SEMI
  | COLON
  | RPAREN
  | RBRACE
  | MOD
  | COMMA
  | PERIOD
  | EOF
  | PLUS
  | TIMES
  | LINK
  | BILINK
  | MINUS
  | DIVIDE
  | EQ
  | ASSIGN
  | NEQ
  | LEQ
  | GEQ
  | LT
  | GT
  | NOT
  | AND
  | OR
  | IF
  | ELSE
  | WHILE
  | FOR
  | RETURN
  | GRAPH
  | NODETYPE
  | EDGETYPE
  | NODE
  | BOOL
  | STRING
  | PRINT
  | NEW
  | CONTINUE
  | DOUBLE
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
  | ID of (string)
  | CHARLIT of (string)
  | STRINGLIT of (string)

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 63 "parser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* LBRACE *);
  259 (* SEMI *);
  260 (* COLON *);
  261 (* RPAREN *);
  262 (* RBRACE *);
  263 (* MOD *);
  264 (* COMMA *);
  265 (* PERIOD *);
    0 (* EOF *);
  266 (* PLUS *);
  267 (* TIMES *);
  268 (* LINK *);
  269 (* BILINK *);
  270 (* MINUS *);
  271 (* DIVIDE *);
  272 (* EQ *);
  273 (* ASSIGN *);
  274 (* NEQ *);
  275 (* LEQ *);
  276 (* GEQ *);
  277 (* LT *);
  278 (* GT *);
  279 (* NOT *);
  280 (* AND *);
  281 (* OR *);
  282 (* IF *);
  283 (* ELSE *);
  284 (* WHILE *);
  285 (* FOR *);
  286 (* RETURN *);
  287 (* GRAPH *);
  288 (* NODETYPE *);
  289 (* EDGETYPE *);
  290 (* NODE *);
  291 (* BOOL *);
  292 (* STRING *);
  293 (* PRINT *);
  294 (* NEW *);
  295 (* CONTINUE *);
  296 (* DOUBLE *);
  297 (* FALSE *);
  298 (* TRUE *);
  299 (* INT *);
  300 (* VOID *);
  301 (* DEST *);
  302 (* EDGES *);
  303 (* STATIC *);
  304 (* CHAR *);
  305 (* DO *);
  306 (* IN *);
    0|]

let yytransl_block = [|
  307 (* LITERAL *);
  308 (* ID *);
  309 (* CHARLIT *);
  310 (* STRINGLIT *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\003\000\003\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\005\000\005\000\006\000\007\000\007\000\008\000\008\000\
\009\000\009\000\010\000\010\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\012\000\012\000\012\000\012\000\012\000\
\012\000\013\000\013\000\014\000\015\000\015\000\015\000\001\000\
\001\000\001\000\000\000"

let yylen = "\002\000\
\000\000\001\000\001\000\003\000\001\000\001\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\004\000\003\000\
\001\000\001\000\001\000\004\000\000\000\001\000\001\000\003\000\
\000\000\001\000\000\000\002\000\002\000\003\000\003\000\005\000\
\007\000\009\000\005\000\003\000\005\000\003\000\003\000\003\000\
\003\000\000\000\002\000\007\000\003\000\003\000\003\000\000\000\
\002\000\002\000\002\000"

let yydefred = "\000\000\
\056\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\057\000\058\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\031\000\000\000\000\000\046\000\048\000\047\000\
\053\000\044\000\000\000\055\000\054\000\049\000\000\000\000\000\
\000\000\000\000\005\000\000\000\006\000\007\000\000\000\025\000\
\027\000\032\000\050\000\000\000\022\000\000\000\000\000\045\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\
\000\000\000\000\000\000\000\000\012\000\000\000\010\000\000\000\
\011\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\051\000\023\000\000\000\
\028\000\000\000\000\000\035\000\052\000\000\000\000\000\000\000\
\000\000\000\000\036\000\000\000\000\000\000\000\000\000\000\000\
\000\000\037\000\039\000\000\000\000\000\000\000\000\000\038\000\
\000\000\000\000\000\000\000\000\043\000\000\000\000\000\000\000\
\041\000\000\000\000\000\042\000"

let yydgoto = "\002\000\
\003\000\065\000\066\000\098\000\040\000\041\000\111\000\020\000\
\021\000\085\000\099\000\010\000\063\000\011\000\012\000"

let yysindex = "\011\000\
\000\000\000\000\100\000\240\254\008\255\028\255\029\255\040\255\
\052\255\000\000\000\000\060\255\100\255\110\255\112\255\001\255\
\119\255\002\255\000\000\113\255\117\255\000\000\000\000\000\000\
\000\000\000\000\096\255\000\000\000\000\000\000\077\255\126\255\
\096\255\096\255\000\000\046\255\000\000\000\000\176\255\000\000\
\000\000\000\000\000\000\146\000\000\000\096\255\096\255\000\000\
\096\255\096\255\096\255\096\255\096\255\096\255\096\255\096\255\
\096\255\096\255\096\255\096\255\096\255\096\255\004\001\000\000\
\130\255\123\255\224\000\208\000\000\000\076\255\000\000\076\255\
\000\000\016\001\224\000\016\001\018\001\018\001\018\001\018\001\
\000\001\240\000\084\255\085\255\015\255\000\000\000\000\096\255\
\000\000\254\254\135\255\000\000\000\000\142\255\143\255\144\255\
\096\255\196\255\000\000\224\000\056\255\096\255\096\255\096\255\
\216\255\000\000\000\000\167\000\188\000\224\000\148\255\000\000\
\104\255\104\255\096\255\125\255\000\000\151\255\104\255\096\255\
\000\000\141\255\104\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\153\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\155\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\160\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\156\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\163\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\072\255\000\000\
\000\000\164\255\041\255\000\000\000\000\239\255\000\000\006\000\
\000\000\047\255\005\255\121\000\029\000\052\000\075\000\098\000\
\125\000\034\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\091\255\000\000\000\000\000\000\179\255\
\000\000\000\000\000\000\000\000\000\000\051\255\000\000\000\000\
\000\000\000\000\179\255\088\255\000\000\000\000\000\000\180\255\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\229\255\000\000\000\000\150\255\000\000\
\000\000\092\000\188\001\126\000\000\000\000\000\000\000"

let yytablesize = 567
let yytable = "\039\000\
\026\000\025\000\029\000\026\000\030\000\044\000\045\000\021\000\
\118\000\021\000\021\000\001\000\021\000\122\000\027\000\033\000\
\092\000\027\000\067\000\068\000\093\000\069\000\070\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\079\000\
\080\000\081\000\082\000\013\000\019\000\034\000\019\000\019\000\
\094\000\019\000\095\000\096\000\097\000\003\000\046\000\047\000\
\003\000\017\000\019\000\017\000\017\000\030\000\017\000\030\000\
\033\000\092\000\019\000\014\000\100\000\107\000\017\000\017\000\
\017\000\035\000\036\000\037\000\038\000\105\000\017\000\017\000\
\035\000\035\000\108\000\109\000\110\000\035\000\034\000\015\000\
\016\000\094\000\049\000\095\000\096\000\097\000\051\000\110\000\
\040\000\040\000\053\000\017\000\110\000\040\000\035\000\004\000\
\033\000\035\000\004\000\035\000\035\000\035\000\022\000\018\000\
\033\000\092\000\035\000\036\000\037\000\038\000\040\000\019\000\
\023\000\040\000\024\000\040\000\040\000\040\000\034\000\028\000\
\031\000\032\000\035\000\035\000\035\000\035\000\034\000\043\000\
\042\000\094\000\088\000\095\000\096\000\097\000\087\000\090\000\
\091\000\030\000\040\000\040\000\040\000\040\000\102\000\103\000\
\104\000\123\000\035\000\036\000\037\000\038\000\115\000\119\000\
\059\000\120\000\035\000\036\000\037\000\038\000\026\000\033\000\
\026\000\026\000\026\000\026\000\034\000\026\000\026\000\001\000\
\002\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
\026\000\026\000\048\000\026\000\026\000\029\000\049\000\101\000\
\029\000\050\000\051\000\000\000\086\000\052\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\106\000\061\000\
\062\000\000\000\049\000\000\000\000\000\050\000\051\000\000\000\
\000\000\052\000\053\000\054\000\055\000\056\000\057\000\058\000\
\059\000\060\000\112\000\061\000\062\000\000\000\049\000\000\000\
\000\000\050\000\051\000\000\000\000\000\052\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\000\000\061\000\
\062\000\008\000\000\000\008\000\008\000\000\000\008\000\000\000\
\008\000\000\000\000\000\000\000\008\000\000\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\000\000\008\000\008\000\
\009\000\000\000\009\000\009\000\000\000\009\000\000\000\009\000\
\000\000\000\000\000\000\009\000\000\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\000\000\009\000\009\000\015\000\
\000\000\015\000\015\000\000\000\015\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\000\000\015\000\015\000\016\000\000\000\
\016\000\016\000\000\000\016\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\016\000\016\000\016\000\016\000\016\000\
\016\000\016\000\000\000\016\000\016\000\013\000\000\000\013\000\
\013\000\000\000\013\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\000\000\013\000\013\000\014\000\000\000\014\000\014\000\
\000\000\014\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\000\000\014\000\014\000\018\000\000\000\018\000\018\000\020\000\
\018\000\020\000\020\000\000\000\020\000\000\000\004\000\005\000\
\018\000\018\000\018\000\006\000\000\000\020\000\007\000\008\000\
\018\000\018\000\000\000\009\000\020\000\020\000\064\000\000\000\
\049\000\000\000\000\000\050\000\051\000\000\000\000\000\052\000\
\053\000\054\000\055\000\056\000\057\000\058\000\059\000\060\000\
\000\000\061\000\062\000\113\000\000\000\049\000\000\000\000\000\
\050\000\051\000\000\000\000\000\052\000\053\000\054\000\055\000\
\056\000\057\000\058\000\059\000\060\000\000\000\061\000\062\000\
\114\000\000\000\049\000\000\000\000\000\050\000\051\000\000\000\
\000\000\052\000\053\000\054\000\055\000\056\000\057\000\058\000\
\059\000\060\000\000\000\061\000\062\000\089\000\049\000\000\000\
\000\000\050\000\051\000\000\000\000\000\052\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\049\000\061\000\
\062\000\050\000\051\000\000\000\000\000\052\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\049\000\061\000\
\062\000\050\000\051\000\000\000\000\000\052\000\053\000\054\000\
\000\000\056\000\057\000\058\000\059\000\060\000\049\000\061\000\
\000\000\050\000\051\000\000\000\000\000\052\000\053\000\054\000\
\000\000\056\000\057\000\058\000\059\000\060\000\049\000\000\000\
\049\000\050\000\051\000\050\000\051\000\052\000\053\000\052\000\
\053\000\000\000\057\000\058\000\059\000\060\000\004\000\005\000\
\000\000\000\000\000\000\006\000\116\000\117\000\083\000\000\000\
\000\000\000\000\121\000\084\000\000\000\000\000\124\000"

let yycheck = "\027\000\
\003\001\001\001\001\001\003\001\003\001\033\000\034\000\003\001\
\115\000\005\001\006\001\001\000\008\001\120\000\017\001\001\001\
\002\001\017\001\046\000\047\000\006\001\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\056\000\057\000\058\000\059\000\
\060\000\061\000\062\000\052\001\003\001\023\001\005\001\006\001\
\026\001\008\001\028\001\029\001\030\001\005\001\001\001\002\001\
\008\001\003\001\017\001\005\001\006\001\003\001\008\001\005\001\
\001\001\002\001\025\001\052\001\088\000\006\001\016\001\017\001\
\018\001\051\001\052\001\053\001\054\001\097\000\024\001\025\001\
\001\001\002\001\102\000\103\000\104\000\006\001\023\001\052\001\
\052\001\026\001\007\001\028\001\029\001\030\001\011\001\115\000\
\001\001\002\001\015\001\052\001\120\000\006\001\023\001\005\001\
\001\001\026\001\008\001\028\001\029\001\030\001\003\001\052\001\
\001\001\002\001\051\001\052\001\053\001\054\001\023\001\052\001\
\003\001\026\001\003\001\028\001\029\001\030\001\023\001\001\001\
\008\001\005\001\051\001\052\001\053\001\054\001\023\001\002\001\
\052\001\026\001\008\001\028\001\029\001\030\001\005\001\052\001\
\052\001\003\001\051\001\052\001\053\001\054\001\001\001\001\001\
\001\001\005\001\051\001\052\001\053\001\054\001\003\001\027\001\
\000\000\003\001\051\001\052\001\053\001\054\001\003\001\005\001\
\005\001\006\001\007\001\008\001\005\001\010\001\011\001\005\001\
\005\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\003\001\024\001\025\001\003\001\007\001\092\000\
\005\001\010\001\011\001\255\255\063\000\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\003\001\024\001\
\025\001\255\255\007\001\255\255\255\255\010\001\011\001\255\255\
\255\255\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\003\001\024\001\025\001\255\255\007\001\255\255\
\255\255\010\001\011\001\255\255\255\255\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\025\001\003\001\255\255\005\001\006\001\255\255\008\001\255\255\
\010\001\255\255\255\255\255\255\014\001\255\255\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\255\255\024\001\025\001\
\003\001\255\255\005\001\006\001\255\255\008\001\255\255\010\001\
\255\255\255\255\255\255\014\001\255\255\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\255\255\024\001\025\001\003\001\
\255\255\005\001\006\001\255\255\008\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\255\255\024\001\025\001\003\001\255\255\
\005\001\006\001\255\255\008\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\255\255\024\001\025\001\003\001\255\255\005\001\
\006\001\255\255\008\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\255\255\024\001\025\001\003\001\255\255\005\001\006\001\
\255\255\008\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\255\255\024\001\025\001\003\001\255\255\005\001\006\001\003\001\
\008\001\005\001\006\001\255\255\008\001\255\255\035\001\036\001\
\016\001\017\001\018\001\040\001\255\255\017\001\043\001\044\001\
\024\001\025\001\255\255\048\001\024\001\025\001\005\001\255\255\
\007\001\255\255\255\255\010\001\011\001\255\255\255\255\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\255\255\024\001\025\001\005\001\255\255\007\001\255\255\255\255\
\010\001\011\001\255\255\255\255\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\255\255\024\001\025\001\
\005\001\255\255\007\001\255\255\255\255\010\001\011\001\255\255\
\255\255\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\255\255\024\001\025\001\006\001\007\001\255\255\
\255\255\010\001\011\001\255\255\255\255\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\007\001\024\001\
\025\001\010\001\011\001\255\255\255\255\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\007\001\024\001\
\025\001\010\001\011\001\255\255\255\255\014\001\015\001\016\001\
\255\255\018\001\019\001\020\001\021\001\022\001\007\001\024\001\
\255\255\010\001\011\001\255\255\255\255\014\001\015\001\016\001\
\255\255\018\001\019\001\020\001\021\001\022\001\007\001\255\255\
\007\001\010\001\011\001\010\001\011\001\014\001\015\001\014\001\
\015\001\255\255\019\001\020\001\021\001\022\001\035\001\036\001\
\255\255\255\255\255\255\040\001\113\000\114\000\043\001\255\255\
\255\255\255\255\119\000\048\001\255\255\255\255\123\000"

let yynames_const = "\
  LPAREN\000\
  LBRACE\000\
  SEMI\000\
  COLON\000\
  RPAREN\000\
  RBRACE\000\
  MOD\000\
  COMMA\000\
  PERIOD\000\
  EOF\000\
  PLUS\000\
  TIMES\000\
  LINK\000\
  BILINK\000\
  MINUS\000\
  DIVIDE\000\
  EQ\000\
  ASSIGN\000\
  NEQ\000\
  LEQ\000\
  GEQ\000\
  LT\000\
  GT\000\
  NOT\000\
  AND\000\
  OR\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  FOR\000\
  RETURN\000\
  GRAPH\000\
  NODETYPE\000\
  EDGETYPE\000\
  NODE\000\
  BOOL\000\
  STRING\000\
  PRINT\000\
  NEW\000\
  CONTINUE\000\
  DOUBLE\000\
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
  ID\000\
  CHARLIT\000\
  STRINGLIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "parser.mly"
                ( [] )
# 421 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 37 "parser.mly"
                 ( List.rev _1 )
# 428 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 40 "parser.mly"
       ( [_1] )
# 435 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 41 "parser.mly"
                            ( _3 :: _1 )
# 443 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 44 "parser.mly"
                  ( Literal(_1) )
# 450 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "parser.mly"
            ( Char(_1) )
# 457 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 46 "parser.mly"
             ( String(_1) )
# 464 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 47 "parser.mly"
                 ( Binop (_1, Add, _3) )
# 472 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 48 "parser.mly"
                  ( Binop (_1, Sub, _3) )
# 480 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 49 "parser.mly"
                  ( Binop (_1, Mult, _3) )
# 488 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 50 "parser.mly"
                  ( Binop (_1, Div, _3) )
# 496 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 51 "parser.mly"
                 ( Binop (_1, Mod, _1) )
# 504 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 52 "parser.mly"
                ( Binop (_1, Less, _3) )
# 512 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 53 "parser.mly"
                ( Binop (_1, Greater, _3) )
# 520 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 54 "parser.mly"
                 ( Binop (_1, Leq, _3) )
# 528 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 55 "parser.mly"
                 ( Binop (_1, Geq, _3) )
# 536 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 56 "parser.mly"
                ( Binop (_1, Equal, _3) )
# 544 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 57 "parser.mly"
                 ( Binop (_1, Neq, _3) )
# 552 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 58 "parser.mly"
                ( Binop (_1, Or, _3) )
# 560 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 59 "parser.mly"
                 ( Binop (_1, And, _3) )
# 568 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 60 "parser.mly"
                  ( Assign(_1, _3) )
# 576 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 61 "parser.mly"
                 ( Not(_2) )
# 583 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 62 "parser.mly"
                              ( Call(_1, _3) )
# 591 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
                    ( _2 )
# 598 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 64 "parser.mly"
      ( _1 )
# 605 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 68 "parser.mly"
                ( Id(_1) )
# 612 "parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arr) in
    Obj.repr(
# 69 "parser.mly"
                ( Array( fst _1, snd _1) )
# 619 "parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                              ( Id(_1),_3 )
# 627 "parser.ml"
               : 'arr))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
                (Noexpr )
# 633 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
         ( _1 )
# 640 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 79 "parser.mly"
     ( [_1] )
# 647 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "parser.mly"
                         ( _3 :: _1 )
# 655 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
                ( [] )
# 661 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 84 "parser.mly"
                ( List.rev _1 )
# 668 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
                ( [] )
# 674 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 88 "parser.mly"
                   ( _2 :: _1 )
# 682 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
            ( Expr(_1) )
# 689 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
                     ( Return(_2) )
# 696 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 93 "parser.mly"
                            ( Block(List.rev _2) )
# 703 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 94 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 711 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 95 "parser.mly"
                                         ( If (_3, _5, _7) )
# 720 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 97 "parser.mly"
   ( For(_3, _5, _7, _9) )
# 730 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 98 "parser.mly"
                                  ( While(_3, _5) )
# 738 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 101 "parser.mly"
              ( _2 )
# 745 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                            ( _2 )
# 753 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 103 "parser.mly"
                 ( _2 )
# 760 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 104 "parser.mly"
                   ( _2 )
# 767 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 105 "parser.mly"
                   ( _2 )
# 774 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 106 "parser.mly"
                 ( _2 )
# 781 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "parser.mly"
                ( [] )
# 787 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 110 "parser.mly"
                     ( _2 :: _1 )
# 795 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'retval) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 114 "parser.mly"
     ( { fname = snd _1;
         formals = _2; 
         locals = List.rev _5;
         body = List.rev _6;
         ret = fst _1
         } )
# 810 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 122 "parser.mly"
                      ( [Int], _2  )
# 817 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 123 "parser.mly"
                        ( [Char], _2  )
# 824 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 124 "parser.mly"
                        ( [Void], _2  )
# 831 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    Obj.repr(
# 128 "parser.mly"
               ( [], [] )
# 837 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 129 "parser.mly"
                 ( (_2 :: fst _1), snd _1 )
# 845 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 130 "parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 853 "parser.ml"
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
