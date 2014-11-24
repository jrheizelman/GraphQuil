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
  | TYPEID of (string)
  | CHARLIT of (string)
  | STRINGLIT of (string)

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 66 "parser.ml"
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
  289 (* GRAPH *);
  290 (* NODETYPE *);
  291 (* EDGETYPE *);
  292 (* NODE *);
  293 (* BOOL *);
  294 (* STRING *);
  295 (* PRINT *);
  296 (* NEW *);
  297 (* CONTINUE *);
  298 (* DOUBLE *);
  299 (* FALSE *);
  300 (* TRUE *);
  301 (* INT *);
  302 (* VOID *);
  303 (* DEST *);
  304 (* EDGES *);
  305 (* STATIC *);
  306 (* CHAR *);
  307 (* DO *);
  308 (* IN *);
    0|]

let yytransl_block = [|
  309 (* LITERAL *);
  310 (* ID *);
  311 (* TYPEID *);
  312 (* CHARLIT *);
  313 (* STRINGLIT *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\003\000\003\000\005\000\
\004\000\004\000\006\000\006\000\007\000\007\000\008\000\008\000\
\010\000\010\000\011\000\011\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\013\000\013\000\014\000\014\000\015\000\
\016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
\016\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\001\000\001\000\001\000\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\004\000\003\000\001\000\001\000\001\000\004\000\
\000\000\001\000\001\000\003\000\000\000\001\000\002\000\004\000\
\000\000\001\000\000\000\002\000\002\000\003\000\003\000\005\000\
\007\000\009\000\005\000\002\000\004\000\000\000\002\000\007\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\000\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\068\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\069\000\
\070\000\000\000\000\000\064\000\065\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\044\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\056\000\054\000\055\000\052\000\053\000\049\000\
\051\000\050\000\057\000\000\000\000\000\001\000\000\000\002\000\
\003\000\000\000\000\000\023\000\066\000\063\000\062\000\061\000\
\059\000\058\000\060\000\067\000\000\000\031\000\000\000\000\000\
\018\000\000\000\000\000\045\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\046\000\020\000\000\000\000\000\000\000\
\000\000\008\000\000\000\006\000\000\000\007\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\032\000\
\000\000\019\000\000\000\024\000\000\000\047\000\000\000\035\000\
\048\000\000\000\000\000\000\000\000\000\000\000\036\000\000\000\
\000\000\000\000\000\000\000\000\037\000\039\000\000\000\000\000\
\000\000\000\000\038\000\000\000\000\000\000\000\000\000\043\000\
\000\000\000\000\000\000\041\000\000\000\000\000\042\000"

let yydgoto = "\002\000\
\003\000\126\000\059\000\095\000\060\000\096\000\138\000\040\000\
\015\000\042\000\117\000\127\000\016\000\113\000\017\000\018\000"

let yysindex = "\002\000\
\000\000\000\000\154\000\212\254\227\254\233\254\213\254\238\254\
\244\254\023\255\028\255\029\255\037\255\055\255\002\255\000\000\
\000\000\177\000\112\255\000\000\000\000\113\255\121\255\124\255\
\132\255\141\255\144\255\145\255\148\255\000\000\067\255\072\255\
\097\255\100\255\102\255\104\255\107\255\111\255\130\255\159\255\
\135\255\173\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\067\255\067\255\000\000\000\255\000\000\
\000\000\121\000\175\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\177\000\000\000\194\255\216\000\
\000\000\067\255\067\255\000\000\067\255\067\255\067\255\067\255\
\067\255\067\255\067\255\067\255\067\255\067\255\067\255\067\255\
\067\255\067\255\143\255\000\000\000\000\041\001\196\255\200\255\
\021\001\000\000\125\255\000\000\125\255\000\000\092\001\092\001\
\206\255\206\255\206\255\206\255\076\001\060\001\041\001\000\000\
\177\000\000\000\067\255\000\000\007\255\000\000\041\001\000\000\
\000\000\201\255\224\255\231\255\067\255\141\000\000\000\039\255\
\067\255\067\255\067\255\161\000\000\000\000\000\237\000\002\001\
\041\001\234\255\000\000\087\255\087\255\067\255\210\255\000\000\
\237\255\087\255\067\255\000\000\238\255\087\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\245\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\242\255\003\255\000\000\000\000\012\255\013\255\017\255\
\063\255\069\255\000\000\070\255\071\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\244\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\161\255\000\000\
\000\000\000\000\185\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\245\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\026\255\000\000\251\255\
\000\000\000\000\209\255\000\000\233\255\000\000\097\000\101\000\
\001\000\025\000\049\000\073\000\255\254\147\255\218\255\000\000\
\074\255\000\000\000\000\000\000\000\000\000\000\073\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\
\098\255\000\000\000\000\000\000\000\000\004\000\106\255\000\000\
\000\000\000\000\003\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\225\255\000\000\000\000\000\000\000\000\126\255\000\000\
\252\255\000\000\146\000\007\000\148\000\000\000\000\000\000\000"

let yytablesize = 626
let yytable = "\058\000\
\074\000\016\000\001\000\016\000\030\000\066\000\016\000\052\000\
\120\000\019\000\022\000\145\000\121\000\041\000\063\000\062\000\
\149\000\031\000\066\000\061\000\072\000\073\000\016\000\016\000\
\016\000\020\000\075\000\063\000\062\000\053\000\027\000\021\000\
\061\000\027\000\122\000\023\000\123\000\124\000\125\000\052\000\
\120\000\024\000\094\000\097\000\134\000\098\000\099\000\100\000\
\101\000\102\000\103\000\104\000\105\000\106\000\107\000\108\000\
\109\000\110\000\111\000\054\000\055\000\053\000\056\000\057\000\
\091\000\059\000\122\000\052\000\123\000\124\000\125\000\058\000\
\060\000\067\000\035\000\035\000\025\000\028\000\059\000\035\000\
\028\000\026\000\027\000\119\000\058\000\060\000\067\000\052\000\
\120\000\053\000\028\000\054\000\055\000\132\000\056\000\057\000\
\035\000\135\000\136\000\137\000\030\000\035\000\030\000\035\000\
\035\000\035\000\040\000\040\000\029\000\053\000\137\000\040\000\
\043\000\044\000\122\000\137\000\123\000\124\000\125\000\054\000\
\055\000\045\000\056\000\057\000\046\000\061\000\035\000\035\000\
\040\000\035\000\035\000\077\000\047\000\040\000\079\000\040\000\
\040\000\040\000\081\000\054\000\055\000\048\000\056\000\057\000\
\049\000\050\000\143\000\144\000\051\000\015\000\062\000\015\000\
\148\000\063\000\015\000\064\000\151\000\065\000\040\000\040\000\
\066\000\040\000\040\000\022\000\067\000\022\000\069\000\022\000\
\022\000\022\000\022\000\015\000\015\000\022\000\022\000\022\000\
\022\000\071\000\022\000\022\000\022\000\022\000\022\000\068\000\
\022\000\022\000\022\000\021\000\070\000\021\000\090\000\021\000\
\021\000\021\000\021\000\092\000\112\000\021\000\021\000\021\000\
\114\000\129\000\021\000\021\000\021\000\021\000\021\000\115\000\
\021\000\021\000\021\000\004\000\077\000\004\000\078\000\079\000\
\004\000\004\000\080\000\081\000\017\000\004\000\017\000\004\000\
\130\000\017\000\004\000\004\000\004\000\004\000\004\000\131\000\
\004\000\004\000\004\000\005\000\142\000\005\000\146\000\147\000\
\005\000\005\000\150\000\017\000\071\000\005\000\033\000\005\000\
\034\000\025\000\005\000\005\000\005\000\005\000\005\000\026\000\
\005\000\005\000\005\000\011\000\118\000\011\000\029\000\029\000\
\011\000\128\000\000\000\000\000\000\000\000\000\000\000\011\000\
\000\000\000\000\011\000\011\000\011\000\011\000\011\000\000\000\
\011\000\011\000\011\000\012\000\000\000\012\000\000\000\000\000\
\012\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000\
\000\000\000\000\012\000\012\000\012\000\012\000\012\000\000\000\
\012\000\012\000\012\000\009\000\000\000\009\000\000\000\000\000\
\009\000\000\000\000\000\000\000\000\000\000\000\000\000\009\000\
\000\000\000\000\009\000\009\000\009\000\009\000\009\000\000\000\
\009\000\009\000\009\000\010\000\000\000\010\000\000\000\000\000\
\010\000\000\000\000\000\000\000\000\000\000\000\000\000\010\000\
\000\000\000\000\010\000\010\000\010\000\010\000\010\000\000\000\
\010\000\010\000\010\000\013\000\000\000\013\000\000\000\014\000\
\013\000\014\000\000\000\000\000\014\000\000\000\000\000\013\000\
\000\000\000\000\013\000\014\000\000\000\000\000\014\000\000\000\
\013\000\013\000\013\000\076\000\014\000\014\000\014\000\077\000\
\000\000\078\000\079\000\000\000\000\000\080\000\081\000\082\000\
\000\000\000\000\083\000\084\000\085\000\086\000\087\000\133\000\
\088\000\089\000\000\000\077\000\000\000\078\000\079\000\000\000\
\000\000\080\000\081\000\082\000\000\000\000\000\083\000\084\000\
\085\000\086\000\087\000\139\000\088\000\089\000\000\000\077\000\
\000\000\078\000\079\000\000\000\000\000\080\000\081\000\082\000\
\000\000\000\000\083\000\084\000\085\000\086\000\087\000\000\000\
\088\000\089\000\004\000\005\000\006\000\007\000\008\000\009\000\
\000\000\000\000\000\000\010\000\000\000\000\000\011\000\012\000\
\000\000\000\000\000\000\013\000\000\000\000\000\000\000\000\000\
\014\000\032\000\005\000\006\000\033\000\034\000\035\000\000\000\
\000\000\000\000\036\000\000\000\093\000\037\000\077\000\000\000\
\078\000\079\000\038\000\000\000\080\000\081\000\082\000\039\000\
\000\000\083\000\084\000\085\000\086\000\087\000\000\000\088\000\
\089\000\140\000\000\000\077\000\000\000\078\000\079\000\000\000\
\000\000\080\000\081\000\082\000\000\000\000\000\083\000\084\000\
\085\000\086\000\087\000\000\000\088\000\089\000\141\000\000\000\
\077\000\000\000\078\000\079\000\000\000\000\000\080\000\081\000\
\082\000\000\000\000\000\083\000\084\000\085\000\086\000\087\000\
\000\000\088\000\089\000\077\000\000\000\078\000\079\000\000\000\
\000\000\080\000\081\000\082\000\000\000\000\000\083\000\084\000\
\085\000\086\000\087\000\000\000\088\000\089\000\116\000\077\000\
\000\000\078\000\079\000\000\000\000\000\080\000\081\000\082\000\
\000\000\000\000\083\000\084\000\085\000\086\000\087\000\000\000\
\088\000\089\000\077\000\000\000\078\000\079\000\000\000\000\000\
\080\000\081\000\082\000\000\000\000\000\083\000\084\000\085\000\
\086\000\087\000\077\000\088\000\078\000\079\000\000\000\000\000\
\080\000\081\000\082\000\000\000\000\000\083\000\084\000\085\000\
\086\000\087\000\077\000\000\000\078\000\079\000\000\000\000\000\
\080\000\081\000\000\000\000\000\000\000\000\000\084\000\085\000\
\086\000\087\000"

let yycheck = "\031\000\
\001\001\003\001\001\000\005\001\003\001\003\001\008\001\001\001\
\002\001\054\001\054\001\142\000\006\001\018\000\003\001\003\001\
\147\000\016\001\016\001\003\001\052\000\053\000\024\001\025\001\
\026\001\055\001\027\001\016\001\016\001\023\001\005\001\055\001\
\016\001\008\001\028\001\054\001\030\001\031\001\032\001\001\001\
\002\001\054\001\074\000\075\000\006\001\077\000\078\000\079\000\
\080\000\081\000\082\000\083\000\084\000\085\000\086\000\087\000\
\088\000\089\000\090\000\053\001\054\001\023\001\056\001\057\001\
\069\000\003\001\028\001\001\001\030\001\031\001\032\001\003\001\
\003\001\003\001\001\001\002\001\054\001\005\001\016\001\006\001\
\008\001\054\001\054\001\115\000\016\001\016\001\016\001\001\001\
\002\001\023\001\054\001\053\001\054\001\125\000\056\001\057\001\
\023\001\129\000\130\000\131\000\003\001\028\001\005\001\030\001\
\031\001\032\001\001\001\002\001\054\001\023\001\142\000\006\001\
\001\001\001\001\028\001\147\000\030\001\031\001\032\001\053\001\
\054\001\001\001\056\001\057\001\001\001\054\001\053\001\054\001\
\023\001\056\001\057\001\007\001\001\001\028\001\010\001\030\001\
\031\001\032\001\014\001\053\001\054\001\001\001\056\001\057\001\
\001\001\001\001\140\000\141\000\001\001\003\001\054\001\005\001\
\146\000\054\001\008\001\054\001\150\000\054\001\053\001\054\001\
\054\001\056\001\057\001\003\001\054\001\005\001\008\001\007\001\
\008\001\009\001\010\001\025\001\026\001\013\001\014\001\015\001\
\016\001\005\001\018\001\019\001\020\001\021\001\022\001\054\001\
\024\001\025\001\026\001\003\001\054\001\005\001\016\001\007\001\
\008\001\009\001\010\001\002\001\054\001\013\001\014\001\015\001\
\005\001\001\001\018\001\019\001\020\001\021\001\022\001\008\001\
\024\001\025\001\026\001\003\001\007\001\005\001\009\001\010\001\
\008\001\009\001\013\001\014\001\003\001\013\001\005\001\015\001\
\001\001\008\001\018\001\019\001\020\001\021\001\022\001\001\001\
\024\001\025\001\026\001\003\001\003\001\005\001\029\001\003\001\
\008\001\009\001\005\001\026\001\000\000\013\001\005\001\015\001\
\005\001\005\001\018\001\019\001\020\001\021\001\022\001\005\001\
\024\001\025\001\026\001\003\001\113\000\005\001\003\001\005\001\
\008\001\120\000\255\255\255\255\255\255\255\255\255\255\015\001\
\255\255\255\255\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\025\001\026\001\003\001\255\255\005\001\255\255\255\255\
\008\001\255\255\255\255\255\255\255\255\255\255\255\255\015\001\
\255\255\255\255\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\025\001\026\001\003\001\255\255\005\001\255\255\255\255\
\008\001\255\255\255\255\255\255\255\255\255\255\255\255\015\001\
\255\255\255\255\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\025\001\026\001\003\001\255\255\005\001\255\255\255\255\
\008\001\255\255\255\255\255\255\255\255\255\255\255\255\015\001\
\255\255\255\255\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\025\001\026\001\003\001\255\255\005\001\255\255\003\001\
\008\001\005\001\255\255\255\255\008\001\255\255\255\255\015\001\
\255\255\255\255\018\001\015\001\255\255\255\255\018\001\255\255\
\024\001\025\001\026\001\003\001\024\001\025\001\026\001\007\001\
\255\255\009\001\010\001\255\255\255\255\013\001\014\001\015\001\
\255\255\255\255\018\001\019\001\020\001\021\001\022\001\003\001\
\024\001\025\001\255\255\007\001\255\255\009\001\010\001\255\255\
\255\255\013\001\014\001\015\001\255\255\255\255\018\001\019\001\
\020\001\021\001\022\001\003\001\024\001\025\001\255\255\007\001\
\255\255\009\001\010\001\255\255\255\255\013\001\014\001\015\001\
\255\255\255\255\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\025\001\033\001\034\001\035\001\036\001\037\001\038\001\
\255\255\255\255\255\255\042\001\255\255\255\255\045\001\046\001\
\255\255\255\255\255\255\050\001\255\255\255\255\255\255\255\255\
\055\001\033\001\034\001\035\001\036\001\037\001\038\001\255\255\
\255\255\255\255\042\001\255\255\005\001\045\001\007\001\255\255\
\009\001\010\001\050\001\255\255\013\001\014\001\015\001\055\001\
\255\255\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\025\001\005\001\255\255\007\001\255\255\009\001\010\001\255\255\
\255\255\013\001\014\001\015\001\255\255\255\255\018\001\019\001\
\020\001\021\001\022\001\255\255\024\001\025\001\005\001\255\255\
\007\001\255\255\009\001\010\001\255\255\255\255\013\001\014\001\
\015\001\255\255\255\255\018\001\019\001\020\001\021\001\022\001\
\255\255\024\001\025\001\007\001\255\255\009\001\010\001\255\255\
\255\255\013\001\014\001\015\001\255\255\255\255\018\001\019\001\
\020\001\021\001\022\001\255\255\024\001\025\001\026\001\007\001\
\255\255\009\001\010\001\255\255\255\255\013\001\014\001\015\001\
\255\255\255\255\018\001\019\001\020\001\021\001\022\001\255\255\
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
  TYPEID\000\
  CHARLIT\000\
  STRINGLIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 36 "parser.mly"
                  ( Literal(_1) )
# 458 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 37 "parser.mly"
                  ( Char(_1) )
# 465 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 38 "parser.mly"
                  ( String(_1) )
# 472 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 39 "parser.mly"
                 ( Binop (_1, Add, _3) )
# 480 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 40 "parser.mly"
                  ( Binop (_1, Sub, _3) )
# 488 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 41 "parser.mly"
                  ( Binop (_1, Mult, _3) )
# 496 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 42 "parser.mly"
                  ( Binop (_1, Div, _3) )
# 504 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 43 "parser.mly"
                 ( Binop (_1, Mod, _1) )
# 512 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 44 "parser.mly"
                ( Binop (_1, Less, _3) )
# 520 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 45 "parser.mly"
                ( Binop (_1, Greater, _3) )
# 528 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 46 "parser.mly"
                 ( Binop (_1, Leq, _3) )
# 536 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 47 "parser.mly"
                 ( Binop (_1, Geq, _3) )
# 544 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 48 "parser.mly"
                ( Binop (_1, Equal, _3) )
# 552 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 49 "parser.mly"
                 ( Binop (_1, Neq, _3) )
# 560 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 50 "parser.mly"
                ( Binop (_1, Or, _3) )
# 568 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 51 "parser.mly"
                 ( Binop (_1, And, _3) )
# 576 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 52 "parser.mly"
                  ( Assign(_1, _3) )
# 584 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 53 "parser.mly"
                 ( Not(_2) )
# 591 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 54 "parser.mly"
                              ( Call(_1, _3) )
# 599 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 55 "parser.mly"
                    ( _2 )
# 606 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 56 "parser.mly"
      ( _1 )
# 613 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 60 "parser.mly"
                ( Id(_1) )
# 620 "parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arr) in
    Obj.repr(
# 61 "parser.mly"
                ( Array( fst _1, snd _1) )
# 627 "parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                              ( Id(_1),_3 )
# 635 "parser.ml"
               : 'arr))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
                ( [] )
# 641 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 68 "parser.mly"
                 ( List.rev _1 )
# 648 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
       ( [_1] )
# 655 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                            ( _3 :: _1 )
# 663 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
                (Noexpr )
# 669 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
         ( _1 )
# 676 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 79 "parser.mly"
               ( [_2] )
# 684 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_decl) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "parser.mly"
                                    ( _4 :: _1 )
# 693 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
                ( [] )
# 699 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 84 "parser.mly"
                 ( List.rev _1 )
# 706 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
                ( [] )
# 712 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 88 "parser.mly"
                   ( _2 :: _1 )
# 720 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
            ( Expr(_1) )
# 727 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
                     ( Return(_2) )
# 734 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 93 "parser.mly"
                            ( Block(List.rev _2) )
# 741 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 94 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 749 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 95 "parser.mly"
                                         ( If (_3, _5, _7) )
# 758 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 97 "parser.mly"
   ( For(_3, _5, _7, _9) )
# 768 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 98 "parser.mly"
                                  ( While(_3, _5) )
# 776 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_decl) in
    Obj.repr(
# 101 "parser.mly"
                 ( _1 )
# 783 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'type_decl) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                               ( _1 )
# 791 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
                ( [] )
# 797 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 106 "parser.mly"
                     ( _2 :: _1 )
# 805 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'retval) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 110 "parser.mly"
     ( { fname = snd _1;
         formals = _2; 
         locals = List.rev _5;
         body = List.rev _6;
         ret = fst _1
         } )
# 820 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 118 "parser.mly"
                      ( [Int], _2  )
# 827 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 119 "parser.mly"
                         ( [Char], _2  )
# 834 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 120 "parser.mly"
                         ( [Void], _2  )
# 841 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 121 "parser.mly"
                           ( [String], _2  )
# 848 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 122 "parser.mly"
                           ( [Double], _2  )
# 855 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 123 "parser.mly"
                         ( [Node], _2  )
# 862 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 124 "parser.mly"
                         ( [Bool], _2  )
# 869 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 125 "parser.mly"
                          ( [Graph], _2  )
# 876 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 126 "parser.mly"
                           ( [Userdef], _2  )
# 884 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 129 "parser.mly"
        ( _2 )
# 891 "parser.ml"
               : 'type_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 130 "parser.mly"
             ( _2 )
# 898 "parser.ml"
               : 'type_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 131 "parser.mly"
           ( _2 )
# 905 "parser.ml"
               : 'type_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 132 "parser.mly"
             ( _2 )
# 912 "parser.ml"
               : 'type_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 133 "parser.mly"
           ( _2 )
# 919 "parser.ml"
               : 'type_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 134 "parser.mly"
           ( _2 )
# 926 "parser.ml"
               : 'type_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 135 "parser.mly"
                   ( _2 )
# 933 "parser.ml"
               : 'type_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 136 "parser.mly"
                   ( _2 )
# 940 "parser.ml"
               : 'type_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 137 "parser.mly"
            ( _2 )
# 947 "parser.ml"
               : 'type_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 138 "parser.mly"
             ( _2 )
# 955 "parser.ml"
               : 'type_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 141 "parser.mly"
               ( [], [] )
# 961 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 142 "parser.mly"
                 ( (_2 :: fst _1), snd _1 )
# 969 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 143 "parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 977 "parser.ml"
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
