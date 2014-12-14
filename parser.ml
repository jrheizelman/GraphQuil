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
\002\000\002\000\002\000\002\000\004\000\004\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\003\000\003\000\006\000\006\000\007\000\007\000\009\000\
\009\000\010\000\010\000\012\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\008\000\013\000\014\000\014\000\015\000\
\015\000\001\000\001\000\001\000\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000\002\000\003\000\004\000\003\000\
\007\000\007\000\007\000\007\000\000\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\000\000\001\000\001\000\003\000\001\000\003\000\000\000\
\001\000\000\000\002\000\003\000\001\000\002\000\003\000\005\000\
\007\000\009\000\005\000\002\000\002\000\000\000\003\000\009\000\
\009\000\000\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\066\000\000\000\000\000\038\000\040\000\041\000\039\000\035\000\
\037\000\034\000\033\000\036\000\031\000\000\000\032\000\000\000\
\000\000\067\000\068\000\000\000\000\000\061\000\000\000\000\000\
\000\000\000\000\046\000\000\000\000\000\060\000\000\000\000\000\
\000\000\047\000\062\000\062\000\000\000\000\000\000\000\000\000\
\000\000\063\000\000\000\050\000\065\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\005\000\000\000\004\000\002\000\
\000\000\051\000\053\000\064\000\000\000\000\000\021\000\020\000\
\000\000\000\000\000\000\000\000\000\000\000\000\054\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\024\000\052\000\000\000\
\000\000\000\000\000\000\055\000\000\000\000\000\000\000\019\000\
\010\000\000\000\008\000\000\000\009\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\023\000\000\000\000\000\000\000\059\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\057\000\000\000\028\000\025\000\026\000\027\000\000\000\058\000"

let yydgoto = "\002\000\
\003\000\057\000\094\000\091\000\025\000\095\000\026\000\027\000\
\028\000\040\000\058\000\059\000\018\000\037\000\019\000"

let yysindex = "\026\000\
\000\000\000\000\234\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\198\254\000\000\241\254\
\046\255\000\000\000\000\051\255\064\255\000\000\255\000\255\000\
\019\255\075\255\000\000\079\255\086\255\000\000\255\000\091\255\
\092\255\000\000\000\000\000\000\255\000\255\000\093\255\044\255\
\057\255\000\000\163\255\000\000\000\000\163\255\163\255\094\255\
\096\255\097\255\163\255\000\000\000\000\010\255\000\000\000\000\
\244\255\000\000\000\000\000\000\200\000\098\255\000\000\000\000\
\163\255\163\255\163\255\008\000\163\255\049\255\000\000\163\255\
\163\255\163\255\163\255\163\255\163\255\136\255\163\255\163\255\
\163\255\163\255\163\255\163\255\163\255\000\000\000\000\221\000\
\242\000\042\001\102\255\000\000\042\001\104\255\110\255\000\000\
\000\000\011\255\000\000\011\255\000\000\093\001\048\255\042\001\
\093\001\000\255\000\255\000\255\000\255\077\001\061\001\120\255\
\120\255\163\255\000\000\163\255\119\255\103\255\000\000\122\255\
\042\001\254\254\120\255\163\255\101\255\105\255\107\255\109\255\
\000\000\131\255\000\000\000\000\000\000\000\000\120\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\138\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\137\255\000\000\139\255\139\255\
\000\000\140\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\111\255\111\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\224\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\143\255\000\000\142\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\087\255\000\000\000\000\043\255\000\000\145\255\000\000\
\000\000\031\000\000\000\054\000\000\000\169\000\000\000\012\255\
\173\000\077\000\100\000\123\000\146\000\187\000\061\255\000\000\
\000\000\143\255\000\000\000\000\000\000\152\255\000\000\000\000\
\074\255\000\000\000\000\146\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\213\255\000\000\144\255\145\000\000\000\000\000\016\000\
\128\000\024\000\000\000\149\255\000\000\126\000\000\000"

let yytablesize = 627
let yytable = "\061\000\
\020\000\120\000\063\000\064\000\118\000\119\000\072\000\068\000\
\073\000\074\000\069\000\130\000\075\000\076\000\022\000\129\000\
\022\000\072\000\017\000\022\000\074\000\088\000\089\000\090\000\
\076\000\093\000\001\000\136\000\097\000\098\000\099\000\100\000\
\101\000\102\000\104\000\105\000\106\000\107\000\108\000\109\000\
\110\000\111\000\070\000\021\000\043\000\044\000\034\000\044\000\
\022\000\045\000\044\000\023\000\039\000\039\000\125\000\126\000\
\046\000\043\000\044\000\127\000\128\000\041\000\060\000\017\000\
\024\000\017\000\047\000\062\000\017\000\046\000\090\000\048\000\
\121\000\049\000\050\000\051\000\017\000\030\000\045\000\047\000\
\090\000\045\000\031\000\032\000\048\000\017\000\049\000\050\000\
\051\000\030\000\033\000\030\000\035\000\036\000\065\000\042\000\
\066\000\067\000\043\000\044\000\052\000\053\000\054\000\087\000\
\114\000\055\000\056\000\096\000\115\000\117\000\046\000\050\000\
\050\000\052\000\053\000\054\000\050\000\116\000\055\000\056\000\
\047\000\044\000\122\000\050\000\124\000\048\000\131\000\049\000\
\050\000\051\000\132\000\123\000\133\000\050\000\134\000\135\000\
\043\000\069\000\050\000\060\000\050\000\050\000\050\000\048\000\
\049\000\029\000\042\000\016\000\046\000\043\000\029\000\029\000\
\056\000\056\000\052\000\053\000\054\000\056\000\047\000\055\000\
\056\000\038\000\103\000\043\000\056\000\000\000\000\000\050\000\
\050\000\050\000\000\000\000\000\050\000\050\000\056\000\046\000\
\000\000\000\000\000\000\056\000\000\000\056\000\056\000\056\000\
\000\000\047\000\000\000\000\000\000\000\000\000\000\000\000\000\
\052\000\053\000\054\000\000\000\000\000\055\000\056\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\056\000\056\000\056\000\000\000\000\000\056\000\056\000\000\000\
\000\000\000\000\000\000\052\000\053\000\054\000\000\000\000\000\
\055\000\056\000\003\000\000\000\003\000\000\000\003\000\003\000\
\003\000\003\000\000\000\000\000\003\000\003\000\003\000\003\000\
\000\000\003\000\003\000\003\000\003\000\003\000\071\000\003\000\
\003\000\000\000\072\000\000\000\073\000\074\000\000\000\000\000\
\075\000\076\000\077\000\078\000\000\000\079\000\080\000\081\000\
\082\000\083\000\092\000\084\000\085\000\000\000\072\000\000\000\
\073\000\074\000\000\000\000\000\075\000\076\000\077\000\078\000\
\000\000\079\000\080\000\081\000\082\000\083\000\000\000\084\000\
\085\000\006\000\000\000\006\000\000\000\000\000\006\000\006\000\
\000\000\000\000\000\000\006\000\000\000\006\000\006\000\000\000\
\006\000\006\000\006\000\006\000\006\000\000\000\006\000\006\000\
\007\000\000\000\007\000\000\000\000\000\007\000\007\000\000\000\
\000\000\000\000\007\000\000\000\007\000\007\000\000\000\007\000\
\007\000\007\000\007\000\007\000\000\000\007\000\007\000\013\000\
\000\000\013\000\000\000\000\000\013\000\000\000\000\000\000\000\
\000\000\000\000\000\000\013\000\013\000\000\000\013\000\013\000\
\013\000\013\000\013\000\000\000\013\000\013\000\014\000\000\000\
\014\000\000\000\000\000\014\000\000\000\000\000\000\000\000\000\
\000\000\000\000\014\000\014\000\000\000\014\000\014\000\014\000\
\014\000\014\000\000\000\014\000\014\000\011\000\000\000\011\000\
\000\000\000\000\011\000\000\000\000\000\000\000\000\000\000\000\
\000\000\011\000\011\000\000\000\011\000\011\000\011\000\011\000\
\011\000\000\000\011\000\011\000\012\000\000\000\012\000\000\000\
\000\000\012\000\000\000\000\000\000\000\000\000\000\000\000\000\
\012\000\012\000\000\000\012\000\012\000\012\000\012\000\012\000\
\000\000\012\000\012\000\015\000\000\000\015\000\000\000\016\000\
\015\000\016\000\000\000\000\000\016\000\000\000\000\000\015\000\
\015\000\000\000\015\000\016\000\016\000\018\000\016\000\018\000\
\015\000\015\000\018\000\000\000\016\000\016\000\000\000\000\000\
\000\000\000\000\018\000\000\000\086\000\000\000\072\000\000\000\
\073\000\074\000\018\000\018\000\075\000\076\000\077\000\078\000\
\000\000\079\000\080\000\081\000\082\000\083\000\000\000\084\000\
\085\000\112\000\000\000\072\000\000\000\073\000\074\000\000\000\
\000\000\075\000\076\000\077\000\078\000\000\000\079\000\080\000\
\081\000\082\000\083\000\000\000\084\000\085\000\113\000\000\000\
\072\000\000\000\073\000\074\000\000\000\000\000\075\000\076\000\
\077\000\078\000\000\000\079\000\080\000\081\000\082\000\083\000\
\000\000\084\000\085\000\004\000\005\000\006\000\007\000\008\000\
\009\000\010\000\011\000\000\000\000\000\000\000\000\000\012\000\
\000\000\000\000\013\000\014\000\000\000\000\000\000\000\015\000\
\004\000\005\000\006\000\007\000\008\000\009\000\010\000\011\000\
\000\000\000\000\000\000\000\000\012\000\000\000\000\000\013\000\
\072\000\000\000\073\000\074\000\015\000\000\000\075\000\076\000\
\077\000\078\000\000\000\079\000\080\000\081\000\082\000\083\000\
\000\000\084\000\085\000\072\000\000\000\073\000\074\000\000\000\
\000\000\075\000\076\000\077\000\000\000\000\000\079\000\080\000\
\081\000\082\000\083\000\072\000\084\000\073\000\074\000\000\000\
\000\000\075\000\076\000\077\000\000\000\000\000\079\000\080\000\
\081\000\082\000\083\000\072\000\000\000\073\000\074\000\000\000\
\000\000\075\000\076\000\000\000\000\000\000\000\000\000\080\000\
\081\000\082\000\083\000"

let yycheck = "\043\000\
\059\001\114\000\046\000\047\000\112\000\113\000\007\001\051\000\
\009\001\010\001\001\001\124\000\013\001\014\001\003\001\123\000\
\005\001\007\001\003\000\008\001\010\001\065\000\066\000\067\000\
\014\001\069\000\001\000\135\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\079\000\080\000\081\000\082\000\083\000\
\084\000\085\000\033\001\059\001\001\001\002\001\031\000\005\001\
\003\001\006\001\008\001\001\001\037\000\038\000\057\001\058\001\
\013\001\001\001\002\001\062\001\063\001\038\000\006\001\003\001\
\001\001\005\001\023\001\044\000\008\001\013\001\114\000\028\001\
\116\000\030\001\031\001\032\001\016\001\059\001\005\001\023\001\
\124\000\008\001\008\001\005\001\028\001\025\001\030\001\031\001\
\032\001\003\001\005\001\005\001\002\001\002\001\001\001\003\001\
\001\001\001\001\001\001\002\001\057\001\058\001\059\001\006\001\
\003\001\062\001\063\001\059\001\005\001\062\001\013\001\001\001\
\002\001\057\001\058\001\059\001\006\001\008\001\062\001\063\001\
\023\001\002\001\004\001\013\001\003\001\028\001\026\001\030\001\
\031\001\032\001\026\001\029\001\026\001\023\001\026\001\005\001\
\001\001\000\000\028\001\003\001\030\001\031\001\032\001\005\001\
\005\001\003\001\005\001\003\000\013\001\005\001\005\001\024\000\
\001\001\002\001\057\001\058\001\059\001\006\001\023\001\062\001\
\063\001\036\000\027\001\001\001\013\001\255\255\255\255\057\001\
\058\001\059\001\255\255\255\255\062\001\063\001\023\001\013\001\
\255\255\255\255\255\255\028\001\255\255\030\001\031\001\032\001\
\255\255\023\001\255\255\255\255\255\255\255\255\255\255\255\255\
\057\001\058\001\059\001\255\255\255\255\062\001\063\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\057\001\058\001\059\001\255\255\255\255\062\001\063\001\255\255\
\255\255\255\255\255\255\057\001\058\001\059\001\255\255\255\255\
\062\001\063\001\003\001\255\255\005\001\255\255\007\001\008\001\
\009\001\010\001\255\255\255\255\013\001\014\001\015\001\016\001\
\255\255\018\001\019\001\020\001\021\001\022\001\003\001\024\001\
\025\001\255\255\007\001\255\255\009\001\010\001\255\255\255\255\
\013\001\014\001\015\001\016\001\255\255\018\001\019\001\020\001\
\021\001\022\001\003\001\024\001\025\001\255\255\007\001\255\255\
\009\001\010\001\255\255\255\255\013\001\014\001\015\001\016\001\
\255\255\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\025\001\003\001\255\255\005\001\255\255\255\255\008\001\009\001\
\255\255\255\255\255\255\013\001\255\255\015\001\016\001\255\255\
\018\001\019\001\020\001\021\001\022\001\255\255\024\001\025\001\
\003\001\255\255\005\001\255\255\255\255\008\001\009\001\255\255\
\255\255\255\255\013\001\255\255\015\001\016\001\255\255\018\001\
\019\001\020\001\021\001\022\001\255\255\024\001\025\001\003\001\
\255\255\005\001\255\255\255\255\008\001\255\255\255\255\255\255\
\255\255\255\255\255\255\015\001\016\001\255\255\018\001\019\001\
\020\001\021\001\022\001\255\255\024\001\025\001\003\001\255\255\
\005\001\255\255\255\255\008\001\255\255\255\255\255\255\255\255\
\255\255\255\255\015\001\016\001\255\255\018\001\019\001\020\001\
\021\001\022\001\255\255\024\001\025\001\003\001\255\255\005\001\
\255\255\255\255\008\001\255\255\255\255\255\255\255\255\255\255\
\255\255\015\001\016\001\255\255\018\001\019\001\020\001\021\001\
\022\001\255\255\024\001\025\001\003\001\255\255\005\001\255\255\
\255\255\008\001\255\255\255\255\255\255\255\255\255\255\255\255\
\015\001\016\001\255\255\018\001\019\001\020\001\021\001\022\001\
\255\255\024\001\025\001\003\001\255\255\005\001\255\255\003\001\
\008\001\005\001\255\255\255\255\008\001\255\255\255\255\015\001\
\016\001\255\255\018\001\015\001\016\001\003\001\018\001\005\001\
\024\001\025\001\008\001\255\255\024\001\025\001\255\255\255\255\
\255\255\255\255\016\001\255\255\005\001\255\255\007\001\255\255\
\009\001\010\001\024\001\025\001\013\001\014\001\015\001\016\001\
\255\255\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\025\001\005\001\255\255\007\001\255\255\009\001\010\001\255\255\
\255\255\013\001\014\001\015\001\016\001\255\255\018\001\019\001\
\020\001\021\001\022\001\255\255\024\001\025\001\005\001\255\255\
\007\001\255\255\009\001\010\001\255\255\255\255\013\001\014\001\
\015\001\016\001\255\255\018\001\019\001\020\001\021\001\022\001\
\255\255\024\001\025\001\034\001\035\001\036\001\037\001\038\001\
\039\001\040\001\041\001\255\255\255\255\255\255\255\255\046\001\
\255\255\255\255\049\001\050\001\255\255\255\255\255\255\054\001\
\034\001\035\001\036\001\037\001\038\001\039\001\040\001\041\001\
\255\255\255\255\255\255\255\255\046\001\255\255\255\255\049\001\
\007\001\255\255\009\001\010\001\054\001\255\255\013\001\014\001\
\015\001\016\001\255\255\018\001\019\001\020\001\021\001\022\001\
\255\255\024\001\025\001\007\001\255\255\009\001\010\001\255\255\
\255\255\013\001\014\001\015\001\255\255\255\255\018\001\019\001\
\020\001\021\001\022\001\007\001\024\001\009\001\010\001\255\255\
\255\255\013\001\014\001\015\001\255\255\255\255\018\001\019\001\
\020\001\021\001\022\001\007\001\255\255\009\001\010\001\255\255\
\255\255\013\001\014\001\255\255\255\255\255\255\255\255\019\001\
\020\001\021\001\022\001"

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
# 483 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
                                         ( Char(_1) )
# 490 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parser.mly"
                                         ( Id(_1))
# 497 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "parser.mly"
                                         ( String_Lit(_1) )
# 504 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 60 "parser.mly"
                                         ( Bool_Lit(_1))
# 511 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 61 "parser.mly"
                                         ( Binop (_1, Add, _3) )
# 519 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 62 "parser.mly"
                                        ( Binop (_1, Sub, _3) )
# 527 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
                                        ( Binop (_1, Mult, _3) )
# 535 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                                         ( Binop (_1, Div, _3) )
# 543 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                                       ( Binop (_1, Mod, _1) )
# 551 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                                        ( Binop (_1, Less, _3) )
# 559 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                                        ( Binop (_1, Greater, _3) )
# 567 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                                       ( Binop (_1, Leq, _3) )
# 575 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                                       ( Binop (_1, Geq, _3) )
# 583 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                                         ( Binop (_1, Equal, _3) )
# 591 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                                       ( Binop (_1, Neq, _3) )
# 599 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                                        ( Binop (_1, Or, _3) )
# 607 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                                       ( Binop (_1, And, _3) )
# 615 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 74 "parser.mly"
                                         ( Add_at(_1, _3) )
# 623 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                                       ( Unop(Not, _2) )
# 630 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                                         ( Unop(Neg, _2) )
# 637 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                                         ( Assign(_1, _3) )
# 645 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 78 "parser.mly"
                                         ( Call(_1, _3) )
# 653 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
                                         ( _2 )
# 660 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : bool) in
    Obj.repr(
# 80 "parser.mly"
                                                    ( Assign_Bool_at(_1, _4, _6) )
# 669 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 81 "parser.mly"
                                                      ( Assign_String_at(_1, _4, _6) )
# 678 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 82 "parser.mly"
                                                    ( Assign_Char_at(_1, _4, _6) )
# 687 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 83 "parser.mly"
                                                    ( Assign_Int_at(_1, _4, _6) )
# 696 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                  ( Noexpr )
# 702 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 87 "parser.mly"
                  ( _1 )
# 709 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
           ( Int )
# 715 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
           ( Char )
# 721 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
           ( String )
# 727 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
           ( Bool )
# 733 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
           ( Node )
# 739 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
           ( Edge )
# 745 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
           ( Graph )
# 751 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
            ( Int_at )
# 757 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
             ( Bool_at )
# 763 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "parser.mly"
             ( String_at )
# 769 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser.mly"
             ( Char_at )
# 775 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
                 ( [] )
# 781 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 104 "parser.mly"
                 ( List.rev _1 )
# 788 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "parser.mly"
                            ( [_1] )
# 795 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                            ( _3 :: _1 )
# 803 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 111 "parser.mly"
                             ( [_1])
# 810 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 112 "parser.mly"
                             ( _3 :: _1 )
# 818 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "parser.mly"
                 ( [] )
# 824 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 116 "parser.mly"
                 ( List.rev _1 )
# 831 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parser.mly"
                   ( [] )
# 837 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 120 "parser.mly"
                   ( _2 :: _1 )
# 845 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 123 "parser.mly"
                          ( {locals = []; statements = List.rev _2; block_num = inc_block_num ()} )
# 852 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 126 "parser.mly"
                                                                  ( Block(_1))
# 859 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                                                                  ( Expr(_1) )
# 866 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                                                                  ( Return(_2) )
# 873 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 130 "parser.mly"
                                                                  ( If(_3, _5, {locals = []; statements = []; block_num = inc_block_num ()}) )
# 881 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'block) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 131 "parser.mly"
                                                                  ( If (_3, _5, _7) )
# 890 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 132 "parser.mly"
                                                                  ( For(_3, _5, _7, _9) )
# 900 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 133 "parser.mly"
                                                                  ( While(_3, _5) )
# 908 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'any_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 136 "parser.mly"
              (_2, _1)
# 916 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl) in
    Obj.repr(
# 139 "parser.mly"
             ( _1 )
# 923 "parser.ml"
               : 'glb_vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "parser.mly"
                          ( [] )
# 929 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl) in
    Obj.repr(
# 144 "parser.mly"
                          ( _2 :: _1 )
# 937 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'any_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 148 "parser.mly"
     ( { fname = _2;
         formals = _4; 
         body_block = {locals = List.rev _7; statements = List.rev _8; block_num = inc_block_num()} ;
         ret = _1 } )
# 951 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 153 "parser.mly"
     ( { fname = _2;
         formals = _4; 
         body_block = {locals = List.rev _7; statements = List.rev _8; block_num = inc_block_num()} ;
         ret = Void } )
# 964 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 159 "parser.mly"
                 ( [], [] )
# 970 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'glb_vdecl) in
    Obj.repr(
# 160 "parser.mly"
                     ( (_2 :: fst _1), snd _1 )
# 978 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 161 "parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 986 "parser.ml"
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
