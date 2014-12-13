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
  | NODETYPE
  | EDGETYPE
  | GRAPH
  | NODE
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

# 82 "parser.ml"
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
  289 (* NODETYPE *);
  290 (* EDGETYPE *);
  291 (* GRAPH *);
  292 (* NODE *);
  293 (* BOOL *);
  294 (* STRING *);
  295 (* PRINT *);
  296 (* NEW *);
  297 (* CONTINUE *);
  298 (* DOUBLE *);
  299 (* EDGE *);
  300 (* FALSE *);
  301 (* TRUE *);
  302 (* INT *);
  303 (* VOID *);
  304 (* DEST *);
  305 (* EDGES *);
  306 (* STATIC *);
  307 (* CHAR *);
  308 (* DO *);
  309 (* IN *);
    0|]

let yytransl_block = [|
  310 (* LITERAL *);
  311 (* BOOLLIT *);
  312 (* ID *);
  313 (* TYPEID *);
  314 (* ARRID *);
  315 (* STRINGLIT *);
  316 (* CHARLIT *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\004\000\
\004\000\005\000\005\000\005\000\005\000\005\000\006\000\006\000\
\006\000\006\000\006\000\006\000\003\000\003\000\007\000\007\000\
\008\000\008\000\010\000\010\000\011\000\011\000\013\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\009\000\014\000\
\015\000\015\000\016\000\016\000\001\000\001\000\001\000\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\002\000\002\000\003\000\004\000\003\000\000\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\000\000\001\000\001\000\003\000\
\001\000\003\000\000\000\001\000\000\000\002\000\003\000\001\000\
\002\000\003\000\005\000\007\000\009\000\005\000\002\000\002\000\
\000\000\003\000\009\000\009\000\000\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\061\000\000\000\000\000\032\000\034\000\035\000\031\000\029\000\
\028\000\033\000\026\000\000\000\027\000\036\000\000\000\030\000\
\000\000\062\000\063\000\000\000\000\000\056\000\000\000\000\000\
\000\000\000\000\041\000\000\000\000\000\055\000\000\000\000\000\
\000\000\042\000\057\000\057\000\000\000\000\000\000\000\000\000\
\000\000\058\000\000\000\045\000\060\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\005\000\000\000\004\000\002\000\
\000\000\046\000\048\000\059\000\000\000\000\000\020\000\019\000\
\000\000\000\000\000\000\000\000\000\000\049\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\023\000\047\000\000\000\000\000\
\000\000\000\000\050\000\000\000\000\000\000\000\010\000\000\000\
\008\000\000\000\009\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\022\000\
\000\000\000\000\054\000\000\000\000\000\000\000\000\000\052\000\
\000\000\000\000\053\000"

let yydgoto = "\002\000\
\003\000\057\000\093\000\090\000\025\000\016\000\094\000\026\000\
\027\000\028\000\040\000\058\000\059\000\018\000\037\000\019\000"

let yysindex = "\001\000\
\000\000\000\000\158\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\206\254\000\000\000\000\214\254\000\000\
\012\255\000\000\000\000\018\255\019\255\000\000\183\255\183\255\
\245\254\049\255\000\000\057\255\069\255\000\000\183\255\073\255\
\086\255\000\000\000\000\000\000\183\255\183\255\056\255\041\255\
\054\255\000\000\130\255\000\000\000\000\130\255\130\255\088\255\
\090\255\091\255\130\255\000\000\000\000\098\255\000\000\000\000\
\248\255\000\000\000\000\000\000\204\000\092\255\000\000\000\000\
\130\255\130\255\130\255\012\000\130\255\000\000\130\255\130\255\
\130\255\130\255\130\255\130\255\130\255\130\255\130\255\130\255\
\130\255\130\255\130\255\130\255\000\000\000\000\225\000\246\000\
\009\001\099\255\000\000\009\001\107\255\095\255\000\000\051\255\
\000\000\051\255\000\000\060\001\009\001\060\001\039\255\039\255\
\039\255\039\255\044\001\028\001\102\255\102\255\130\255\000\000\
\130\255\087\255\000\000\114\255\009\001\102\255\130\255\000\000\
\116\255\102\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\119\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\122\255\000\000\121\255\121\255\
\000\000\124\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\105\255\105\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\228\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\127\255\000\000\129\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\076\255\000\000\000\000\058\255\000\000\133\255\000\000\035\000\
\000\000\058\000\000\000\173\000\075\255\177\000\081\000\104\000\
\127\000\150\000\191\000\002\255\000\000\000\000\127\255\000\000\
\000\000\143\255\000\000\000\000\082\255\000\000\134\255\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\213\255\000\000\146\255\124\000\000\000\000\000\000\000\
\013\000\108\000\229\255\000\000\159\255\000\000\105\000\000\000"

let yytablesize = 594
let yytable = "\061\000\
\116\000\001\000\063\000\064\000\017\000\020\000\017\000\068\000\
\121\000\017\000\041\000\114\000\115\000\021\000\022\000\017\000\
\062\000\017\000\023\000\024\000\120\000\087\000\088\000\089\000\
\123\000\092\000\017\000\095\000\096\000\097\000\098\000\099\000\
\100\000\101\000\102\000\103\000\104\000\105\000\106\000\107\000\
\108\000\043\000\044\000\034\000\030\000\071\000\045\000\072\000\
\073\000\039\000\039\000\074\000\075\000\046\000\043\000\044\000\
\031\000\071\000\042\000\060\000\073\000\032\000\039\000\047\000\
\075\000\039\000\046\000\089\000\048\000\117\000\049\000\050\000\
\051\000\033\000\035\000\089\000\047\000\021\000\025\000\021\000\
\025\000\048\000\021\000\049\000\050\000\051\000\040\000\036\000\
\065\000\040\000\066\000\067\000\043\000\044\000\052\000\053\000\
\054\000\086\000\069\000\055\000\056\000\111\000\113\000\044\000\
\046\000\045\000\045\000\052\000\053\000\054\000\045\000\112\000\
\055\000\056\000\047\000\118\000\119\000\045\000\064\000\048\000\
\122\000\049\000\050\000\051\000\055\000\043\000\015\000\045\000\
\044\000\024\000\043\000\029\000\045\000\037\000\045\000\045\000\
\045\000\038\000\024\000\000\000\038\000\000\000\046\000\051\000\
\051\000\052\000\053\000\054\000\051\000\000\000\055\000\056\000\
\047\000\000\000\000\000\051\000\000\000\000\000\045\000\045\000\
\045\000\000\000\000\000\045\000\045\000\051\000\000\000\000\000\
\000\000\000\000\051\000\000\000\051\000\051\000\051\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\052\000\
\053\000\054\000\000\000\000\000\055\000\056\000\004\000\005\000\
\006\000\007\000\008\000\009\000\051\000\051\000\051\000\000\000\
\010\000\051\000\051\000\011\000\012\000\000\000\000\000\000\000\
\013\000\000\000\000\000\000\000\000\000\000\000\014\000\004\000\
\005\000\006\000\007\000\008\000\009\000\000\000\000\000\000\000\
\000\000\010\000\000\000\000\000\011\000\000\000\003\000\000\000\
\003\000\013\000\003\000\003\000\003\000\003\000\000\000\014\000\
\003\000\003\000\003\000\003\000\000\000\003\000\003\000\003\000\
\003\000\003\000\070\000\003\000\003\000\000\000\071\000\000\000\
\072\000\073\000\000\000\000\000\074\000\075\000\076\000\077\000\
\000\000\078\000\079\000\080\000\081\000\082\000\091\000\083\000\
\084\000\000\000\071\000\000\000\072\000\073\000\000\000\000\000\
\074\000\075\000\076\000\077\000\000\000\078\000\079\000\080\000\
\081\000\082\000\000\000\083\000\084\000\006\000\000\000\006\000\
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
\085\000\000\000\071\000\000\000\072\000\073\000\018\000\018\000\
\074\000\075\000\076\000\077\000\000\000\078\000\079\000\080\000\
\081\000\082\000\000\000\083\000\084\000\109\000\000\000\071\000\
\000\000\072\000\073\000\000\000\000\000\074\000\075\000\076\000\
\077\000\000\000\078\000\079\000\080\000\081\000\082\000\000\000\
\083\000\084\000\110\000\000\000\071\000\000\000\072\000\073\000\
\000\000\000\000\074\000\075\000\076\000\077\000\000\000\078\000\
\079\000\080\000\081\000\082\000\000\000\083\000\084\000\071\000\
\000\000\072\000\073\000\000\000\000\000\074\000\075\000\076\000\
\077\000\000\000\078\000\079\000\080\000\081\000\082\000\000\000\
\083\000\084\000\071\000\000\000\072\000\073\000\000\000\000\000\
\074\000\075\000\076\000\000\000\000\000\078\000\079\000\080\000\
\081\000\082\000\071\000\083\000\072\000\073\000\000\000\000\000\
\074\000\075\000\076\000\000\000\000\000\078\000\079\000\080\000\
\081\000\082\000\071\000\000\000\072\000\073\000\000\000\000\000\
\074\000\075\000\000\000\000\000\000\000\000\000\079\000\080\000\
\081\000\082\000"

let yycheck = "\043\000\
\111\000\001\000\046\000\047\000\003\001\056\001\005\001\051\000\
\119\000\008\001\038\000\109\000\110\000\056\001\003\001\003\000\
\044\000\016\001\001\001\001\001\118\000\065\000\066\000\067\000\
\122\000\069\000\025\001\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\079\000\080\000\081\000\082\000\083\000\
\084\000\001\001\002\001\031\000\056\001\007\001\006\001\009\001\
\010\001\037\000\038\000\013\001\014\001\013\001\001\001\002\001\
\008\001\007\001\003\001\006\001\010\001\005\001\005\001\023\001\
\014\001\008\001\013\001\111\000\028\001\113\000\030\001\031\001\
\032\001\005\001\002\001\119\000\023\001\003\001\003\001\005\001\
\005\001\028\001\008\001\030\001\031\001\032\001\005\001\002\001\
\001\001\008\001\001\001\001\001\001\001\002\001\054\001\055\001\
\056\001\006\001\001\001\059\001\060\001\003\001\008\001\002\001\
\013\001\001\001\002\001\054\001\055\001\056\001\006\001\005\001\
\059\001\060\001\023\001\029\001\003\001\013\001\000\000\028\001\
\005\001\030\001\031\001\032\001\003\001\005\001\003\000\023\001\
\005\001\003\001\001\001\024\000\028\001\005\001\030\001\031\001\
\032\001\005\001\005\001\255\255\036\000\255\255\013\001\001\001\
\002\001\054\001\055\001\056\001\006\001\255\255\059\001\060\001\
\023\001\255\255\255\255\013\001\255\255\255\255\054\001\055\001\
\056\001\255\255\255\255\059\001\060\001\023\001\255\255\255\255\
\255\255\255\255\028\001\255\255\030\001\031\001\032\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\054\001\
\055\001\056\001\255\255\255\255\059\001\060\001\033\001\034\001\
\035\001\036\001\037\001\038\001\054\001\055\001\056\001\255\255\
\043\001\059\001\060\001\046\001\047\001\255\255\255\255\255\255\
\051\001\255\255\255\255\255\255\255\255\255\255\057\001\033\001\
\034\001\035\001\036\001\037\001\038\001\255\255\255\255\255\255\
\255\255\043\001\255\255\255\255\046\001\255\255\003\001\255\255\
\005\001\051\001\007\001\008\001\009\001\010\001\255\255\057\001\
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
\019\001\020\001\021\001\022\001\255\255\024\001\025\001\007\001\
\255\255\009\001\010\001\255\255\255\255\013\001\014\001\015\001\
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
  NODETYPE\000\
  EDGETYPE\000\
  GRAPH\000\
  NODE\000\
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
# 461 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
                                         ( Char(_1) )
# 468 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parser.mly"
                                         ( Id(_1))
# 475 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "parser.mly"
                                         ( String_Lit(_1) )
# 482 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 60 "parser.mly"
                                         ( Bool_Lit(_1))
# 489 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 61 "parser.mly"
                                         ( Binop (_1, Add, _3) )
# 497 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 62 "parser.mly"
                                        ( Binop (_1, Sub, _3) )
# 505 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
                                        ( Binop (_1, Mult, _3) )
# 513 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                                         ( Binop (_1, Div, _3) )
# 521 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                                       ( Binop (_1, Mod, _1) )
# 529 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                                        ( Binop (_1, Less, _3) )
# 537 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                                        ( Binop (_1, Greater, _3) )
# 545 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                                       ( Binop (_1, Leq, _3) )
# 553 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                                       ( Binop (_1, Geq, _3) )
# 561 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                                         ( Binop (_1, Equal, _3) )
# 569 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                                       ( Binop (_1, Neq, _3) )
# 577 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                                        ( Binop (_1, Or, _3) )
# 585 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                                       ( Binop (_1, And, _3) )
# 593 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
                                       ( Unop(Not, _2) )
# 600 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                                         ( Unop(Neg, _2) )
# 607 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                                         ( Assign(_1, _3) )
# 615 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 77 "parser.mly"
                                         ( Call(_1, _3) )
# 623 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 78 "parser.mly"
                                         ( _2 )
# 630 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
                  ( Noexpr )
# 636 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
                  ( _1 )
# 643 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
           ( Int )
# 649 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
           ( Char )
# 655 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
           ( String )
# 661 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
           ( Bool )
# 667 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'obj_type) in
    Obj.repr(
# 89 "parser.mly"
           ( _1 )
# 674 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
           ( Node )
# 680 "parser.ml"
               : 'obj_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
           ( NodeType )
# 686 "parser.ml"
               : 'obj_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
           ( Edge )
# 692 "parser.ml"
               : 'obj_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
           ( EdgeType )
# 698 "parser.ml"
               : 'obj_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
           ( Graph )
# 704 "parser.ml"
               : 'obj_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 97 "parser.mly"
           ( UserDef )
# 711 "parser.ml"
               : 'obj_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser.mly"
                 ( [] )
# 717 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 101 "parser.mly"
                 ( List.rev _1 )
# 724 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                            ( [_1] )
# 731 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                            ( _3 :: _1 )
# 739 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 108 "parser.mly"
                             ( [_1])
# 746 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 109 "parser.mly"
                             ( _3 :: _1 )
# 754 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
                 ( [] )
# 760 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 113 "parser.mly"
                 ( List.rev _1 )
# 767 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "parser.mly"
                   ( [] )
# 773 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 117 "parser.mly"
                   ( _2 :: _1 )
# 781 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 120 "parser.mly"
                          ( {locals = []; statements = List.rev _2; block_num = inc_block_num ()} )
# 788 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 123 "parser.mly"
                                                                  ( Block(_1))
# 795 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                                                                  ( Expr(_1) )
# 802 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                                                                  ( Return(_2) )
# 809 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 127 "parser.mly"
                                                                  ( If(_3, _5, {locals = []; statements = []; block_num = inc_block_num ()}) )
# 817 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'block) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 128 "parser.mly"
                                                                  ( If (_3, _5, _7) )
# 826 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 129 "parser.mly"
                                                                  ( For(_3, _5, _7, _9) )
# 836 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 130 "parser.mly"
                                                                  ( While(_3, _5) )
# 844 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'any_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 133 "parser.mly"
              (_2, _1)
# 852 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl) in
    Obj.repr(
# 136 "parser.mly"
             ( _1 )
# 859 "parser.ml"
               : 'glb_vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "parser.mly"
                          ( [] )
# 865 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl) in
    Obj.repr(
# 141 "parser.mly"
                          ( _2 :: _1 )
# 873 "parser.ml"
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
# 887 "parser.ml"
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
# 900 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 156 "parser.mly"
                 ( [], [] )
# 906 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'glb_vdecl) in
    Obj.repr(
# 157 "parser.mly"
                     ( (_2 :: fst _1), snd _1 )
# 914 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 158 "parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 922 "parser.ml"
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
