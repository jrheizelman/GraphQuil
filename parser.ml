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
# 1 "parser.mly"
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
\015\000\015\000\016\000\001\000\001\000\001\000\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\002\000\002\000\003\000\004\000\003\000\000\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\000\000\001\000\001\000\003\000\
\001\000\003\000\000\000\001\000\000\000\002\000\003\000\001\000\
\002\000\003\000\005\000\007\000\009\000\005\000\002\000\002\000\
\000\000\003\000\009\000\000\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\060\000\000\000\000\000\032\000\034\000\035\000\031\000\029\000\
\028\000\033\000\026\000\027\000\036\000\000\000\030\000\000\000\
\061\000\062\000\000\000\056\000\000\000\000\000\000\000\041\000\
\000\000\055\000\000\000\000\000\042\000\057\000\000\000\000\000\
\000\000\058\000\000\000\045\000\059\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\005\000\000\000\004\000\002\000\
\000\000\046\000\048\000\000\000\000\000\020\000\019\000\000\000\
\000\000\000\000\000\000\000\000\049\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\023\000\047\000\000\000\000\000\000\000\
\000\000\050\000\000\000\000\000\000\000\010\000\000\000\008\000\
\000\000\009\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\022\000\000\000\
\000\000\054\000\000\000\000\000\000\000\000\000\052\000\000\000\
\000\000\053\000"

let yydgoto = "\002\000\
\003\000\080\000\084\000\081\000\022\000\015\000\085\000\023\000\
\016\000\025\000\033\000\050\000\051\000\017\000\031\000\018\000"

let yysindex = "\062\000\
\000\000\000\000\145\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\010\255\000\000\073\255\
\000\000\000\000\078\255\000\000\145\255\024\255\074\255\000\000\
\076\255\000\000\145\255\082\255\000\000\000\000\145\255\085\255\
\042\255\000\000\117\255\000\000\000\000\117\255\117\255\088\255\
\089\255\090\255\117\255\000\000\000\000\091\255\000\000\000\000\
\210\255\000\000\000\000\166\000\055\255\000\000\000\000\117\255\
\117\255\117\255\230\255\117\255\000\000\117\255\117\255\117\255\
\117\255\117\255\117\255\117\255\117\255\117\255\117\255\117\255\
\117\255\117\255\117\255\000\000\000\000\187\000\208\000\227\000\
\097\255\000\000\227\000\098\255\096\255\000\000\012\255\000\000\
\012\255\000\000\006\001\227\000\006\001\132\255\132\255\132\255\
\132\255\246\000\250\254\103\255\103\255\117\255\000\000\117\255\
\064\255\000\000\110\255\227\000\103\255\117\255\000\000\112\255\
\103\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\120\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\119\255\000\000\121\255\000\000\122\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\093\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\190\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\125\255\000\000\126\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\072\255\
\000\000\000\000\020\255\000\000\127\255\000\000\253\255\000\000\
\020\000\000\000\135\000\059\255\139\000\043\000\066\000\089\000\
\112\000\153\000\044\255\000\000\000\000\125\255\000\000\000\000\
\106\255\000\000\000\000\045\255\000\000\128\255\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\223\255\000\000\205\255\132\000\000\000\000\000\000\000\
\246\255\000\000\107\000\000\000\201\255\000\000\000\000\000\000"

let yytablesize = 540
let yytable = "\049\000\
\062\000\052\000\063\000\064\000\054\000\055\000\065\000\066\000\
\067\000\059\000\024\000\069\000\070\000\071\000\072\000\073\000\
\029\000\074\000\062\000\049\000\032\000\064\000\078\000\079\000\
\039\000\066\000\083\000\039\000\086\000\087\000\088\000\089\000\
\090\000\091\000\092\000\093\000\094\000\095\000\096\000\097\000\
\098\000\099\000\035\000\036\000\105\000\106\000\017\000\037\000\
\017\000\040\000\107\000\017\000\040\000\111\000\038\000\035\000\
\036\000\114\000\112\000\017\000\077\000\021\000\001\000\021\000\
\039\000\019\000\021\000\038\000\017\000\040\000\108\000\041\000\
\042\000\043\000\025\000\020\000\025\000\039\000\021\000\026\000\
\028\000\027\000\040\000\030\000\041\000\042\000\043\000\034\000\
\056\000\057\000\058\000\060\000\109\000\045\000\045\000\044\000\
\045\000\046\000\045\000\102\000\047\000\048\000\103\000\104\000\
\036\000\045\000\051\000\051\000\044\000\045\000\046\000\051\000\
\110\000\047\000\048\000\045\000\113\000\035\000\051\000\063\000\
\045\000\055\000\045\000\045\000\045\000\043\000\044\000\024\000\
\051\000\038\000\037\000\038\000\024\000\051\000\014\000\051\000\
\051\000\051\000\062\000\039\000\063\000\064\000\053\000\000\000\
\065\000\066\000\045\000\045\000\045\000\000\000\000\000\045\000\
\045\000\000\000\000\000\000\000\000\000\000\000\000\000\051\000\
\051\000\051\000\000\000\000\000\051\000\051\000\000\000\000\000\
\000\000\000\000\044\000\045\000\046\000\000\000\000\000\047\000\
\048\000\004\000\005\000\006\000\007\000\008\000\009\000\000\000\
\000\000\000\000\000\000\010\000\000\000\000\000\011\000\000\000\
\003\000\000\000\003\000\012\000\003\000\003\000\003\000\003\000\
\000\000\013\000\003\000\003\000\003\000\003\000\000\000\003\000\
\003\000\003\000\003\000\003\000\061\000\003\000\003\000\000\000\
\062\000\000\000\063\000\064\000\000\000\000\000\065\000\066\000\
\067\000\068\000\000\000\069\000\070\000\071\000\072\000\073\000\
\082\000\074\000\075\000\000\000\062\000\000\000\063\000\064\000\
\000\000\000\000\065\000\066\000\067\000\068\000\000\000\069\000\
\070\000\071\000\072\000\073\000\000\000\074\000\075\000\006\000\
\000\000\006\000\000\000\000\000\006\000\006\000\000\000\000\000\
\000\000\006\000\000\000\006\000\006\000\000\000\006\000\006\000\
\006\000\006\000\006\000\000\000\006\000\006\000\007\000\000\000\
\007\000\000\000\000\000\007\000\007\000\000\000\000\000\000\000\
\007\000\000\000\007\000\007\000\000\000\007\000\007\000\007\000\
\007\000\007\000\000\000\007\000\007\000\013\000\000\000\013\000\
\000\000\000\000\013\000\000\000\000\000\000\000\000\000\000\000\
\000\000\013\000\013\000\000\000\013\000\013\000\013\000\013\000\
\013\000\000\000\013\000\013\000\014\000\000\000\014\000\000\000\
\000\000\014\000\000\000\000\000\000\000\000\000\000\000\000\000\
\014\000\014\000\000\000\014\000\014\000\014\000\014\000\014\000\
\000\000\014\000\014\000\011\000\000\000\011\000\000\000\000\000\
\011\000\000\000\000\000\000\000\000\000\000\000\000\000\011\000\
\011\000\000\000\011\000\011\000\011\000\011\000\011\000\000\000\
\011\000\011\000\012\000\000\000\012\000\000\000\000\000\012\000\
\000\000\000\000\000\000\000\000\000\000\000\000\012\000\012\000\
\000\000\012\000\012\000\012\000\012\000\012\000\000\000\012\000\
\012\000\015\000\000\000\015\000\000\000\016\000\015\000\016\000\
\000\000\000\000\016\000\000\000\000\000\015\000\015\000\000\000\
\015\000\016\000\016\000\018\000\016\000\018\000\015\000\015\000\
\018\000\000\000\016\000\016\000\000\000\000\000\000\000\000\000\
\018\000\000\000\076\000\000\000\062\000\000\000\063\000\064\000\
\018\000\018\000\065\000\066\000\067\000\068\000\000\000\069\000\
\070\000\071\000\072\000\073\000\000\000\074\000\075\000\100\000\
\000\000\062\000\000\000\063\000\064\000\000\000\000\000\065\000\
\066\000\067\000\068\000\000\000\069\000\070\000\071\000\072\000\
\073\000\000\000\074\000\075\000\101\000\000\000\062\000\000\000\
\063\000\064\000\000\000\000\000\065\000\066\000\067\000\068\000\
\000\000\069\000\070\000\071\000\072\000\073\000\000\000\074\000\
\075\000\062\000\000\000\063\000\064\000\000\000\000\000\065\000\
\066\000\067\000\068\000\000\000\069\000\070\000\071\000\072\000\
\073\000\000\000\074\000\075\000\062\000\000\000\063\000\064\000\
\000\000\000\000\065\000\066\000\067\000\000\000\000\000\069\000\
\070\000\071\000\072\000\073\000\062\000\000\000\063\000\064\000\
\000\000\000\000\065\000\066\000\000\000\000\000\000\000\000\000\
\070\000\071\000\072\000\073\000"

let yycheck = "\033\000\
\007\001\035\000\009\001\010\001\038\000\039\000\013\001\014\001\
\015\001\043\000\021\000\018\001\019\001\020\001\021\001\022\001\
\027\000\024\001\007\001\053\000\031\000\010\001\056\000\057\000\
\005\001\014\001\060\000\008\001\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\069\000\070\000\071\000\072\000\073\000\
\074\000\075\000\001\001\002\001\100\000\101\000\003\001\006\001\
\005\001\005\001\102\000\008\001\008\001\109\000\013\001\001\001\
\002\001\113\000\110\000\016\001\006\001\003\001\001\000\005\001\
\023\001\056\001\008\001\013\001\025\001\028\001\104\000\030\001\
\031\001\032\001\003\001\003\001\005\001\023\001\001\001\056\001\
\005\001\008\001\028\001\002\001\030\001\031\001\032\001\003\001\
\001\001\001\001\001\001\001\001\029\001\001\001\002\001\054\001\
\055\001\056\001\006\001\003\001\059\001\060\001\005\001\008\001\
\002\001\013\001\001\001\002\001\054\001\055\001\056\001\006\001\
\003\001\059\001\060\001\023\001\005\001\001\001\013\001\000\000\
\028\001\003\001\030\001\031\001\032\001\005\001\005\001\003\001\
\023\001\013\001\005\001\005\001\005\001\028\001\003\000\030\001\
\031\001\032\001\007\001\023\001\009\001\010\001\036\000\255\255\
\013\001\014\001\054\001\055\001\056\001\255\255\255\255\059\001\
\060\001\255\255\255\255\255\255\255\255\255\255\255\255\054\001\
\055\001\056\001\255\255\255\255\059\001\060\001\255\255\255\255\
\255\255\255\255\054\001\055\001\056\001\255\255\255\255\059\001\
\060\001\033\001\034\001\035\001\036\001\037\001\038\001\255\255\
\255\255\255\255\255\255\043\001\255\255\255\255\046\001\255\255\
\003\001\255\255\005\001\051\001\007\001\008\001\009\001\010\001\
\255\255\057\001\013\001\014\001\015\001\016\001\255\255\018\001\
\019\001\020\001\021\001\022\001\003\001\024\001\025\001\255\255\
\007\001\255\255\009\001\010\001\255\255\255\255\013\001\014\001\
\015\001\016\001\255\255\018\001\019\001\020\001\021\001\022\001\
\003\001\024\001\025\001\255\255\007\001\255\255\009\001\010\001\
\255\255\255\255\013\001\014\001\015\001\016\001\255\255\018\001\
\019\001\020\001\021\001\022\001\255\255\024\001\025\001\003\001\
\255\255\005\001\255\255\255\255\008\001\009\001\255\255\255\255\
\255\255\013\001\255\255\015\001\016\001\255\255\018\001\019\001\
\020\001\021\001\022\001\255\255\024\001\025\001\003\001\255\255\
\005\001\255\255\255\255\008\001\009\001\255\255\255\255\255\255\
\013\001\255\255\015\001\016\001\255\255\018\001\019\001\020\001\
\021\001\022\001\255\255\024\001\025\001\003\001\255\255\005\001\
\255\255\255\255\008\001\255\255\255\255\255\255\255\255\255\255\
\255\255\015\001\016\001\255\255\018\001\019\001\020\001\021\001\
\022\001\255\255\024\001\025\001\003\001\255\255\005\001\255\255\
\255\255\008\001\255\255\255\255\255\255\255\255\255\255\255\255\
\015\001\016\001\255\255\018\001\019\001\020\001\021\001\022\001\
\255\255\024\001\025\001\003\001\255\255\005\001\255\255\255\255\
\008\001\255\255\255\255\255\255\255\255\255\255\255\255\015\001\
\016\001\255\255\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\025\001\003\001\255\255\005\001\255\255\255\255\008\001\
\255\255\255\255\255\255\255\255\255\255\255\255\015\001\016\001\
\255\255\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\025\001\003\001\255\255\005\001\255\255\003\001\008\001\005\001\
\255\255\255\255\008\001\255\255\255\255\015\001\016\001\255\255\
\018\001\015\001\016\001\003\001\018\001\005\001\024\001\025\001\
\008\001\255\255\024\001\025\001\255\255\255\255\255\255\255\255\
\016\001\255\255\005\001\255\255\007\001\255\255\009\001\010\001\
\024\001\025\001\013\001\014\001\015\001\016\001\255\255\018\001\
\019\001\020\001\021\001\022\001\255\255\024\001\025\001\005\001\
\255\255\007\001\255\255\009\001\010\001\255\255\255\255\013\001\
\014\001\015\001\016\001\255\255\018\001\019\001\020\001\021\001\
\022\001\255\255\024\001\025\001\005\001\255\255\007\001\255\255\
\009\001\010\001\255\255\255\255\013\001\014\001\015\001\016\001\
\255\255\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\025\001\007\001\255\255\009\001\010\001\255\255\255\255\013\001\
\014\001\015\001\016\001\255\255\018\001\019\001\020\001\021\001\
\022\001\255\255\024\001\025\001\007\001\255\255\009\001\010\001\
\255\255\255\255\013\001\014\001\015\001\255\255\255\255\018\001\
\019\001\020\001\021\001\022\001\007\001\255\255\009\001\010\001\
\255\255\255\255\013\001\014\001\255\255\255\255\255\255\255\255\
\019\001\020\001\021\001\022\001"

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
# 50 "parser.mly"
                                         ( Literal(_1) )
# 444 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 51 "parser.mly"
                                         ( Char(_1) )
# 451 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "parser.mly"
                                         ( Id(_1))
# 458 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "parser.mly"
                                         ( String_Lit(_1) )
# 465 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 54 "parser.mly"
                                         ( Bool_Lit(_1))
# 472 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 55 "parser.mly"
                                         ( Binop (_1, Add, _3) )
# 480 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 56 "parser.mly"
                                        ( Binop (_1, Sub, _3) )
# 488 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 57 "parser.mly"
                                        ( Binop (_1, Mult, _3) )
# 496 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 58 "parser.mly"
                                         ( Binop (_1, Div, _3) )
# 504 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 59 "parser.mly"
                                       ( Binop (_1, Mod, _1) )
# 512 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 60 "parser.mly"
                                        ( Binop (_1, Less, _3) )
# 520 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 61 "parser.mly"
                                        ( Binop (_1, Greater, _3) )
# 528 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 62 "parser.mly"
                                       ( Binop (_1, Leq, _3) )
# 536 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
                                       ( Binop (_1, Geq, _3) )
# 544 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                                         ( Binop (_1, Equal, _3) )
# 552 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                                       ( Binop (_1, Neq, _3) )
# 560 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                                        ( Binop (_1, Or, _3) )
# 568 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                                       ( Binop (_1, And, _3) )
# 576 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                                       ( Unop(Not, _2) )
# 583 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                                         ( Unop(Neg, _2) )
# 590 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                                         ( Assign(_1, _3) )
# 598 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 71 "parser.mly"
                                         ( Call(_1, _3) )
# 606 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                                         ( _2 )
# 613 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
                  ( Noexpr )
# 619 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                  ( _1 )
# 626 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
           ( Int )
# 632 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
           ( Char )
# 638 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
           ( String )
# 644 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
           ( Bool )
# 650 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'obj_type) in
    Obj.repr(
# 83 "parser.mly"
           ( _1 )
# 657 "parser.ml"
               : 'any_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
           ( Node )
# 663 "parser.ml"
               : 'obj_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
           ( NodeType )
# 669 "parser.ml"
               : 'obj_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
           ( Edge )
# 675 "parser.ml"
               : 'obj_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
           ( EdgeType )
# 681 "parser.ml"
               : 'obj_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
           ( Graph )
# 687 "parser.ml"
               : 'obj_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "parser.mly"
           ( UserDef )
# 694 "parser.ml"
               : 'obj_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
                 ( [] )
# 700 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 95 "parser.mly"
                 ( List.rev _1 )
# 707 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                            ( [_1] )
# 714 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                            ( _3 :: _1 )
# 722 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 102 "parser.mly"
                             ( [_1])
# 729 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 103 "parser.mly"
                             ( _3 :: _1 )
# 737 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "parser.mly"
                 ( [] )
# 743 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 107 "parser.mly"
                 ( List.rev _1 )
# 750 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
                   ( [] )
# 756 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 111 "parser.mly"
                   ( _2 :: _1 )
# 764 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 114 "parser.mly"
                          ( {locals = []; statements = List.rev _2; block_num = inc_block_num ()} )
# 771 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 117 "parser.mly"
                                                                  ( Block(_1))
# 778 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                                                                  ( Expr(_1) )
# 785 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                                                                  ( Return(_2) )
# 792 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 120 "parser.mly"
                                                                  ( If(_3, _5, {locals = []; statements = []; block_num = inc_block_num ()}) )
# 800 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'block) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 121 "parser.mly"
                                                                  ( If (_3, _5, _7) )
# 809 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 122 "parser.mly"
                                                                  ( For(_3, _5, _7, _9) )
# 819 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 123 "parser.mly"
                                                                  ( While(_3, _5) )
# 827 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'any_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 126 "parser.mly"
              (_2, _1)
# 835 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl) in
    Obj.repr(
# 129 "parser.mly"
             ( _1 )
# 842 "parser.ml"
               : 'glb_vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "parser.mly"
                          ( [] )
# 848 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl) in
    Obj.repr(
# 134 "parser.mly"
                          ( _2 :: _1 )
# 856 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'any_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 138 "parser.mly"
     ( { fname = _2;
         formals = _4; 
         body_block = {locals = List.rev _7; statements = List.rev _8; block_num = inc_block_num()} ;
         ret = _1 } )
# 870 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 144 "parser.mly"
                 ( [], [] )
# 876 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'glb_vdecl) in
    Obj.repr(
# 145 "parser.mly"
                     ( (_2 :: fst _1), snd _1 )
# 884 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 146 "parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 892 "parser.ml"
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
