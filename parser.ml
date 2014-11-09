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
  | STRUCT
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
  | NUM of (int)
  | ID of (string)

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 62 "parser.ml"
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
  293 (* STRUCT *);
  294 (* PRINT *);
  295 (* NEW *);
  296 (* CONTINUE *);
  297 (* DOUBLE *);
  298 (* FALSE *);
  299 (* TRUE *);
  300 (* INT *);
  301 (* VOID *);
  302 (* DEST *);
  303 (* EDGES *);
  304 (* STATIC *);
  305 (* CHAR *);
  306 (* DO *);
  307 (* IN *);
    0|]

let yytransl_block = [|
  308 (* NUM *);
  309 (* ID *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\003\000\003\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\005\000\005\000\006\000\006\000\
\007\000\007\000\008\000\008\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\010\000\010\000\010\000\011\000\011\000\
\012\000\001\000\001\000\001\000\000\000"

let yylen = "\002\000\
\000\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\004\000\003\000\001\000\000\000\001\000\001\000\003\000\
\000\000\001\000\000\000\002\000\002\000\003\000\003\000\005\000\
\007\000\009\000\005\000\003\000\003\000\003\000\000\000\002\000\
\008\000\000\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\042\000\000\000\000\000\000\000\000\000\000\000\000\000\043\000\
\044\000\000\000\000\000\000\000\000\000\037\000\038\000\036\000\
\023\000\000\000\000\000\000\000\000\000\024\000\039\000\000\000\
\000\000\040\000\000\000\027\000\041\000\000\000\000\000\000\000\
\000\000\020\000\000\000\000\000\028\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\029\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\019\000\031\000\000\000\000\000\000\000\000\000\
\030\000\000\000\000\000\000\000\000\000\000\000\006\000\000\000\
\008\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\018\000\000\000\000\000\035\000\
\000\000\000\000\000\000\000\000\033\000\000\000\000\000\034\000"

let yydgoto = "\002\000\
\003\000\066\000\067\000\036\000\064\000\018\000\019\000\025\000\
\037\000\008\000\024\000\009\000"

let yysindex = "\008\000\
\000\000\000\000\222\254\205\254\235\254\247\254\044\255\000\000\
\000\000\047\255\048\255\061\255\020\255\000\000\000\000\000\000\
\000\000\071\255\079\255\037\255\089\255\000\000\000\000\042\255\
\010\255\000\000\003\255\000\000\000\000\115\255\124\255\126\255\
\003\255\000\000\032\255\099\255\000\000\040\000\040\255\003\255\
\003\255\003\255\119\255\003\255\003\255\000\000\003\255\003\255\
\003\255\003\255\003\255\003\255\003\255\003\255\003\255\003\255\
\003\255\003\255\000\000\000\000\058\000\076\000\092\000\129\255\
\000\000\131\255\134\255\092\000\092\000\056\255\000\000\056\255\
\000\000\136\000\136\000\086\255\086\255\086\255\086\255\123\000\
\108\000\059\255\059\255\003\255\000\000\003\255\127\255\000\000\
\148\255\092\000\059\255\003\255\000\000\150\255\059\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\153\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\152\255\000\000\000\000\000\000\
\000\000\154\255\000\000\000\000\000\000\000\000\000\000\046\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\162\255\000\000\164\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\103\255\000\000\
\000\000\000\000\166\255\029\255\000\255\142\255\000\000\165\255\
\000\000\024\000\028\000\188\255\211\255\234\255\001\000\197\255\
\123\255\000\000\000\000\162\255\000\000\000\000\052\255\000\000\
\000\000\141\255\000\000\079\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\229\255\207\255\000\000\000\000\144\000\
\012\000\150\000\000\000\000\000"

let yytablesize = 414
let yytable = "\038\000\
\004\000\010\000\017\000\027\000\017\000\043\000\005\000\017\000\
\001\000\006\000\027\000\028\000\061\000\062\000\063\000\029\000\
\068\000\069\000\007\000\070\000\071\000\072\000\073\000\074\000\
\075\000\076\000\077\000\078\000\079\000\080\000\081\000\011\000\
\044\000\003\000\089\000\030\000\003\000\031\000\032\000\033\000\
\027\000\028\000\094\000\012\000\013\000\060\000\027\000\027\000\
\045\000\014\000\015\000\027\000\032\000\032\000\034\000\035\000\
\063\000\032\000\090\000\027\000\028\000\034\000\035\000\016\000\
\063\000\030\000\048\000\031\000\032\000\033\000\050\000\027\000\
\017\000\027\000\027\000\027\000\004\000\032\000\020\000\032\000\
\032\000\032\000\005\000\021\000\030\000\006\000\031\000\032\000\
\033\000\022\000\023\000\034\000\035\000\087\000\088\000\047\000\
\048\000\027\000\027\000\049\000\050\000\046\000\093\000\032\000\
\032\000\022\000\096\000\022\000\047\000\048\000\034\000\035\000\
\049\000\050\000\051\000\040\000\052\000\053\000\054\000\055\000\
\056\000\065\000\057\000\058\000\041\000\015\000\042\000\015\000\
\047\000\048\000\015\000\084\000\049\000\050\000\051\000\085\000\
\052\000\053\000\054\000\055\000\056\000\086\000\057\000\058\000\
\005\000\004\000\005\000\015\000\004\000\005\000\092\000\005\000\
\045\000\091\000\095\000\005\000\025\000\005\000\026\000\005\000\
\005\000\005\000\005\000\005\000\021\000\005\000\005\000\007\000\
\001\000\007\000\002\000\039\000\007\000\026\000\007\000\000\000\
\000\000\000\000\007\000\000\000\007\000\000\000\007\000\007\000\
\007\000\007\000\007\000\000\000\007\000\007\000\011\000\000\000\
\011\000\000\000\000\000\011\000\000\000\000\000\000\000\016\000\
\000\000\016\000\000\000\011\000\016\000\011\000\011\000\011\000\
\011\000\011\000\000\000\011\000\011\000\012\000\000\000\012\000\
\000\000\000\000\012\000\000\000\016\000\016\000\000\000\000\000\
\000\000\000\000\012\000\000\000\012\000\012\000\012\000\012\000\
\012\000\000\000\012\000\012\000\009\000\000\000\009\000\000\000\
\000\000\009\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\009\000\000\000\009\000\009\000\009\000\009\000\009\000\
\000\000\009\000\009\000\010\000\000\000\010\000\000\000\000\000\
\010\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\010\000\000\000\010\000\010\000\010\000\010\000\010\000\000\000\
\010\000\010\000\013\000\000\000\013\000\000\000\014\000\013\000\
\014\000\000\000\000\000\014\000\000\000\000\000\000\000\013\000\
\000\000\013\000\000\000\014\000\059\000\014\000\000\000\013\000\
\013\000\047\000\048\000\014\000\014\000\049\000\050\000\051\000\
\000\000\052\000\053\000\054\000\055\000\056\000\082\000\057\000\
\058\000\000\000\000\000\047\000\048\000\000\000\000\000\049\000\
\050\000\051\000\000\000\052\000\053\000\054\000\055\000\056\000\
\083\000\057\000\058\000\000\000\000\000\047\000\048\000\000\000\
\000\000\049\000\050\000\051\000\000\000\052\000\053\000\054\000\
\055\000\056\000\000\000\057\000\058\000\047\000\048\000\000\000\
\000\000\049\000\050\000\051\000\000\000\052\000\053\000\054\000\
\055\000\056\000\000\000\057\000\058\000\047\000\048\000\000\000\
\000\000\049\000\050\000\051\000\000\000\052\000\053\000\054\000\
\055\000\056\000\000\000\057\000\047\000\048\000\000\000\000\000\
\049\000\050\000\051\000\000\000\052\000\053\000\054\000\055\000\
\056\000\047\000\048\000\000\000\000\000\049\000\050\000\000\000\
\000\000\000\000\053\000\054\000\055\000\056\000"

let yycheck = "\027\000\
\035\001\053\001\003\001\001\001\005\001\033\000\041\001\008\001\
\001\000\044\001\001\001\002\001\040\000\041\000\042\000\006\001\
\044\000\045\000\053\001\047\000\048\000\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\056\000\057\000\058\000\053\001\
\001\001\005\001\084\000\026\001\008\001\028\001\029\001\030\001\
\001\001\002\001\092\000\053\001\001\001\006\001\001\001\002\001\
\017\001\003\001\003\001\006\001\001\001\002\001\052\001\053\001\
\084\000\006\001\086\000\001\001\002\001\052\001\053\001\003\001\
\092\000\026\001\011\001\028\001\029\001\030\001\015\001\026\001\
\053\001\028\001\029\001\030\001\035\001\026\001\008\001\028\001\
\029\001\030\001\041\001\005\001\026\001\044\001\028\001\029\001\
\030\001\053\001\002\001\052\001\053\001\082\000\083\000\010\001\
\011\001\052\001\053\001\014\001\015\001\003\001\091\000\052\001\
\053\001\003\001\095\000\005\001\010\001\011\001\052\001\053\001\
\014\001\015\001\016\001\001\001\018\001\019\001\020\001\021\001\
\022\001\003\001\024\001\025\001\001\001\003\001\001\001\005\001\
\010\001\011\001\008\001\003\001\014\001\015\001\016\001\005\001\
\018\001\019\001\020\001\021\001\022\001\008\001\024\001\025\001\
\003\001\005\001\005\001\025\001\008\001\008\001\003\001\010\001\
\000\000\027\001\005\001\014\001\005\001\016\001\005\001\018\001\
\019\001\020\001\021\001\022\001\003\001\024\001\025\001\003\001\
\005\001\005\001\005\001\028\000\008\001\024\000\010\001\255\255\
\255\255\255\255\014\001\255\255\016\001\255\255\018\001\019\001\
\020\001\021\001\022\001\255\255\024\001\025\001\003\001\255\255\
\005\001\255\255\255\255\008\001\255\255\255\255\255\255\003\001\
\255\255\005\001\255\255\016\001\008\001\018\001\019\001\020\001\
\021\001\022\001\255\255\024\001\025\001\003\001\255\255\005\001\
\255\255\255\255\008\001\255\255\024\001\025\001\255\255\255\255\
\255\255\255\255\016\001\255\255\018\001\019\001\020\001\021\001\
\022\001\255\255\024\001\025\001\003\001\255\255\005\001\255\255\
\255\255\008\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\016\001\255\255\018\001\019\001\020\001\021\001\022\001\
\255\255\024\001\025\001\003\001\255\255\005\001\255\255\255\255\
\008\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\016\001\255\255\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\025\001\003\001\255\255\005\001\255\255\003\001\008\001\
\005\001\255\255\255\255\008\001\255\255\255\255\255\255\016\001\
\255\255\018\001\255\255\016\001\005\001\018\001\255\255\024\001\
\025\001\010\001\011\001\024\001\025\001\014\001\015\001\016\001\
\255\255\018\001\019\001\020\001\021\001\022\001\005\001\024\001\
\025\001\255\255\255\255\010\001\011\001\255\255\255\255\014\001\
\015\001\016\001\255\255\018\001\019\001\020\001\021\001\022\001\
\005\001\024\001\025\001\255\255\255\255\010\001\011\001\255\255\
\255\255\014\001\015\001\016\001\255\255\018\001\019\001\020\001\
\021\001\022\001\255\255\024\001\025\001\010\001\011\001\255\255\
\255\255\014\001\015\001\016\001\255\255\018\001\019\001\020\001\
\021\001\022\001\255\255\024\001\025\001\010\001\011\001\255\255\
\255\255\014\001\015\001\016\001\255\255\018\001\019\001\020\001\
\021\001\022\001\255\255\024\001\010\001\011\001\255\255\255\255\
\014\001\015\001\016\001\255\255\018\001\019\001\020\001\021\001\
\022\001\010\001\011\001\255\255\255\255\014\001\015\001\255\255\
\255\255\255\255\019\001\020\001\021\001\022\001"

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
  STRUCT\000\
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
  NUM\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 32 "parser.mly"
                ( [] )
# 364 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 33 "parser.mly"
                 ( List.rev _1 )
# 371 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 36 "parser.mly"
       ( [_1] )
# 378 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 37 "parser.mly"
                            ( _3 :: _1 )
# 386 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 40 "parser.mly"
               ( Binop (_1, Add, _3))
# 394 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 41 "parser.mly"
                 ()
# 402 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 42 "parser.mly"
                 ()
# 410 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 43 "parser.mly"
                  ()
# 418 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 44 "parser.mly"
              ()
# 426 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 45 "parser.mly"
              ()
# 434 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 46 "parser.mly"
               ()
# 442 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 47 "parser.mly"
               ()
# 450 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 48 "parser.mly"
              ()
# 458 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 49 "parser.mly"
               ()
# 466 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 50 "parser.mly"
              ()
# 474 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 51 "parser.mly"
               ()
# 482 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 52 "parser.mly"
                ( Assign(_1, _3) )
# 490 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 53 "parser.mly"
                              ( Call(_1, _3) )
# 498 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 54 "parser.mly"
                    ( _2 )
# 505 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 55 "parser.mly"
     ( Lit(_1))
# 512 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
                (Noexpr )
# 518 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 60 "parser.mly"
         ( _1 )
# 525 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "parser.mly"
     ( [_1] )
# 532 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "parser.mly"
                         ( _3 :: _1 )
# 540 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
                ( [] )
# 546 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 68 "parser.mly"
                ( List.rev _1 )
# 553 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
                ( [] )
# 559 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 72 "parser.mly"
                   ( _2 :: _1 )
# 567 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
            ( Expr(_1) )
# 574 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                     ( Return(_2) )
# 581 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 77 "parser.mly"
                            ( Block(List.rev _2) )
# 588 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 78 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 596 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 79 "parser.mly"
                                         ( If (_3, _5, _7) )
# 605 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 81 "parser.mly"
   ( For(_3, _5, _7, _9) )
# 615 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 82 "parser.mly"
                                  ( While(_3, _5) )
# 623 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 85 "parser.mly"
              ( _2 )
# 630 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 86 "parser.mly"
                 ( _2 )
# 637 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 87 "parser.mly"
                   ( _2 )
# 644 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
                ( [] )
# 650 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 91 "parser.mly"
                     ( _2 :: _1 )
# 658 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 95 "parser.mly"
    ({  fname = _1;
    	formals = _3;
    	locals = List.rev _6;
    	body = List.rev _7; })
# 671 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser.mly"
               ( [], [] )
# 677 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : unit) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 102 "parser.mly"
                 ( (_2 :: fst _1), snd _1 )
# 685 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : unit) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 103 "parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 693 "parser.ml"
               : unit))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : unit)
