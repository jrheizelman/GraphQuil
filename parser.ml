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
\004\000\004\000\004\000\004\000\004\000\004\000\005\000\005\000\
\006\000\006\000\007\000\007\000\008\000\008\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\010\000\010\000\010\000\
\010\000\010\000\011\000\011\000\012\000\001\000\001\000\001\000\
\000\000"

let yylen = "\002\000\
\000\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\003\000\004\000\003\000\001\000\000\000\001\000\
\001\000\003\000\000\000\001\000\000\000\002\000\002\000\003\000\
\003\000\005\000\007\000\009\000\005\000\003\000\003\000\003\000\
\003\000\003\000\000\000\002\000\008\000\000\000\002\000\002\000\
\002\000"

let yydefred = "\000\000\
\046\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\047\000\048\000\000\000\000\000\000\000\000\000\000\000\
\000\000\039\000\041\000\040\000\038\000\042\000\025\000\000\000\
\000\000\000\000\000\000\026\000\043\000\000\000\000\000\044\000\
\000\000\029\000\045\000\000\000\000\000\000\000\000\000\000\000\
\022\000\000\000\000\000\030\000\000\000\000\000\018\000\000\000\
\000\000\000\000\000\000\000\000\000\000\031\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\021\000\033\000\000\000\000\000\000\000\
\000\000\032\000\000\000\000\000\000\000\000\000\009\000\000\000\
\007\000\000\000\008\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\020\000\000\000\
\000\000\037\000\000\000\000\000\000\000\000\000\035\000\000\000\
\000\000\036\000"

let yydgoto = "\002\000\
\003\000\075\000\076\000\043\000\073\000\024\000\025\000\031\000\
\044\000\010\000\030\000\011\000"

let yysindex = "\003\000\
\000\000\000\000\121\000\215\254\240\254\250\254\254\254\002\255\
\047\255\000\000\000\000\057\255\073\255\074\255\085\255\092\255\
\041\255\000\000\000\000\000\000\000\000\000\000\000\000\090\255\
\094\255\049\255\101\255\000\000\000\000\088\255\012\255\000\000\
\048\255\000\000\000\000\048\255\106\255\107\255\109\255\048\255\
\000\000\004\255\131\255\000\000\072\000\044\255\000\000\048\255\
\048\255\048\255\151\255\048\255\048\255\000\000\048\255\048\255\
\048\255\048\255\048\255\048\255\048\255\048\255\048\255\048\255\
\048\255\048\255\048\255\000\000\000\000\093\000\114\000\130\000\
\112\255\000\000\111\255\110\255\130\000\130\000\000\000\251\254\
\000\000\251\254\000\000\189\000\189\000\076\255\076\255\076\255\
\076\255\173\000\157\000\091\255\091\255\048\255\000\000\048\255\
\095\255\000\000\123\255\130\000\091\255\048\255\000\000\126\255\
\091\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\128\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\094\255\000\000\000\000\000\000\000\000\000\000\000\000\128\255\
\000\000\000\000\000\000\000\000\000\000\052\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\136\255\000\000\135\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\063\255\
\000\000\000\000\000\000\143\255\003\255\122\255\000\000\174\255\
\000\000\197\255\000\000\056\000\060\000\220\255\243\255\010\000\
\033\000\229\255\054\255\000\000\000\000\136\255\000\000\000\000\
\083\255\000\000\000\000\031\255\000\000\152\255\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\223\255\163\255\000\000\000\000\125\000\
\207\255\133\000\000\000\000\000"

let yytablesize = 467
let yytable = "\045\000\
\099\000\055\000\047\000\001\000\052\000\057\000\051\000\003\000\
\104\000\059\000\003\000\012\000\033\000\034\000\070\000\071\000\
\072\000\035\000\077\000\078\000\053\000\079\000\080\000\081\000\
\082\000\083\000\084\000\085\000\086\000\087\000\088\000\089\000\
\090\000\091\000\036\000\004\000\013\000\037\000\004\000\038\000\
\039\000\040\000\097\000\098\000\033\000\034\000\014\000\017\000\
\033\000\069\000\015\000\103\000\029\000\029\000\016\000\106\000\
\016\000\029\000\016\000\018\000\072\000\016\000\100\000\041\000\
\042\000\024\000\036\000\024\000\072\000\037\000\036\000\038\000\
\039\000\040\000\029\000\019\000\020\000\029\000\016\000\029\000\
\029\000\029\000\055\000\034\000\034\000\056\000\057\000\021\000\
\034\000\058\000\059\000\033\000\034\000\023\000\022\000\041\000\
\042\000\026\000\027\000\041\000\042\000\028\000\029\000\029\000\
\029\000\034\000\048\000\049\000\034\000\050\000\034\000\034\000\
\034\000\036\000\094\000\095\000\037\000\096\000\038\000\039\000\
\040\000\101\000\004\000\005\000\019\000\102\000\019\000\049\000\
\006\000\019\000\105\000\007\000\028\000\054\000\034\000\034\000\
\008\000\055\000\023\000\001\000\056\000\057\000\041\000\042\000\
\058\000\059\000\060\000\002\000\061\000\062\000\063\000\064\000\
\065\000\074\000\066\000\067\000\023\000\055\000\046\000\000\000\
\056\000\057\000\032\000\000\000\058\000\059\000\060\000\000\000\
\061\000\062\000\063\000\064\000\065\000\000\000\066\000\067\000\
\005\000\000\000\005\000\000\000\000\000\005\000\000\000\005\000\
\000\000\000\000\000\000\005\000\000\000\005\000\000\000\005\000\
\005\000\005\000\005\000\005\000\000\000\005\000\005\000\006\000\
\000\000\006\000\000\000\000\000\006\000\000\000\006\000\000\000\
\000\000\000\000\006\000\000\000\006\000\000\000\006\000\006\000\
\006\000\006\000\006\000\000\000\006\000\006\000\012\000\000\000\
\012\000\000\000\000\000\012\000\000\000\000\000\000\000\017\000\
\000\000\017\000\000\000\012\000\017\000\012\000\012\000\012\000\
\012\000\012\000\000\000\012\000\012\000\013\000\000\000\013\000\
\000\000\000\000\013\000\000\000\017\000\017\000\000\000\000\000\
\000\000\000\000\013\000\000\000\013\000\013\000\013\000\013\000\
\013\000\000\000\013\000\013\000\010\000\000\000\010\000\000\000\
\000\000\010\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\010\000\000\000\010\000\010\000\010\000\010\000\010\000\
\000\000\010\000\010\000\011\000\000\000\011\000\000\000\000\000\
\011\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\011\000\000\000\011\000\011\000\011\000\011\000\011\000\000\000\
\011\000\011\000\014\000\000\000\014\000\000\000\015\000\014\000\
\015\000\000\000\000\000\015\000\000\000\000\000\000\000\014\000\
\000\000\014\000\000\000\015\000\068\000\015\000\055\000\014\000\
\014\000\056\000\057\000\015\000\015\000\058\000\059\000\060\000\
\000\000\061\000\062\000\063\000\064\000\065\000\000\000\066\000\
\067\000\092\000\000\000\055\000\000\000\000\000\056\000\057\000\
\000\000\000\000\058\000\059\000\060\000\000\000\061\000\062\000\
\063\000\064\000\065\000\000\000\066\000\067\000\093\000\000\000\
\055\000\000\000\000\000\056\000\057\000\000\000\000\000\058\000\
\059\000\060\000\000\000\061\000\062\000\063\000\064\000\065\000\
\055\000\066\000\067\000\056\000\057\000\000\000\000\000\058\000\
\059\000\060\000\000\000\061\000\062\000\063\000\064\000\065\000\
\000\000\066\000\067\000\004\000\005\000\000\000\000\000\000\000\
\000\000\006\000\000\000\055\000\007\000\000\000\056\000\057\000\
\000\000\008\000\058\000\059\000\060\000\009\000\061\000\062\000\
\063\000\064\000\065\000\055\000\066\000\000\000\056\000\057\000\
\000\000\000\000\058\000\059\000\060\000\000\000\061\000\062\000\
\063\000\064\000\065\000\055\000\000\000\000\000\056\000\057\000\
\000\000\000\000\058\000\059\000\000\000\000\000\000\000\062\000\
\063\000\064\000\065\000"

let yycheck = "\033\000\
\094\000\007\001\036\000\001\000\001\001\011\001\040\000\005\001\
\102\000\015\001\008\001\053\001\001\001\002\001\048\000\049\000\
\050\000\006\001\052\000\053\000\017\001\055\000\056\000\057\000\
\058\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\023\001\005\001\053\001\026\001\008\001\028\001\
\029\001\030\001\092\000\093\000\001\001\002\001\053\001\001\001\
\001\001\006\001\053\001\101\000\001\001\002\001\053\001\105\000\
\003\001\006\001\005\001\003\001\094\000\008\001\096\000\052\001\
\053\001\003\001\023\001\005\001\102\000\026\001\023\001\028\001\
\029\001\030\001\023\001\003\001\003\001\026\001\025\001\028\001\
\029\001\030\001\007\001\001\001\002\001\010\001\011\001\003\001\
\006\001\014\001\015\001\001\001\002\001\053\001\003\001\052\001\
\053\001\008\001\005\001\052\001\053\001\053\001\002\001\052\001\
\053\001\023\001\001\001\001\001\026\001\001\001\028\001\029\001\
\030\001\023\001\003\001\005\001\026\001\008\001\028\001\029\001\
\030\001\027\001\035\001\036\001\003\001\003\001\005\001\000\000\
\041\001\008\001\005\001\044\001\005\001\003\001\052\001\053\001\
\049\001\007\001\003\001\005\001\010\001\011\001\052\001\053\001\
\014\001\015\001\016\001\005\001\018\001\019\001\020\001\021\001\
\022\001\003\001\024\001\025\001\005\001\007\001\034\000\255\255\
\010\001\011\001\030\000\255\255\014\001\015\001\016\001\255\255\
\018\001\019\001\020\001\021\001\022\001\255\255\024\001\025\001\
\003\001\255\255\005\001\255\255\255\255\008\001\255\255\010\001\
\255\255\255\255\255\255\014\001\255\255\016\001\255\255\018\001\
\019\001\020\001\021\001\022\001\255\255\024\001\025\001\003\001\
\255\255\005\001\255\255\255\255\008\001\255\255\010\001\255\255\
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
\255\255\018\001\255\255\016\001\005\001\018\001\007\001\024\001\
\025\001\010\001\011\001\024\001\025\001\014\001\015\001\016\001\
\255\255\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\025\001\005\001\255\255\007\001\255\255\255\255\010\001\011\001\
\255\255\255\255\014\001\015\001\016\001\255\255\018\001\019\001\
\020\001\021\001\022\001\255\255\024\001\025\001\005\001\255\255\
\007\001\255\255\255\255\010\001\011\001\255\255\255\255\014\001\
\015\001\016\001\255\255\018\001\019\001\020\001\021\001\022\001\
\007\001\024\001\025\001\010\001\011\001\255\255\255\255\014\001\
\015\001\016\001\255\255\018\001\019\001\020\001\021\001\022\001\
\255\255\024\001\025\001\035\001\036\001\255\255\255\255\255\255\
\255\255\041\001\255\255\007\001\044\001\255\255\010\001\011\001\
\255\255\049\001\014\001\015\001\016\001\053\001\018\001\019\001\
\020\001\021\001\022\001\007\001\024\001\255\255\010\001\011\001\
\255\255\255\255\014\001\015\001\016\001\255\255\018\001\019\001\
\020\001\021\001\022\001\007\001\255\255\255\255\010\001\011\001\
\255\255\255\255\014\001\015\001\255\255\255\255\255\255\019\001\
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
# 386 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 33 "parser.mly"
                 ( List.rev _1 )
# 393 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 36 "parser.mly"
       ( [_1] )
# 400 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 37 "parser.mly"
                            ( _3 :: _1 )
# 408 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 40 "parser.mly"
                 ( Binop (_1, Add, _3) )
# 416 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 41 "parser.mly"
                  ( Binop (_1, Sub, _3) )
# 424 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 42 "parser.mly"
                  ( Binop (_1, Mult, _3) )
# 432 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 43 "parser.mly"
                   ( Binop (_1, Div, _3) )
# 440 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 44 "parser.mly"
                 ( Binop (_1, Mod, _1) )
# 448 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 45 "parser.mly"
                ( Binop (_1, Less, _3) )
# 456 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 46 "parser.mly"
                ( Binop (_1, Greater, _3) )
# 464 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 47 "parser.mly"
                 ( Binop (_1, Leq, _3) )
# 472 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 48 "parser.mly"
                 ( Binop (_1, Geq, _3) )
# 480 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 49 "parser.mly"
                ( Binop (_1, Equal, _3) )
# 488 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 50 "parser.mly"
                 ( Binop (_1, Neq, _3) )
# 496 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 51 "parser.mly"
                ( Binop (_1, Or, _3) )
# 504 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 52 "parser.mly"
                 ( Binop (_1, And, _3) )
# 512 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 53 "parser.mly"
           ( Not(_2) )
# 519 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 54 "parser.mly"
                ( Assign(_1, _3) )
# 527 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 55 "parser.mly"
                              ( Call(_1, _3) )
# 535 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 56 "parser.mly"
                    ( _2 )
# 542 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 57 "parser.mly"
     ( Lit(_1))
# 549 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
                (Noexpr )
# 555 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 62 "parser.mly"
         ( _1 )
# 562 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
     ( [_1] )
# 569 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
                         ( _3 :: _1 )
# 577 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
                ( [] )
# 583 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 70 "parser.mly"
                ( List.rev _1 )
# 590 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
                ( [] )
# 596 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 74 "parser.mly"
                   ( _2 :: _1 )
# 604 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
            ( Expr(_1) )
# 611 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 78 "parser.mly"
                     ( Return(_2) )
# 618 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 79 "parser.mly"
                            ( Block(List.rev _2) )
# 625 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 80 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 633 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 81 "parser.mly"
                                         ( If (_3, _5, _7) )
# 642 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 83 "parser.mly"
   ( For(_3, _5, _7, _9) )
# 652 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 84 "parser.mly"
                                  ( While(_3, _5) )
# 660 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 87 "parser.mly"
              ( _2 )
# 667 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 88 "parser.mly"
                 ( _2 )
# 674 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 89 "parser.mly"
                   ( _2 )
# 681 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 90 "parser.mly"
                   ( _2 )
# 688 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 91 "parser.mly"
                 ( _2 )
# 695 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
                ( [] )
# 701 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 95 "parser.mly"
                     ( _2 :: _1 )
# 709 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 99 "parser.mly"
    ({  fname = _1;
    	formals = _3;
    	locals = List.rev _6;
    	body = List.rev _7; })
# 722 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
               ( [], [] )
# 728 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 106 "parser.mly"
                 ( (_2 :: fst _1), snd _1 )
# 736 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 107 "parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 744 "parser.ml"
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
