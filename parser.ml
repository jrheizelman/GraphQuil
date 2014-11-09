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
\004\000\004\000\005\000\005\000\006\000\006\000\007\000\007\000\
\008\000\008\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\010\000\011\000\011\000\012\000\001\000\001\000\000\000"

let yylen = "\002\000\
\000\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\001\000\000\000\001\000\001\000\003\000\000\000\001\000\
\000\000\002\000\002\000\003\000\003\000\005\000\007\000\009\000\
\005\000\003\000\000\000\002\000\008\000\000\000\002\000\002\000"

let yydefred = "\000\000\
\038\000\000\000\000\000\000\000\039\000\000\000\021\000\000\000\
\000\000\000\000\000\000\022\000\035\000\000\000\000\000\000\000\
\036\000\000\000\025\000\037\000\000\000\000\000\000\000\000\000\
\018\000\000\000\026\000\034\000\000\000\000\000\000\000\000\000\
\000\000\027\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\029\000\
\000\000\000\000\000\000\000\000\028\000\000\000\006\000\000\000\
\008\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\033\000\000\000\
\000\000\000\000\031\000\000\000\000\000\032\000"

let yydgoto = "\002\000\
\003\000\000\000\000\000\026\000\052\000\008\000\009\000\016\000\
\027\000\017\000\014\000\005\000"

let yysindex = "\002\000\
\000\000\000\000\205\254\009\255\000\000\236\254\000\000\035\255\
\031\255\250\254\046\255\000\000\000\000\005\255\007\255\255\254\
\000\000\049\255\000\000\000\000\064\255\074\255\075\255\032\255\
\000\000\083\255\000\000\000\000\028\255\032\255\032\255\032\255\
\103\255\000\000\032\255\032\255\032\255\032\255\032\255\032\255\
\032\255\032\255\032\255\032\255\032\255\032\255\032\255\000\000\
\001\000\019\000\035\000\076\255\000\000\027\255\000\000\027\255\
\000\000\079\000\035\000\079\000\067\255\067\255\067\255\067\255\
\066\000\051\000\044\255\044\255\032\255\060\255\000\000\085\255\
\044\255\032\255\000\000\084\255\044\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\092\000\000\000\000\000\090\255\000\000\104\255\
\000\000\000\000\000\000\000\000\000\000\033\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\107\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\021\255\000\000\000\000\126\255\000\000\149\255\
\000\000\225\255\050\255\241\255\159\255\182\255\192\255\215\255\
\132\255\066\255\000\000\000\000\107\255\038\255\000\000\000\000\
\000\000\106\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\232\255\191\255\000\000\000\000\093\000\
\220\255\000\000\000\000\000\000"

let yytablesize = 357
let yytable = "\033\000\
\019\000\004\000\001\000\072\000\020\000\049\000\050\000\051\000\
\076\000\006\000\054\000\055\000\056\000\057\000\058\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\020\000\
\021\000\020\000\022\000\023\000\024\000\019\000\070\000\071\000\
\007\000\048\000\025\000\011\000\075\000\036\000\025\000\030\000\
\078\000\038\000\010\000\030\000\051\000\019\000\012\000\013\000\
\015\000\051\000\025\000\028\000\017\000\021\000\017\000\022\000\
\023\000\024\000\025\000\018\000\025\000\025\000\025\000\030\000\
\030\000\030\000\030\000\030\000\015\000\021\000\015\000\022\000\
\023\000\024\000\031\000\032\000\035\000\036\000\069\000\025\000\
\037\000\038\000\015\000\025\000\025\000\034\000\073\000\074\000\
\077\000\030\000\015\000\040\000\035\000\036\000\023\000\025\000\
\037\000\038\000\039\000\040\000\041\000\042\000\043\000\044\000\
\045\000\053\000\046\000\047\000\024\000\019\000\019\000\029\000\
\035\000\036\000\000\000\000\000\037\000\038\000\039\000\040\000\
\041\000\042\000\043\000\044\000\045\000\000\000\046\000\047\000\
\005\000\000\000\005\000\000\000\000\000\000\000\016\000\005\000\
\016\000\000\000\000\000\005\000\000\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\016\000\005\000\005\000\007\000\
\000\000\007\000\000\000\016\000\016\000\000\000\007\000\000\000\
\000\000\011\000\007\000\011\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\000\000\007\000\007\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\000\000\011\000\011\000\
\012\000\000\000\012\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\009\000\000\000\009\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\000\000\012\000\012\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\000\000\009\000\
\009\000\010\000\000\000\010\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\013\000\000\000\013\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\000\000\010\000\010\000\
\013\000\013\000\013\000\014\000\000\000\014\000\000\000\000\000\
\013\000\013\000\000\000\000\000\000\000\000\000\000\000\000\000\
\014\000\014\000\014\000\000\000\000\000\067\000\000\000\000\000\
\014\000\014\000\035\000\036\000\000\000\000\000\037\000\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\068\000\
\046\000\047\000\000\000\000\000\035\000\036\000\000\000\000\000\
\037\000\038\000\039\000\040\000\041\000\042\000\043\000\044\000\
\045\000\000\000\046\000\047\000\035\000\036\000\000\000\000\000\
\037\000\038\000\039\000\040\000\041\000\042\000\043\000\044\000\
\045\000\000\000\046\000\047\000\035\000\036\000\000\000\000\000\
\037\000\038\000\039\000\000\000\041\000\042\000\043\000\044\000\
\045\000\000\000\046\000\035\000\036\000\000\000\000\000\037\000\
\038\000\039\000\000\000\041\000\042\000\043\000\044\000\045\000\
\035\000\036\000\000\000\000\000\037\000\038\000\000\000\000\000\
\000\000\042\000\043\000\044\000\045\000"

let yycheck = "\024\000\
\002\001\053\001\001\000\069\000\006\001\030\000\031\000\032\000\
\074\000\001\001\035\000\036\000\037\000\038\000\039\000\040\000\
\041\000\042\000\043\000\044\000\045\000\046\000\047\000\003\001\
\026\001\005\001\028\001\029\001\030\001\002\001\067\000\068\000\
\053\001\006\001\002\001\005\001\073\000\011\001\006\001\002\001\
\077\000\015\001\008\001\006\001\069\000\002\001\053\001\002\001\
\044\001\074\000\052\001\003\001\003\001\026\001\005\001\028\001\
\029\001\030\001\026\001\053\001\028\001\029\001\030\001\026\001\
\001\001\028\001\029\001\030\001\003\001\026\001\005\001\028\001\
\029\001\030\001\001\001\001\001\010\001\011\001\003\001\052\001\
\014\001\015\001\017\001\052\001\052\001\003\001\027\001\003\001\
\005\001\052\001\025\001\000\000\010\001\011\001\005\001\052\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\003\001\024\001\025\001\005\001\003\001\005\001\019\000\
\010\001\011\001\255\255\255\255\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\255\255\024\001\025\001\
\003\001\255\255\005\001\255\255\255\255\255\255\003\001\010\001\
\005\001\255\255\255\255\014\001\255\255\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\017\001\024\001\025\001\003\001\
\255\255\005\001\255\255\024\001\025\001\255\255\010\001\255\255\
\255\255\003\001\014\001\005\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\255\255\024\001\025\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\255\255\024\001\025\001\
\003\001\255\255\005\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\003\001\255\255\005\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\255\255\024\001\025\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\025\001\003\001\255\255\005\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\255\255\005\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\255\255\024\001\025\001\
\016\001\017\001\018\001\003\001\255\255\005\001\255\255\255\255\
\024\001\025\001\255\255\255\255\255\255\255\255\255\255\255\255\
\016\001\017\001\018\001\255\255\255\255\005\001\255\255\255\255\
\024\001\025\001\010\001\011\001\255\255\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\005\001\
\024\001\025\001\255\255\255\255\010\001\011\001\255\255\255\255\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\255\255\024\001\025\001\010\001\011\001\255\255\255\255\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\255\255\024\001\025\001\010\001\011\001\255\255\255\255\
\014\001\015\001\016\001\255\255\018\001\019\001\020\001\021\001\
\022\001\255\255\024\001\010\001\011\001\255\255\255\255\014\001\
\015\001\016\001\255\255\018\001\019\001\020\001\021\001\022\001\
\010\001\011\001\255\255\255\255\014\001\015\001\255\255\255\255\
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
# 342 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 33 "parser.mly"
                 ( List.rev _1 )
# 349 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 36 "parser.mly"
       ( [_1] )
# 356 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 37 "parser.mly"
                            ( _3 :: _1 )
# 364 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 40 "parser.mly"
               ( Binop (_1, Add, _3))
# 372 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 41 "parser.mly"
                 ()
# 380 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 42 "parser.mly"
                 ()
# 388 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 43 "parser.mly"
                  ()
# 396 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 44 "parser.mly"
              ()
# 404 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 45 "parser.mly"
              ()
# 412 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 46 "parser.mly"
               ()
# 420 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 47 "parser.mly"
               ()
# 428 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 48 "parser.mly"
              ()
# 436 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 49 "parser.mly"
               ()
# 444 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 50 "parser.mly"
              ()
# 452 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 51 "parser.mly"
               ()
# 460 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 52 "parser.mly"
                  ()
# 468 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 53 "parser.mly"
     ( Lit(_1))
# 475 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
                (Noexpr )
# 481 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 58 "parser.mly"
         ( _1 )
# 488 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "parser.mly"
     ( [_1] )
# 495 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 62 "parser.mly"
                         ( _3 :: _1 )
# 503 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
                ( [] )
# 509 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 66 "parser.mly"
                ( List.rev _1 )
# 516 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
                ( [] )
# 522 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 70 "parser.mly"
                   ( _2 :: _1 )
# 530 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
            ( Expr(_1) )
# 537 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
                     ( Return(_2) )
# 544 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 75 "parser.mly"
                            ( Block(List.rev _2) )
# 551 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 76 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 559 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 77 "parser.mly"
                                         ( If (_3, _5, _7) )
# 568 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 79 "parser.mly"
   ( For(_3, _5, _7, _9) )
# 578 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 80 "parser.mly"
                                  ( While(_3, _5) )
# 586 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 83 "parser.mly"
              ( _2 )
# 593 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                ( [] )
# 599 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 87 "parser.mly"
                     ( _2 :: _1 )
# 607 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 91 "parser.mly"
    ({  fname = _1;
    	formals = _3;
    	locals = List.rev _6;
    	body = List.rev _7; })
# 620 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
               ( [], [] )
# 626 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : unit) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 99 "parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 634 "parser.ml"
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
