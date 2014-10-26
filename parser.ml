type token =
  | LPAREN
  | LBRACE
  | SEMI
  | RPAREN
  | RBRACE
  | COMMA
  | EOF
  | PLUS
  | TIMES
  | MINUS
  | DIVIDE
  | ASSIGN
  | PERIOD
  | EQ
  | NEQ
  | LEQ
  | GEQ
  | LT
  | GT
  | OR
  | AND
  | NOT
  | IF
  | ELSE
  | WHILE
  | FOR
  | RETURN
  | GRAPH
  | NODE
  | BOOL
  | STRING
  | STRUCT
  | PRINT
  | NEW
  | ID
  | NUM of (int)

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 44 "parser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* LBRACE *);
  259 (* SEMI *);
  260 (* RPAREN *);
  261 (* RBRACE *);
  262 (* COMMA *);
    0 (* EOF *);
  263 (* PLUS *);
  264 (* TIMES *);
  265 (* MINUS *);
  266 (* DIVIDE *);
  267 (* ASSIGN *);
  268 (* PERIOD *);
  269 (* EQ *);
  270 (* NEQ *);
  271 (* LEQ *);
  272 (* GEQ *);
  273 (* LT *);
  274 (* GT *);
  275 (* OR *);
  276 (* AND *);
  277 (* NOT *);
  278 (* IF *);
  279 (* ELSE *);
  280 (* WHILE *);
  281 (* FOR *);
  282 (* RETURN *);
  283 (* GRAPH *);
  284 (* NODE *);
  285 (* BOOL *);
  286 (* STRING *);
  287 (* STRUCT *);
  288 (* PRINT *);
  289 (* NEW *);
  290 (* ID *);
    0|]

let yytransl_block = [|
  291 (* NUM *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\001\000\000\000"

let yylen = "\002\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\001\000\000\000\002\000"

let yydefred = "\000\000\
\015\000\000\000\016\000"

let yydgoto = "\002\000\
\003\000\000\000"

let yysindex = "\255\255\
\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000"

let yytablesize = 0
let yytable = "\001\000"

let yycheck = "\001\000"

let yynames_const = "\
  LPAREN\000\
  LBRACE\000\
  SEMI\000\
  RPAREN\000\
  RBRACE\000\
  COMMA\000\
  EOF\000\
  PLUS\000\
  TIMES\000\
  MINUS\000\
  DIVIDE\000\
  ASSIGN\000\
  PERIOD\000\
  EQ\000\
  NEQ\000\
  LEQ\000\
  GEQ\000\
  LT\000\
  GT\000\
  OR\000\
  AND\000\
  NOT\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  FOR\000\
  RETURN\000\
  GRAPH\000\
  NODE\000\
  BOOL\000\
  STRING\000\
  STRUCT\000\
  PRINT\000\
  NEW\000\
  ID\000\
  "

let yynames_block = "\
  NUM\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 29 "parser.mly"
               ( Binop (_1, Add, _3))
# 165 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 30 "parser.mly"
                 ()
# 173 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 31 "parser.mly"
                 ()
# 181 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 32 "parser.mly"
                  ()
# 189 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 33 "parser.mly"
              ()
# 197 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 34 "parser.mly"
              ()
# 205 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 35 "parser.mly"
               ()
# 213 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 36 "parser.mly"
               ()
# 221 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 37 "parser.mly"
              ()
# 229 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 38 "parser.mly"
               ()
# 237 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 39 "parser.mly"
              ()
# 245 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 40 "parser.mly"
               ()
# 253 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 41 "parser.mly"
                  ()
# 261 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 42 "parser.mly"
     ( Lit(_1))
# 268 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
               ( [], [] )
# 274 "parser.ml"
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
