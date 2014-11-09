type token =
  | LPAREN
  | LBRACE
  | SEMI
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
  | CHR
  | DO
  | IN
  | NUM of (int)
  | ID of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
