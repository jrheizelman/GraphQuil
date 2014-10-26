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

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
