{ open Parser }

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse 
[' ' '\t' 'r' '\n'] { token lexbuf }
| "/*" { comment lexbuf }
| '(' { LPAREN }
| '{' { LBRACE }
| ';' { SEMI }
| ')' { RPAREN }
| '}' { RBRACE }
| ',' { COMMA }
| eof { EOF }
| '+' { PLUS }
| '*' { TIMES }
| '-' { MINUS }
| '/' { DIVIDE }
| '=' { ASSIGN }
| '.' {PERIOD}
| "==" { EQ }
| "!=" { NEQ }
| "<=" { LEQ }
| ">=" { GEQ }
| '<' { LT }
| '>' { GT }
| '!' { NOT }
| "&&" {AND}
| "||" {OR}
| "if" { IF }
| "else" { ELSE }
| "while" { WHILE }
| "for" { FOR }
| "return" { RETURN }
| "graph" { GRAPH }
| "node" { NODE }
| "bool" { BOOL }
| "string" { STRING }
| "type" { STRUCT }
| "print" { PRINT }
| "new" { NEW }
| ['0'-'9']+ as lxm { NUM(int_of_string lxm)}
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm)}
| _ as char { raise (Failure("illegal character " ^ Char.escaped char))}

and comment = parse
"*/" { token lexbuf }
| _ { comment lexbuf }