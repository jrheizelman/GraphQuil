{ open Parser }

rule token = parse 
[' ' '\t' 'r' '\n'] { token lexbuf }
| "/*" { comment lexbuf }
| '(' { LPAREN }
| '{' { LBRACE }
| ';' { SEMI }
| '+' { PLUS }
| '*' { TIMES }
| '=' { ASSIGN }
| "!=" { NEQ }
| "<=" { LEQ }
| ">=" { GEQ }
| "else" { ELSE }
| "while" { WHILE }
| "int" { INT }
| eof { EOF }
| ')' { RPAREN }
| '}' { RBRACE }
| ',' { COMMA }
| '-' { MINUS }
| '/' { DIVIDE }
| "==" { EQ }
| '<' { LT }
| '>' { GT }
| "if" { IF }
| "for" { FOR }
| "return" { RETURN }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm)}
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm)}
| _ as char { raise (Failure("illegal character " ^ Char.escaped char))}

and comment = parse
"*/" { token lexbuf }
| _ { comment lexbuf }