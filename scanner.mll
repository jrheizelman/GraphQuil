{ open Parser }

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse 
[' ' '\t' 'r' '\n'] { token lexbuf }
| "/*" { comment lexbuf }
| '(' { LPAREN }
| '{' { LBRACE }
| '[' { LBRACK }
| ';' { SEMI }
| ':' { COLON }
| ')' { RPAREN }
| '}' { RBRACE }
| ']' { RBRACK }
| '%' { MOD }
| ',' { COMMA }
| '.' { PERIOD }
| eof { EOF }
| '+' { PLUS }
| '*' { TIMES }
| "->" { LINK }
| "<->" { BILINK }
| '-' { MINUS }
| '/' { DIVIDE }
| "==" { EQ }
| '=' { ASSIGN }
| '.' {PERIOD}
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
| "Graph" { GRAPH }
| "NodeType" { NODETYPE }
| "EdgeType" { EDGETYPE }
| "Node" { NODE }
| "bool" { BOOL }
| "String" { STRING }
| "print" { PRINT }
| "new" { NEW }
| "continue" { CONTINUE }
| "double" { DOUBLE }
| "false" { FALSE }
| "true" { TRUE }
| "int" { INT }
| "void" { VOID }
| "dest" { DEST }
| "edges" { EDGES }
| "static" { STATIC }
| "char" { CHAR }
| "do" { DO }
| "in" { IN }
| '"' [^'"']* '"'  as lxm { STRINGLIT(lxm) }
| ''' [ ^'''] as ch ''' { CHARLIT(ch) }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm)}
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm)}
| _ as char { raise (Failure("illegal character " ^ Char.escaped char))}

and comment = parse
"*/" { token lexbuf }
| _ { comment lexbuf }
