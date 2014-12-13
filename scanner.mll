(*
Authors: Jon Paul
		 Gemma Ragozzine
		 John Heizelman
		 Steven Weiner
*)

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
| "Edge" { EDGE }
| "bool" { BOOL }
| "String" { STRING }
| "new" { NEW }
| "continue" { CONTINUE }
| "double" { DOUBLE }
| "int" { INT }
| "void" { VOID }
| "dest" { DEST }
| "edges" { EDGES }
| "static" { STATIC }
| "char" { CHAR }
| "in" { IN }
| '"' [^'"']* '"'  as lxm { STRINGLIT(lxm) }
| ''' [ ^'''] as ch ''' { CHARLIT(ch) }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm)}
| ['a'-'z' 'A' - 'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*"[]" as lxm { ARRID(lxm)}
| ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm)}
| ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { TYPEID(lxm)}
| ("true" | "false") as lxm	{ BOOLLIT(bool_of_string lxm) }
| eof { EOF }
| _ as char { raise(Failure("illegal character " ^ Char.escaped char))}

and comment = parse
"*/" { token lexbuf }
| _ { comment lexbuf }
