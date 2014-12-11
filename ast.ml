type bop = Add | Sub | Mult | Div | Equal | And | Neq | Mod | Leq | Geq | Greater | Less | Or

type uop = Neg | Not

type validtype = Int | Char | String | Double | Bool | Arr | Node | NodeType | Edge | EdgeType | Graph | UserDef

type expr=
Literal of int
| Noexpr
| Id of string
| Binop of expr * bop * expr
| Unop of uop * expr
| Call of string * expr list
| Array of expr * expr (* Come back to this *)
| String_Lit of string
| Char of string
| Assign of expr * expr
| Construct of validtype * expr list
| MakeArr of validtype * expr
| Access of string * string
| Bool_lit of bool

type stmt =
Block of block
| Expr of expr
| Return of expr
| If of expr * block * block
| For of expr * expr * expr * block
| While of expr * block

type func_decl = {
	fname : string;
  formals : variable list;
  body : block;
  ret : validtype
}

type block = {
  locals : variable list;
  statements: stmt list;
  block_num: int;
}

type variable = validtype * string

type program = variable list * func_decl list 

(*
To be used to log the variable in the symbol table. 
string = name
validtype = type
int = block number (scope)
*)
type symbol_table_var = string * validtype * int

(*
To be used to log the variable in the symbol table. 
string = name
validtype = return type
validtype list = formals list
int = block number (scope)
*)
type symbol_table_func = string * validtype * validtype list * int

(*
General declaration of either variable or function to be written in symbol table
*)
type declaration = 
    SymbTable_Var of symbol_table_var
  | SymbTable_Func of symbol_table_func 

(*************************
**** PRINT AST **********
*************************)

let string_of_bop = function 
     Add -> "+" 
  | Sub -> "-" 
  | Mult -> "*" 
  | Div -> "/" 
  | Mod -> "mod"
  | Child -> "%"
  | Equal -> "==" 
  | Neq -> "!="
  | Less -> "<" 
  | Leq -> "<=" 
  | Greater -> ">" 
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_unop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    Literal(n) -> string_of_int n
  | Char(n) -> "\'" ^ (String.make 1) n ^"\'"
  | Id(s) -> s
  | String_Lit(s) -> "\"" ^ s ^ "\""
  | Bool_Lit(l) -> string_of_bool l
  | Binop(e1, op, e2) ->
      string_of_expr e1 ^ " " ^ 
      string_of_bop op  ^ " " ^
      string_of_expr e2
  | Unop(op, e) -> 
      string_of_unop op ^ " " ^ 
      string_of_expr e
  | Assign(v, e) -> 
      string_of_expr v  ^ " = " ^
      string_of_expr e
  | Call(f, argl) ->
    f ^ "(" ^ String.concat ", " (List.map string_of_expr argl) ^ ")" 
 
 let string_of_valid_type = function
    Int -> "int"
  | Char -> "char"
  | String -> "String"
  | Double -> "double"
  | Bool -> "bool"
  | Arr -> "array"
  | Node -> "Node"
  | NodeType -> "NodeType"
  | Edge -> "Edge"
  | EdgeType -> "EdgeType"
  | Graph -> "Graph"
  | Userdef -> "UserDef"

  let string_of_variable v = string_of_valid_type  (fst v) ^ " " ^
                             snd v

  let string_of_stmt = function
    Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e1, b1, b2) -> 
      (match b2.statements with
        [] -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_block b1
      | _  -> "if (" ^ string_of_expr e ^ ")\n" ^
              string_of_block b1 ^ "else\n" ^ string_of_block b2)
  | For(a1, e, a2, b) ->
      "for (" ^ string_of_expr a1 ^ "; " ^ 
                string_of_expr e ^ "; " ^
                string_of_expr a2 ^ ") {\n" ^ 
                string_of_block b ^ "}"
  | While(e, b) -> 
      "while (" ^ string_of_expr e ^ ") {\n" ^ 
      string_of_block b ^ "}"

  and string_of_block (b:block) = "{\n" ^
    String.concat ";\n" (List.map string_of_variable b.locals) ^ (if (List.length b.locals) > 0 then ";\n" else "") ^
    String.concat "" (List.map string_of_stmt b.statements) ^
    "}\n"

  let string_of_func fdecl = (string_of_valid_type fdecl.ret) ^ " " ^
    fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_variable fdecl.formals) ^ ")\n" ^
    (string_of_block func_decl.block)

  let string_of_prog prog = String.concat ";\n" (List.map string_of_variable (fst prog)) ^
    (if (List.length (fst prog)) > 0 then ";\n" else "") ^
    String.concat "\n" (List.map string_of_func (snd prog))
