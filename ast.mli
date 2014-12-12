type bop = Add | Sub | Mult | Div | Equal | And | Neq | Mod | Leq | Geq | Greater | Less | Or

type uop = Neg | Not

type validtype = Int | Char | String | Double | Bool | Arr | Node | NodeType | Edge | EdgeType | Graph | UserDef

type expr=
Literal of int
| Noexpr
| Id of string
| String_Lit of string
| Binop of expr * bop * expr
| Unop of uop * expr
| Call of string * expr list
| Array of expr * expr (* Come back to this *)
| Char of string
| Assign of expr * expr
| Construct of validtype * expr list
| MakeArr of validtype * expr
| Access of string * string
| Bool_Lit of bool

type variable = string * validtype

type stmt =
 Block of block
| Expr of expr
| Return of expr
| If of expr * block * block
| For of expr * expr * expr * block
| While of expr * block

and block = {
   locals : variable list;
  statements: stmt list;
  block_num: int;
}

type func_decl = {
	fname : string;
  formals : variable list;
  body_block : block;
  ret : validtype
}


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
