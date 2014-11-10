type op = 
Add 
| Sub 
| Mult
| Div 
| Assign 
| Less 
| Greater  
| Leq 
| Geq 
| Neq 
| And 
| Or 
| Mod
| Equal

type expr=
Literal of int
| Noexpr
| Id of string
| Assign of string * expr
| Binop of expr * op * expr
| Call of string * expr list
| Lit of int
| Not of expr

type stmt =
Block of stmt list
| Expr of expr
| Return of expr
| If of expr * stmt * stmt
| For of expr * expr * expr * stmt
| While of expr * stmt

type func_decl = {
	fname : string;
	formals : string list;
	locals : string list;
	body : stmt list;
}

type program = string list * func_decl list 