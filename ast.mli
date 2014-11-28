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

type validtype = Void | Int | Char | String | Double | Node 
	| Graph | EdgeType | NodeType | Bool | Userdef | Edge | Arr

type expr=
Literal of int
| Noexpr
| Id of string
| Binop of expr * op * expr
| Call of string * expr list
| Array of expr * expr
| Lit of int
| Not of expr
| String of string
| Char of string
| Assign of expr * expr
| Construct of validtype * expr list
| MakeArr of validtype * expr
| Access of string * string

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
	ret : validtype list
}

type var_decl = {
	vname: string;
 	vtype: validtype list;
}

type program = var_decl list * func_decl list 
