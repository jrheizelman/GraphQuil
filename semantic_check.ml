open ast
open sast

let fst_of_three (t, _, _) = t
let snd_of_three (_, t, _) = t
let fst_of_four (t, _, _, _) = t

(* Structure the main function *)
let main_fdecl (f:function_t) =
	if f.fname_t = "main" && 
		f.ret_t = Int &&
		f.formals_t = [] 
		then true
	else false

(* called to get the type of an expression *)
let type_of_expr = function
	  Literal_t(i) -> Int
	| Noexpr_t -> raise (Failure("Type of expression called on noexpr type."))
	| Id_t(t, _, _) -> t
	| Binop_t(t, _, _, _) -> t
	| Unop_t(t, _, _) -> t
	| Call_t(f, _) -> let (_,r,_,_) = f in r
	| Array_t(t, _, _) -> t
	| String_Lit_t(s) -> s
	| Char_t(c) -> c
	| Assign_t(t, _, _) -> t
	| Construct_t(t, _) -> t
	| MakeArr_t(t, _) -> t
	| Access_t(t, _, _) -> t
	| Bool_Lit_t(b) -> b

(* Error raised for improper binary operation *)
let binop_err (t1:validtype) (t2:validtype) (op:bop) =
		raise(Failure("Operator " ^ (string_op_bop op) ^ 
			" not compatible with expressions of type " ^ 
			string_of_valid_type t1 ^ " and  " ^ 
			string_of_valid_type t2 ^ "."))

(* Check binary operation *)
let check_binop (e1:expr_t) (e2:expr_t) (op:bop) =
	(* TODO check expressions type for null *)
	let (t1, t2) = (type_of_expr e1, type_of_expr e2) in
		(* Both are ints *)
		match (t1, t2) with (Int, Int) -> 
			(match op with
		  	(Add | Sub | | Mult | Div | Mod) -> Binop_t(Int, e1, op, e2)
		  	| (Equal | Neq | Less | Leq | Greater | Geq) -> Binop_t(Bool, e1, op, e2) 
		  	| _ -> binop_err t1 t2 op)
		(* Both are bools *)
		| (Bool, Bool) -> 
			(match op with (And | Or | Equal | Neq) ->
				Binop_t(Bool, e1, op, e2)
				| _ -> binop_err t1 t2 op)
		(* Both are chars *)
		| (Char, Char) ->
			(match op with (Add | Sub) ->
				Binop_t(Char, e1, op, e2)
				| _ -> binop_err t1 t2 op)
		| _ -> binop_err t1 t2 op

let unop_err (t:validtype) (op:uop) = 
	raise(Failure("Operator " ^ (string_of_unop op) ^
		" not compatible with expression of type " ^
		(string_of_valid_type t) ^ "."))

let check_unop (e:expr_t) (op:uop) =
	let t = type_of_expr e in
		match t with 
		(* Expression is an int *)
		   Int ->
		   	(match op with
			 	  Neg -> Unop_t(Int, e, op)
			 	| _ -> unop_err t op)
		(* Expression is a bool *)
		 | Bool ->
		 	(match op with
			 	  Not -> Unop_t(Bool, e, op)
			 	  | _ -> unop_err t op)
		 | _ -> unop_err t op

(* compare arg lists with func formal params *)
let rec compare_args formals actuals =
	match (formals, actuals) with 
		([], []) -> true
		| (head1::tail1, head2::tail2) ->
			(match (head1, head2) with _ ->
			(head1 = head2) && compare_args tail1 tail2)
		| _ -> false

 and check_func_call (name:string) (elist: expr_t list) env =
 	let decl = symbolTable.symbol_table_find name env in
 		let fdecl = 
 			(match decl with
 				(* make sure it is a function *)
 				SymbTable_Func(f) -> f
 				| _ -> raise(Failure("Variable " ^ name ^ " is not a function."))) in
 			let (fname, ret_type, formals, id) = fdecl in
 				let actuals = List.map type_of_expr eList in
 					match name with
 						"print" -> Call_t((fname, ret_type, actuals, id), eList)
 						| _ -> (* Check num of arguments *)
 							if(List.length formals) = (List.length actuals) then
 								(* Check type of args *)
 								if (compare_args formals actuals) then Call_t(fdecl, eList)
 								else raise(Failure("Function " ^ name ^
 								"'s argument types do not match its formals"))
 							else raise(Failure("Function " ^ name ^ " expected " ^
 							(string_of_int (List.length formals)) ^
 							" arguments but was called with " ^ 
 							(string_of_int (List.length actuals))))

let rec check_valid_id (id_name:string) env = 
	(* Check if id is in the symbol table *)
	let decl = symbolTable.symbol_table_find id_name env in
		(* Check if id is in the correct scope *)
		let id = symbolTable.symbol_table_get_id id_name env in
			(match decl with
				(* SymbTable_Var = name * type * id *)
				SymbTable_Var(v) -> (snd_of_three v, fst_of_three v, id)
				| _ -> raise(Failure("Symbol " ^ id_name ^ " is not a variable.")))
 
 (* Unclear if we need this *)
(* let get_left_value_of_expr (e:expr_t) env = 
 	match e with 
 		Id_t(t, s, _) -> s
 		| Binop_t(t, l, o, r) -> get_left_value_of_expr l
 		| Unop_t(t, l, o) -> get_left_value_of_expr l
 		| _ -> raise(Failure("Cannot get the left value of expression."))

and check_left_value (e:expr) env = 
	match e with
		(* If left expression is an id *)
		Id(s) -> let (t, e, id) = check_valid_id s env in Id_t(t, e, id)
		(* If left expression is anything else *)
		| _ -> let checked_expr = (check_expr e env) in 
			match checked_expr with
				Binop_t(_, _, op, _) -> *)













