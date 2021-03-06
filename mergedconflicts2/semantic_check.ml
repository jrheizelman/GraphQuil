(*
	Authors: Gemma Ragozzine
			 John Heizelman
*)

open Ast
open Sast
open SymbolTable


(* Structure the main function *)
let main_fdecl (f:function_t) = 
	if f.fname_t = "main" then
		if f.ret_t = Void then
			if f.formals_t = [] then true
			else raise(Failure("Main method argument list must be empty"))
		else raise(Failure("Main method must return void"))
	else false

(* called to get the type of an expression *)
let type_of_expr = function
	  Literal_t(i) -> Int
	| Noexpr_t -> raise (Failure("Type of expression called on noexpr type."))
	| Id_t(t, _, _) -> t
	| Binop_t(t, _, _, _) -> t
	| Unop_t(t, _, _) -> t
	| Call_t(f, _) -> let (_,r,_,_) = f in r
	| String_Lit_t(s) -> String
	| Char_t(c) -> Char
	| Assign_t(t, _, _) -> t
	| Bool_Lit_t(b) -> Bool
	| Add_at_t(_, _) -> Node
	| Assign_at_t(t, _, _) -> t
	| Access_t(t, _, _) -> t

let type_of_attribute = function
	Char_rat(_, _) -> Char
	| String_rat(_,_) -> String_at
	| Int_rat(_, _) -> Int_at
	| Bool_rat(_, _) -> Bool_at

let access_type_of_attr = function
	Char_rat(_, _) -> Char
	| String_rat(_,_) -> String
	| Int_rat(_, _) -> Int
	| Bool_rat(_, _) -> Bool

(* Error raised for improper binary operation *)
let binop_err (t1:validtype) (t2:validtype) (op:bop) =
		raise(Failure("Operator " ^ (string_of_bop op) ^ 
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
		  	(Add | Sub | Mult | Div | Mod) -> Binop_t(Int, e1, op, e2)
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
			 	  Neg -> Unop_t(Int, op, e)
			 	| _ -> unop_err t op)
		(* Expression is a bool *)
		 | Bool ->
		 	(match op with
			 	  Not -> Unop_t(Bool, op, e)
			 	  | _ -> unop_err t op)
		 | _ -> unop_err t op

let assign_err (t1:validtype) (t2:validtype) = 
	raise(Failure("Cannot assign expression of type " ^
		string_of_valid_type t2 ^ " to expression of type " ^
		string_of_valid_type t1 ^ "."))

let add_err (t1:validtype) (t2:validtype) = 
	raise(Failure("Add must be of attribute type " ^ 
		  	"to Node or Edge type, " ^ string_of_valid_type t1 ^ " to " ^ 
		  	string_of_valid_type t2 ^ " was given."))

let check_assign (l:expr_t) (r:expr_t) = 
	let (l_t, r_t) = (type_of_expr l, type_of_expr r) in
		  if(l_t = r_t) then Assign_t(l_t, l, r)
		  else assign_err l_t r_t 

(* compare arg lists with func formal params *)
let rec compare_args formals actuals =
	match (formals, actuals) with 
		([], []) -> true
		| (head1::tail1, head2::tail2) ->
			(match (head1, head2) with _ ->
			(head1 = head2) && compare_args tail1 tail2)
		| _ -> false

 and check_func_call (name:string) (eList: expr_t list) env =
 	let decl = symbol_table_find name env in
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
	let decl = symbol_table_find id_name env in
		(* Check if id is in the correct scope *)
		let id = symbol_table_get_id id_name env in
			(match decl with
				(* SymbTable_Var = name * type * id *)
				SymbTable_Var(v) -> (snd_of_three v, fst_of_three v, id)
				| _ -> raise(Failure("Symbol " ^ id_name ^ " is not a variable.")))
 
 (* Unclear if we need this 
 	This came in so much handy for me (JH), thanks*)
let rec get_left_value_of_expr (e:expr_t) = 
 	match e with 
 		Id_t(t, s, _) -> s
 		| Binop_t(t, l, o, r) -> get_left_value_of_expr l
 		| Unop_t(t, o, l) -> get_left_value_of_expr l
 		| _ -> raise(Failure("Cannot get the left value of expression."))

and check_left_value (e:expr) env = 
	match e with
		(* If left expression is an id *)
		Id(s) -> let (t, e, id) = check_valid_id s env in Id_t(t, e, id)
		(* If left expression is anything else *)
		| _ -> raise(Failure("Left hand side of assignment operator is improper type"))

and check_attribute (a:attribute) env = 
	match a with
	Char_rat(t, v) -> Char_rat(t, v)
	| String_rat(t, v) -> String_rat(t, v)
	| Int_rat(t, v) -> Int_rat(t, v)
	| Bool_rat(t, v) -> Bool_rat(t,v)

(* Did not check Array, construct, makeArr, Access*)
and check_expr (e:expr) env tagtab = 
	match e with 
	 Literal(i) -> Literal_t(i) 
	 | Noexpr -> Noexpr_t
	 | Id(s) -> let (t, st, id) = check_valid_id s env in Id_t(t, st, id)
	 | Binop(e1, op, e2) -> 
	 	let(ce1, ce2) = (check_expr e1 env tagtab, check_expr e2 env tagtab) in
			check_binop ce1 ce2 op
	 | Unop(op, e) -> 
	 	let ce = check_expr e env tagtab in
	 		check_unop ce op
	 | Call(n, eList) -> 
	 	let checkedList = check_exprList eList env tagtab in
	 		check_func_call n checkedList env
	 | String_Lit(s) -> String_Lit_t(s)
	 | Char_e(c) -> Char_t(c)
	 | Assign(l, r) -> 
	 	let checked_r = check_expr r env tagtab in
	 		let checked_l = check_left_value l env in
	 			check_assign checked_l checked_r
	 | Bool_Lit(b) -> Bool_Lit_t(b)
	 | Assign_at(l, r) ->
	 	let checked_l = check_left_value l env in 
	 		let checked_r = check_attribute r env in
	 			check_assign_attribute checked_l checked_r tagtab
	 | Add_at(l, r) -> 
	 	let checked_l = check_expr l env tagtab in
	 		let checked_r = check_expr r env tagtab in 
	 			check_add checked_l checked_r tagtab
	 | Access(e, t) ->
	 	let checked_e = check_expr e env tagtab in
	 		check_access checked_e t tagtab

and check_assign_attribute (l:expr_t) (r:attribute) tagtab = 
	let(l_t, r_t) = (type_of_expr l, type_of_attribute r) in
		if(l_t = r_t) then 
			let at_name = get_left_value_of_expr l in 
				ignore(tag_table_assign_at at_name (at_name, r) tagtab);
				Assign_at_t(l_t, l, r)
		else assign_err l_t r_t

and check_add (l:expr_t) (r:expr_t) tagtab = 
	let (l_t, r_t) = (type_of_expr l, type_of_expr r) in
		if(l_t = Node || l_t = Edge) then 
		  	let (l_name, r_name) = (get_left_value_of_expr l, get_left_value_of_expr r) in 
		  		ignore(tag_table_add l_name r_name tagtab);
		  		match r_t with
		  		String_at -> Add_at_t(l, r)
		  		| Int_at -> Add_at_t(l, r)
		  		| Char_at -> Add_at_t(l, r)
		  		| Bool_at -> Add_at_t(l, r)
		  		| _ -> add_err l_t r_t
		  		else add_err l_t r_t

and check_access (e:expr_t) (s:string) tagtab = 
	let e_t = type_of_expr e in
		if(e_t = Node || e_t = Edge) then
			let name = get_left_value_of_expr e in
				let (n, at) = tag_table_find name s tagtab in
					Access_t(access_type_of_attr at, e, s)
		else raise(Failure("Access must be on type Node or " ^ 
			"Edge, " ^ string_of_valid_type e_t ^ " given."))

and check_exprList (eList: expr list) env tagtab= 
	match eList with
		  [] -> []
		| head::tail -> (check_expr head env tagtab) :: (check_exprList tail env tagtab)

let rec check_statement (s:stmt) ret_type env tagtab (scope:int) =
	match s with 
		  Block(b) -> 
			let checked_block = check_block b ret_type env tagtab scope in
				Block_t(checked_block)
		| Expr(e) -> Expr_t(check_expr e env tagtab)
		| Return(e) -> 
			let checked_e = check_expr e env tagtab in
				let type_e = type_of_expr checked_e in
					if (type_e = ret_type) then Return_t(checked_e)
				else raise(Failure("Function tries to return type " ^
					(string_of_valid_type type_e) ^ " but should return type " ^
					(string_of_valid_type ret_type) ^ "."))
		| If(e, b1, b2) -> 
			let ce = check_expr e env tagtab in
				let te = type_of_expr ce in
					(match te with
						  Bool -> If_t(ce, check_block b1 ret_type env tagtab scope, check_block b2 ret_type env tagtab scope)
						  | _ -> raise(Failure("If statement must evaluate on a boolean expression.")))
					
		| For(e1, e2, e3, b) ->
			let(c1, c2, c3) = (check_expr e1 env tagtab, check_expr e2 env tagtab, check_expr e3 env tagtab) in 
				if (type_of_expr c2 = Bool) then
					(* Increment scope, check block to be valid block *)
					For_t(c1, c2, c3, check_block b ret_type env tagtab (scope + 1))
				else raise(Failure("For loop condition must evaluate to a boolean expression"))
		| While(e, b) ->
			let ce = check_expr e env tagtab in
				if (type_of_expr ce = Bool)
					then While_t(ce, check_block b ret_type env tagtab (scope + 1))
				else raise(Failure("While loop must evaluate on a boolean expression"))
		(*| Vdecl(t, id) ->
			symbol_table_add_var_list *)
		(*| Link(l, r) -> 
			let checked_l = check_expr l env in 
				let type_l = type_of_expr checked_l in
					let checked_r = check_expr r env in 
						let type_r = type_of_expr checked_r in
							if(type_l = type_r) then Link_t(checked_l, checked_r)
						else raise(Failure("Function tries to link type " ^
							(string_of_valid_type type_l) ^ " with type " ^
							(string_of_valid_type type_r) ^ "."))*)

and check_block (b:block) (ret_type:validtype) env tagtab (scope:int) = 
	let variables = check_is_vdecl_list b.locals (fst env, b.block_num) in
		let stmts = check_stmt_list b.statements ret_type (fst env, b.block_num) tagtab scope in
			{locals_t = variables; statements_t = stmts; block_num_t = b.block_num}


and check_stmt_list (s:stmt list) (ret_type:validtype) env tagtab (scope:int) =
	match s with
		  [] -> []
		| head::tail -> 
			check_statement head ret_type env tagtab scope :: check_stmt_list tail ret_type env tagtab scope

and check_is_vdecl_list (vars:variable list) env = 
	match vars with
		  [] -> []
		| head :: tail -> 
			let decl = symbol_table_find (fst head) env in
				let id = symbol_table_get_id (fst head) env in
					match decl with
						  SymbTable_Func(f) -> raise(Failure("Symbol "^ (fst head) ^
							 "is a function, not a variable "))
						| SymbTable_Var(v) ->
							let varType = snd_of_three v in
								match varType with 
									validtype -> (fst_of_three v, snd_of_three v, id) :: check_is_vdecl_list tail env


let rec check_is_fdecl (func:string) env =
	let fdecl = symbol_table_find func env in
		match fdecl with
			  SymbTable_Var(v) -> raise(Failure("Symbol is a variable, not a function."))
			| SymbTable_Func(f) -> f

and check_function (f:func_decl) env tagtab = 
	let checked_block = check_block f.body_block f.ret env tagtab 0 in
		let checked_formals = check_is_vdecl_list f.formals (fst env, f.body_block.block_num) in
			let checked_scope = check_is_fdecl f.fname env in
				{fname_t = fst_of_four checked_scope; ret_t = f.ret; formals_t = checked_formals; body_block_t = checked_block }

and check_function_list (funcs:func_decl list) env tagtab = 
	match funcs with 
		  [] -> []
		| head::tail -> check_function head env tagtab :: check_function_list tail env tagtab

and check_main_exists (f:function_t list) = 
	if (List.filter main_fdecl f) = [] then false else true

and check_program (p:program) env tagtab = 
	let vs = fst p in
		let fs = snd p in
			let checked_vs = check_is_vdecl_list vs env in
				let checked_fs = check_function_list fs env tagtab in
					if(check_main_exists checked_fs) then (checked_vs, checked_fs)
					else raise(Failure("Function main not found."))