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
	| Add_at_t(t,s, at) -> t
	(*| Assign_at_t(t, _, _) -> t*)
	| Access_t(t, _, _) -> t

(*let type_check_attr (s, t, e) = 
	if type_of_expr e = t then attribute_t(s,t,e)
	else raise(Failure(string_of_expr_t e ^ " is not of type " ^ string_of_valid_type t))*)

let type_of_attribute (at:attribute) = match at with Attr(_,t,_) -> t

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

let check_assign (l:expr_t) (r:expr_t) = 
	let (l_t, r_t) = (type_of_expr l, type_of_expr r) in
		  if(l_t = r_t) then Assign_t(l_t, l, r)
		  else assign_err l_t r_t 

let check_at_for_tag (tag: string) (a:attribute) = match a with 
	Attr(s,t,e) -> if s = tag then true else false 

let handle_access (n:string) (tag:string) (ty:validtype) = match ty with 
	Node(l) -> if List.exists (check_at_for_tag tag) l then let at = List.find (check_at_for_tag tag) l in 
	 		Access_t(type_of_attribute at, n, tag)
	 	else raise(Failure("The tag " ^ tag ^ " is not associated with variable " ^ n))
	 | Edge(l) -> if List.exists (check_at_for_tag tag) l then let at = List.find (check_at_for_tag tag) l in 
	 		Access_t(type_of_attribute at, n, tag)
	 	else raise(Failure("The tag " ^ tag ^ " is not associated with variable " ^ n))
	 | _ -> raise(Failure("Left side of access must be Node or Edge type, " ^ 
			string_of_valid_type ty ^ " given."))

(*let check_assign_attribute (l:expr_t) (r:attribute_t) = 
	let(l_t, r_t) = (type_of_expr l, type_of_attribute r) in
		if(l_t = r_t) then Assign_at_t(l_t, l, r)
		else assign_err l_t r_t *)

(*let check_add (e1:expr_t) (e2:expr_t) env = 
	let (e1_t, e2_t) = (type_of_expr e1, type_of_expr) in match e1_t with
		Node(l) -> ();
		| Edge(l) -> ();
		| _ -> raise(Failure("Left side of add statement must be Node or Edge type, " ^ 
			string_of_valid_type e1_t ^ " given."))
		match e2_t with 
		Attr(a) -> Add_at_t(Node(a :: l))
		| _ -> -> raise(Failure("Right side of add statement must be Attribute type, " ^ 
			string_of_valid_type e1_t ^ " given."))*)

(*let check_right_add type_l checked_r checked_l = 
	let type_r = type_of_expr checked_r in match type_r with
		Attr(l) -> Add_at_t(type_l, checked_l, checked_r)
		| _ -> raise(Failure("Right side of add statement must be Attribute type, " ^ 
			string_of_valid_type type_r ^ " given."))*)

(*let check_access (e:expr_t) (s:string) env 
	(*pass in attribute, check name of expr of what it belongs to to e*) = (* e is the node/edge, s is the tag *)
	let e_t = type_of_expr e in
		if(e_t = Node || e_t = Edge) then
			let ((_, _, l), f) = (env, fun (at:attribute_t) -> let (t,_,e1) = tuple_of_attr_t at in t = s && e = e1) in
				if List.exists f l then
					let at = List.find f l in
						Access_t(type_of_attribute at, e, s)
				else raise(Failure("The tag " ^ s ^ " is not associated with " ^ string_of_expr_t e))
		else raise(Failure("Access must be on type Node or " ^ 
			"Edge, " ^ string_of_valid_type e_t ^ " given."))*)

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
 
 (* Unclear if we need this *)
let rec get_left_value_of_expr (e:expr_t) env = 
 	match e with 
 		Id_t(t, s, _) -> s
 		| Binop_t(t, l, o, r) -> get_left_value_of_expr l env
 		| Unop_t(t, o, l) -> get_left_value_of_expr l env
 		| _ -> raise(Failure("Cannot get the left value of expression."))

and check_left_value (e:expr) env = 
	match e with
		(* If left expression is an id *)
		Id(s) -> let (t, e, id) = check_valid_id s env in Id_t(t, e, id)
		(* If left expression is anything else *)
		| _ -> raise(Failure("Left hand side of assignment operator is improper type"))

(* Did not check Array, construct, makeArr, Access*)
and check_expr (e:expr) env = 
	match e with 
	 Literal(i) -> Literal_t(i) 
	 | Noexpr -> Noexpr_t
	 | Id(s) -> let (t, st, id) = check_valid_id s env in Id_t(t, st, id)
	 | Binop(e1, op, e2) -> 
	 	let(ce1, ce2) = (check_expr e1 env, check_expr e2 env) in
			check_binop ce1 ce2 op
	 | Unop(op, e) -> 
	 	let ce = check_expr e env in
	 		check_unop ce op
	 | Call(n, eList) -> 
	 	let checkedList = check_exprList eList env  in
	 		check_func_call n checkedList env
	 | String_Lit(s) -> String_Lit_t(s)
	 | Access(n, tag) -> let (ty, st, id) = check_valid_id n env in handle_access n tag ty
	 | Char_e(c) -> Char_t(c)
	 | Assign(l, r) -> 
	 	let checked_r = check_expr r env in
	 		let checked_l = check_left_value l env in
	 			check_assign checked_l checked_r
	 | Bool_Lit(b) -> Bool_Lit_t(b)
	 | Add_at(s, a) -> let (t, st, id) = check_valid_id s env in match t with
	 		Node(l) -> let t = Node(a :: l) in 
	 			ignore(symbol_table_override_decl st (SymbTable_Var(st,t,id)) (fst env, id));
	 			Add_at_t(t,s, a)
	 		| Edge(l) -> let t = Edge(a :: l) in 
	 			ignore(symbol_table_override_decl st (SymbTable_Var(st,t,id)) (fst env, id));
	 			Add_at_t(t,s, a)
	 		| _ -> raise(Failure("Left side of add statement must be Node or Edge type, " ^ 
				string_of_valid_type t ^ " given."))

	 		(*check that its a node or an edge, return access_t of (stored type, id name (expr_t), tag name) *)

	 		
	 		(*check_att_type stored type == attribute type? gotten from tag name*)



	 	(*let (nt, nstr, nid) = check_valid_id n env in
	 		let (att, atstr, atid)  = check_valid_id at env in
	 			if (nt != Node && nt != Edge) then raise(Failure("First argument must " ^ 
	 				"be of type Node, " ^ string_of_valid_type nt ^ " was given."))
	 		else
	 			match att with
	 				Int_at -> Add_at_t((Id_t(nt, nstr, nid), Id_t(att, atstr, atid)))
	 				| Bool_at -> Add_at_t((Id_t(nt, nstr, nid), Id_t(att, atstr, atid)))
	 				| Char_at -> Add_at_t((Id_t(nt, nstr, nid), Id_t(att, atstr, atid)))
	 				| String_at -> Add_at_t((Id_t(nt, nstr, nid), Id_t(att, atstr, atid)))
	 				| _ -> raise(Failure("Second argument must " ^ 
	 				"be an attribute type, " ^ string_of_valid_type nt ^ " was given."))*)

and check_exprList (eList: expr list) env = 
	match eList with
		  [] -> []
		| head::tail -> (check_expr head env) :: (check_exprList tail env)

let rec check_statement (s:stmt) ret_type env (scope:int) =
	match s with 
		  Block(b) -> 
			let checked_block = check_block b ret_type env scope in
				Block_t(checked_block)
		| Expr(e) -> Expr_t(check_expr e env)
		| Return(e) -> 
			let checked_e = check_expr e env in
				let type_e = type_of_expr checked_e in
					if (type_e = ret_type) then Return_t(checked_e)
				else raise(Failure("Function tries to return type " ^
					(string_of_valid_type type_e) ^ " but should return type " ^
					(string_of_valid_type ret_type) ^ "."))
		| If(e, b1, b2) -> 
			let ce = check_expr e env in
				let te = type_of_expr ce in
					(match te with
						  Bool -> If_t(ce, check_block b1 ret_type env scope, check_block b2 ret_type env scope)
						  | _ -> raise(Failure("If statement must evaluate on a boolean expression.")))
					
		| For(e1, e2, e3, b) ->
			let(c1, c2, c3) = (check_expr e1 env, check_expr e2 env, check_expr e3 env) in 
				if (type_of_expr c2 = Bool) then
					(* Increment scope, check block to be valid block *)
					For_t(c1, c2, c3, check_block b ret_type env (scope + 1))
				else raise(Failure("For loop condition must evaluate to a boolean expression"))
		| While(e, b) ->
			let ce = check_expr e env in
				if (type_of_expr ce = Bool)
					then While_t(ce, check_block b ret_type env (scope + 1))
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

and check_block (b:block) (ret_type:validtype) env (scope:int) = 
	let (table, _) = env in 
		let variables = check_is_vdecl_list b.locals (table, b.block_num) in
			let stmts = check_stmt_list b.statements ret_type (table, b.block_num) scope in
				{locals_t = variables; statements_t = stmts; block_num_t = b.block_num}


and check_stmt_list (s:stmt list) (ret_type:validtype) env (scope:int) =
	match s with
		  [] -> []
		| head::tail -> 
			check_statement head ret_type env scope :: check_stmt_list tail ret_type env scope

and check_is_vdecl_list (vars:variable list) env = 
	match vars with
		  [] -> []
		| head :: tail -> 
			let decl = symbol_table_find (fst head) env in
				let id = symbol_table_get_id (fst head) env in
					match decl with
						  SymbTable_Func(f) -> raise(Failure("Symbol "^ (fst head) ^
							 "is a function, not a vairable "))
						| SymbTable_Var(v) ->
							let varType = snd_of_three v in
								match varType with 
									validtype -> (fst_of_three v, snd_of_three v, id) :: check_is_vdecl_list tail env


let rec check_is_fdecl (func:string) env =
	let fdecl = symbol_table_find func env in
		match fdecl with
			  SymbTable_Var(v) -> raise(Failure("Symbol is a variable, not a function."))
			| SymbTable_Func(f) -> f

and check_function (f:func_decl) env = 
	let checked_block = check_block f.body_block f.ret env 0 in
		let (table, _) = env in 
			let checked_formals = check_is_vdecl_list f.formals (table, f.body_block.block_num) in
				let checked_scope = check_is_fdecl f.fname env in
					{fname_t = fst_of_four checked_scope; ret_t = f.ret; formals_t = checked_formals; body_block_t = checked_block }

and check_function_list (funcs:func_decl list) env = 
	match funcs with 
		  [] -> []
		| head::tail -> check_function head env :: check_function_list tail env

and check_main_exists (f:function_t list) = 
	if (List.filter main_fdecl f) = [] then false else true

and check_program (p:program) env = 
	let vs = fst p in
		let fs = snd p in
			let checked_vs = check_is_vdecl_list vs env in
				let checked_fs = check_function_list fs env in
					if(check_main_exists checked_fs) then (checked_vs, checked_fs)
					else raise(Failure("Function main not found."))