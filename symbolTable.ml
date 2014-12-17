(*
Authors: Gemma Ragozzine
		John Heizelman
*)
open Ast

(* Symbol table made of a map containing pairs of String: ast.decl pairs *)
(* fst of each pair is the string of decl, snd of each pair is the decl type *)

module SymbolMap = Map.Make(String)
(*module TagMap = Map.Make(String)*)

(* each index in the array refers to a block and holds within it the scope of the parent block *)
let ancestor_scope = Array.make 1000 0


let string_of_decl = function
	  SymbTable_Var(n, t, id) -> string_of_variable (n,t) ^ " scope: " ^string_of_int id
	| SymbTable_Func(n, t, f, id) -> (string_of_valid_type t) ^ " " ^
										n ^ "(" ^
										String.concat ", " (List.map string_of_valid_type f) ^ ") scope: " ^ string_of_int id

(* table == env in lorax *)
let string_of_symbol_table (env:Ast.declaration SymbolMap.t * int * attribute list) = 
	let (v1, _, _) = env in 
		let symbol_list = SymbolMap.fold
			(fun f e symList -> (string_of_decl e) :: symList) v1 [] in
		let sortedMap = List.sort Pervasives.compare symbol_list in
		String.concat "\n" sortedMap

(* A symbol's id also refers to its scope *)
let rec symbol_table_get_id (name:string) (env:Ast.declaration SymbolMap.t * int * attribute list) =
	let(table, id, l) = env in
		let to_find = name ^ "_" ^ (string_of_int id) in
			if SymbolMap.mem to_find table then id
			else 
				if id = 0 then raise (Failure("Get id - Symbol " ^ name ^ " not declared in current scope! (Scope: " ^ string_of_int id ^ ")"))
				else symbol_table_get_id name (table, ancestor_scope.(id), l)

(* Look for symbol in given scope (block id) and if not found, recursively check all ancestor scopes*)
let rec symbol_table_find (name:string) (env:Ast.declaration SymbolMap.t * int * attribute list) = 
	let(table, id, l) = env in
		let to_find = name ^ "_" ^ (string_of_int id) in
			if SymbolMap.mem to_find table then SymbolMap.find to_find table
			else 
				if id = 0 then raise (Failure("Find name - Symbol " ^ name ^ " not declared in current scope! (Scope: " ^ string_of_int id ^ ")"))
				else symbol_table_find name (table, ancestor_scope.(id), l)

let rec symbol_table_add_decl (name:string) (decl:declaration) (env:Ast.declaration SymbolMap.t * int * attribute list) =
	let (table, id, l) = env in
		let to_find = name ^ "_" ^ (string_of_int id) in
			if SymbolMap.mem to_find table then raise(Failure("Symbol " ^ name ^ " already declared in this scope! (Scope: " ^ string_of_int id ^ ")"))
			else ((SymbolMap.add to_find decl table), id, l)

(* Recursively add list of variables to symbol table *)
let rec symbol_table_add_var_list (vars:variable list) (env:Ast.declaration SymbolMap.t * int * attribute list) = 
	match vars with
		  [] -> env
		| (var_name, var_type) :: tail -> 
			let (_, v2, _) = env in 
				let env = symbol_table_add_decl var_name (SymbTable_Var(var_name, var_type, v2)) env in
					symbol_table_add_var_list tail env

let rec symbol_table_add_stmt_list (stmts:stmt list) (env:Ast.declaration SymbolMap.t * int * attribute list) =
	match stmts with
		  [] -> env 
		| head :: tail -> let env = (match head with
			  Block(b) -> symbol_table_add_block b env
			| For(e1, e2, e3, b) -> symbol_table_add_block b env
			| While(e, b) -> symbol_table_add_block b env
			| If(e, b1, b2) -> let env = symbol_table_add_block b1 env in symbol_table_add_block b2 env
			| _ -> env) in symbol_table_add_stmt_list tail env

and symbol_table_add_block (b:block) (env:Ast.declaration SymbolMap.t * int * attribute list) = 
	let (table, id, l) = env in
		let (table, id, l) = symbol_table_add_var_list b.locals (table, b.block_num, l) in
			let (table, id, l) = symbol_table_add_stmt_list b.statements (table, id, l) in 
					ancestor_scope.(b.block_num) <- id;(* Note which scope we are putting this block into *)
				(table, id, l) (* Return old scope we started int(block id) and name of last statement we added*)

let symbol_table_add_func (f:func_decl) (env:Ast.declaration SymbolMap.t * int * attribute list) = 
	let (table, id, l) = env in 
		let args = List.map snd f.formals in (* Get the name of each formal *)
			let (table, _, l) = symbol_table_add_decl f.fname (SymbTable_Func(f.fname, f.ret, args, id)) env in
			let (table, id, l) = symbol_table_add_var_list f.formals (table, f.body_block.block_num, l) in
				symbol_table_add_block f.body_block (table, id, l)


let rec symbol_table_add_func_list (funcs:func_decl list) (env:Ast.declaration SymbolMap.t * int * attribute list) = 
	match funcs with
		  [] -> env
		| head :: tail -> let env = symbol_table_add_func head env in
		symbol_table_add_func_list tail env

let add_built_in_funcs (env:Ast.declaration SymbolMap.t * int * attribute list) = 
	symbol_table_add_decl "print" (SymbTable_Func("print", Int, [], 0)) env

let symbol_table_of_prog (p:Ast.program) =
	(* Table starts off as an empty map with scope (block id) set to 0 *)
	let env = add_built_in_funcs(SymbolMap.empty, 0, []) in
		let env = symbol_table_add_var_list (fst p) env in
			symbol_table_add_func_list (snd p) env