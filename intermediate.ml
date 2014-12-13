open ast
open sast
open semantic_check

let string_of_intermediate = function


(* Returns the string name for the date type*)
let rec get_datatype_name = function
 
Literal_t (t) -> string_of_int t
 | Id_t (_, t, _) -> t 
 | Binop_t (d, e1, op, e2) -> get_datatype_name e1 ^
 	(match op with
	  Add -> " + " ^ get_datatype_name e2 
	  | Sub -> " - " ^ get_datatype_name e2 
	  | Mult -> " * " ^ get_datatype_name e2
	  | Div -> " / " ^ get_datatype_name e2
	  | Mod -> " mod " ^ get_datatype_name e2
	  | Child -> " % " ^ get_datatype_name e2
	  | Equal -> " == " ^ get_datatype_name e2 
	  | Neq -> " != " ^ get_datatype_name e2
      | Less -> " < " ^ get_datatype_name e2 
      | Leq -> " <= " ^ get_datatype_name e2 
      | Greater -> " > " ^ get_datatype_name e2 
	  | Geq -> " >= " ^ get_datatype_name e2
	  | And -> " && " ^ get_datatype_name e2
	  | Or -> " || " ^ get_datatype_name e2 )
 | Unop_t (d, e1, op) -> get_datatype_name e1 ^
 	(match op with
 	 Neg-> "-"
 	 | Not -> "!")
 | Call_t (f, ) (*unsure of meaning of construction in semantic file*)
 | Array_t (t, _, _) -> t (*should be similar to Assign_t with get_datatype_name function implemented *)
 | String_Lit_t (s) -> s
 | Char_t (c) -> c (*does this not have to be "\'" ^ (String.make 1) c ^"\'"?*)
 | Assign_t (t, l, _) -> get_datatype_name t ^ " = " ^ get_datatype_name l (* doesn't this needs to be redined in semantic file as Assign_t (t, _, _)->?*)
 (*| Construct_t (t, _) *)-> t  (*should be similar to Assign_t with get_datatype_name function implemented *)
 | MakeArr_t (t, _) -> t
 | Access_t (t, _, _) -> t
 | Bool_Literal_t (b) -> string_of_bool b
 | Noexpr_t -> "" (*don't see Noexpr in our ast*)


(* Returns the java form of a statement*)
let rec get_java_statement = function

Block_t (stmts) -> "{\n" ^ String.concat "" (List.map get_java_statement stmts) ^ "}\n" (*maps the recursive function to each element in the statement list provided as a function argument*)

| Expr_t (expr) -> get_datatype_name expr^ ";\n";

| Return_t (expr) -> "return " ^ get_datatype_name expr ^ ";\n"; 

| If_t (expr, stmts, Block_t([])) -> "if (" ^ get_datatype_name expr ^ ")\n" ^ get_java_statement stmts 

| If_t (expr, stmts, stmts2) -> "if (" ^ get_datatype_name expr ^ ")\n" ^ get_java_statement stmts ^ "else\n" ^ get_java_statement stmts2

| For_t (expr1, expr2, expr3, stmts) -> "for ( int " ^ get_datatype_name expr1 ^ "=" get_datatype_name expr2 "; i<=" ^ get_datatype_name expr3 ^ ";" ^get_datatype_name expr1^"++)" ^ get_java_statement stmts

| While_t (expr, stmts) -> "while (" ^ get_datatype_name expr ^ ") " ^ get_java_statement stmts





(*in let rec get_datatype_as_string hasName = function *)