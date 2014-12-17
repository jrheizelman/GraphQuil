(* 
Written by Jon Paul
*)


open Ast
open Sast
open Printf
open Semantic_check


(* Returns the string name for the date type*)
let rec get_datatype_name = function
Literal_t (t) -> string_of_int t
 | Id_t (_, t, _) -> t 
 | Binop_t (datatype, e1, op, e2) -> get_datatype_name e1 ^
 	(match op with
	  Add -> " + " ^ get_datatype_name e2 
	  | Sub -> " - " ^ get_datatype_name e2 
	  | Mult -> " * " ^ get_datatype_name e2
	  | Div -> " / " ^ get_datatype_name e2
	  | Mod -> " mod " ^ get_datatype_name e2
	  (*| Child -> " % " ^ get_datatype_name e2 *)
	  | Equal -> " == " ^ get_datatype_name e2 
	  | Neq -> " != " ^ get_datatype_name e2
      | Less -> " < " ^ get_datatype_name e2 
      | Leq -> " <= " ^ get_datatype_name e2 
      | Greater -> " > " ^ get_datatype_name e2 
	  | Geq -> " >= " ^ get_datatype_name e2
	  | And -> " && " ^ get_datatype_name e2
	  | Or -> " || " ^ get_datatype_name e2 )
 | Unop_t (datatype, op, e1) -> get_datatype_name e1 ^
 	(match op with
 	 Neg-> "-"
 	 | Not -> "!")
 | Call_t ((arg1,arg2, arg3, arg4), arg5 ) -> arg1
 (*| Array_t (t, _, _) -> t Removed from SAST, no longer needed atm*) (*should be similar to Assign_t with get_datatype_name function implemented *)
 | String_Lit_t (s) -> s
 | Char_t (c) -> c (*does this not have to be "\'" ^ (String.make 1) c ^"\'"?*)
 (* Note sure how to do assignment operator atm| Assign_t (_, e1, e2) ->  *) (* doesn't this needs to be redined in semantic file as Assign_t (t, _, _)->?*)
 (*| Construct_t (t, _) -> Removed from SAST, line no longer needed atm*)
 (*| MakeArr_t (t, _) -> t Removed from SAST, line no longer neeeded atm*)
 (*| Access_t (t, _, _) -> t) Removed from SAST, line no longer necessary atm*)
 | Bool_Lit_t (b) -> string_of_bool b
 | Noexpr_t -> "" (*don't see Noexpr in our ast*)

 (* Returns the statement in java form as a string*)
let rec get_java_statement = function

Block_t(t) -> "{\n" ^ String.concat "" (List.map get_java_statement t.statements_t) ^ "}\n" (*maps the recursive function to each element in the statement list provided as a function argument*)
| Expr_t (expr) -> get_datatype_name expr^ ";\n";
| Return_t (expr) -> "return " ^ get_datatype_name expr ^ ";\n"; 
(*| If_t (expr, stmts, Block_t([]))-> "if (" ^ get_datatype_name expr ^ ")\n" ^ List.map get_java_statement stmts *)
| If_t (expr, stmts, stmts2) -> 
		(match stmts2.statements_t with 
		 [] -> "if (" ^ get_datatype_name expr ^ ")\n" ^ Sast.string_of_block_t stmts
		| _ -> "if (" ^ get_datatype_name expr ^ ")\n" ^ Sast.string_of_block_t stmts ^ "else\n" ^ Sast.string_of_block_t stmts2)
| For_t (expr1, expr2, expr3, stmts) -> "for ( int " ^ get_datatype_name expr1 ^ "=" ^ get_datatype_name expr2 ^ "; i<=" ^ get_datatype_name expr3 ^ ";" ^get_datatype_name expr1 ^ "++)" ^ Sast.string_of_block_t stmts
| While_t (expr, stmts) -> "while (" ^ get_datatype_name expr ^ ") " ^ Sast.string_of_block_t stmts

(* Returns java declaration of the datatype as a string*)
(* Not entirely sure that left hand side is valid in this form *)
let get_java_declaration hasID expr = 
	(match hasID with 
		(*each of these is taken from the ast.ml file with the line "type validtype"*)
		(*if Int doesn't work, revert back to validtype(int)*)
		Int -> "int "
		| Char -> "char "
		| String -> "String "
		| Bool -> "boolean "
		| Void -> "void " ) ^ " " ^
 		(match expr with
			Assign_t (t, expr1, _) -> get_datatype_name expr1 ^ 
				(match hasID with 
					String-> "= new String(" ^ get_datatype_name expr ^ ");"
					| _ -> "= " ^ get_datatype_name expr ^ ";")
			| _ -> get_datatype_name expr ^ ";")

let rec writeToFile fileName pString = 
  let file = open_out (fileName ^ ".java") in
    fprintf file "%s"  pString

and createJavaProgram program = 
	let body = String.concat "" (List.rev (List.map get_java_statement program)) in
	let output = sprintf "
	public class GraphQuil {
		%s
	}
	" body in
	writeToFile "graphQuil" output

(*let _ =
	let lexbuf = Lexing.from_channel stdin in
	let program = Parser.program Scanner.token lexbuf in
	let file = open_out "graphquil.java" in
	createJavaProgram file program
	close_out file*)