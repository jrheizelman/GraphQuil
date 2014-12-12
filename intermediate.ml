open Ast
open Sast
open Semantic_check

(* Returns the string name for the date type*)
let rec get_data_type = function
 
Int_Literal_t (n, d) -> string_of_int n
 | Double_Literal_t of double * validtype list (*before we include this, we need to define string_of_double in ast*)
 | Bool_Literal_t (l, d) -> string_of_bool l
 | Char_Literal_t (n, d) -> "\'" ^ (String.make 1) n ^"\'"
 | Noexpr_t -> "" (*don't see Noexpr in our ast*)
 | Id_t (s, d) -> s  (*will double check this construction vs. (match (String.get s 0) with 'R' -> s^"()"*)
 | Binop_t of expr_t (e1, op, e2, d) -> get_data_type e1 ^
 	(match op with
	  Add -> "+ " ^ get_data_type e2 
	  | Sub -> "- " ^ get_data_type e2 
	  | Mult -> "* " ^ get_data_type e2
	  | Div -> "/ " ^ get_data_type e2
	  | Mod -> "mop" ^ get_data_type e2
	  | Child -> "%" ^ get_data_type e2
	  | Equal -> "== " ^ get_data_type e2 
	  | Neq -> "!= " ^ get_data_type e2
      | Less -> "< " ^ get_data_type e2 
      | Leq -> "<= " ^ get_data_type e2 
      | Greater -> "> " ^ get_data_type e2 
	  | Geq -> ">= " ^ get_data_type e2
	  | And -> "&& " ^ get_data_type e2
	  | Or -> "|| " ^ get_data_type e2 

 | Call_t (f, argl, d) (*this will be lengthy*)

 (*below this line is still in progress*)
 | Array_t of expr_t * expr_t * validtype list
 | Not_t of expr_t * validtype list
 | String_t of string * validtype list
 | Assign_t of expr * expr * validtype list
 | Construct_t of validtype * expr list * validtype list
 | MakeArr_t of validtype * expr * validtype list
 | Access_t of string * string * validtype list



type ir_expr=
Ir_Int_Literal of ir

type ir_stmt=
|Ir_If of ir_var_decl * string 