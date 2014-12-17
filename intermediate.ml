open Sast

let match_initial_type = function
    fname_t -> sprintf "other"
  | formals_t -> sprintf "other"
  | ret_t -> sprintf "other"
  | body_block_t -> sprintf "other"

let rec get_datatype_name = function
 
Literal_t (t) -> string_of_int t
 | Id_t (_, t, _) -> t 
 | Binop_t (d, e1, op, e2) -> get_datatype_name e1 in
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
 | Unop_t (d, e1, op) -> get_datatype_name e1 in
 	(match op with
 	 Neg-> "-"
 	 | Not -> "!")
 | Call_t (f, ) (*unsure of meaning of construction in semantic file*)
 (*| Array_t (t, _, _) -> t Removed from SAST, no longer needed atm*) (*should be similar to Assign_t with get_datatype_name function implemented *)
 | String_Lit_t (s) -> s
 | Char_t (c) -> c (*does this not have to be "\'" ^ (String.make 1) c ^"\'"?*)
 | Assign_t (t, l, _) -> t ^ " = " ^ l (* doesn't this needs to be redined in semantic file as Assign_t (t, _, _)->?*)
 (*| Construct_t (t, _) -> Removed from SAST, no longer needed atm*)
 (*| MakeArr_t (t, _) -> t Removed from SAST, no longer neeeded atm*)
 | Access_t (t, _, _) -> t
 | Bool_Literal_t (b) -> string_of_bool b
 | Noexpr_t -> "" (*don't see Noexpr in our ast*)