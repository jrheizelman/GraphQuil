include Ast
(*Graphquil*)

(*Make symbol table*)
module StringMap = Map.Make(String)

type expr_t = 
(*Difference between the first 4 lits and "Lit" or "char"?*)
    Int_Literal_t of int * validtype list
  | Double_Literal_t of double * validtype list
  | Bool_Literal_t of bool * validtype list
  | Char_Literal_t of char * validtype list
  | Noexpr_t
  | Id_t of string * validtype list
  | Binop_t of expr_t * op * expr_t * validtype list
  | Call_t of string * expr_t list * validtype list
  | Array_t of expr_t * expr_t * validtype list
  | Not_t of expr_t * validtype list
  | String_t of string * validtype list
  | Assign_t of expr * expr * validtype list
  | Construct_t of validtype * expr list * validtype list
  | MakeArr_t of validtype * expr * validtype list
  | Access_t of string * string * validtype list


type stmt_t =  
    Block_t of stmt_t list
  | Expr_t of expr_t
  | Return_t of expr_t
  | If_t of expr_t * stmt_t * stmt_t
  | For_t of expr_t * expr_t * expr_t * stmt_t
  | While_t of expr_t * stmt_t

type function = {
	param : var_entry list;
	ret_t : validtype list
}

type variable = {
	offset: int;
	typ : validtype list;
}