open Ast
(*Graphquil*)

(* Elements from AST but with typing added*)

type expr_t = 
    Literal_t of int
  | Noexpr_t
  | Id_t of validtype * string * int
  | Binop_t of validtype * expr_t * bop * expr_t 
  | Unop_t of validtype * expr_t * uop
  | Call_t of symbol_table_func * expr_t list
  | String_Lit_t of string 
  | Char_t of string 
  | Assign_t of validtype * expr_t * expr_t
  | Bool_Lit_t of bool
  | Doub_Lit_t of float


type stmt_t =  
    Block_t of block_t
  | Expr_t of expr_t
  | Return_t of expr_t
  | If_t of expr_t * block_t * block_t
  | For_t of expr_t * expr_t * expr_t * block_t
  | While_t of expr_t * block_t

and block_t = {
  locals_t : symbol_table_var list;
  statements_t : stmt_t list;
  block_num_t : int
}

type function_t = {
  fname_t: string;
	formals_t : symbol_table_var list;
	ret_t : validtype;
  body_block_t : block_t;
}

type program_t = symbol_table_var list * function_t list