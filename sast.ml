(*
Author: Gemma Ragozzine
        John Heizelman
*)

open Ast
(*Graphquil*)

(* Elements from AST but with typing added*)

let fst_of_three (t, _, _) = t
let snd_of_three (_, t, _) = t
let fst_of_four (t, _, _, _) = t

type expr_t = 
    Literal_t of int
  | Noexpr_t
  | Id_t of validtype * string * int
  | Binop_t of validtype * expr_t * bop * expr_t 
  | Unop_t of validtype  * uop * expr_t
  | Call_t of symbol_table_func * expr_t list
  | String_Lit_t of string 
  | Char_t of string 
  | Assign_t of validtype * expr_t * expr_t
  | Bool_Lit_t of bool
  | Add_at_t of validtype * string * attribute (* each expr_t is the id of the node and attr, type checked *)
  (*| Assign_at_t of validtype * expr_t * attribute_t*)
  | Access_t of validtype * string * string (* validtype is what attr is holding, Char, String, etc, expr_t is node/edge, string is tag*)

type stmt_t =  
    Block_t of block_t
  | Expr_t of expr_t
  | Return_t of expr_t
  | If_t of expr_t * block_t * block_t
  | For_t of expr_t * expr_t * expr_t * block_t
  | While_t of expr_t * block_t
  (*| Link_t of expr_t * expr_t*)
  (*| Vdecl_t of validtype * string * expr_t*)

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


(*************************
**** PRINT SAST **********
*************************)


let rec string_of_expr_t = function
    Literal_t(n) -> string_of_int n
  | Char_t(n) -> "\'" ^ n ^"\'"
  | Id_t(t, s, i) -> s
  | String_Lit_t(s) -> s 
  | Bool_Lit_t(l) -> string_of_bool l
  | Binop_t(t, e1, op, e2) ->
      string_of_expr_t e1 ^ " " ^ 
      string_of_bop op  ^ " " ^
      string_of_expr_t e2
  | Unop_t(t, op, e) -> 
      string_of_unop op ^ " " ^ 
      string_of_expr_t e
  | Assign_t(t, v, e) -> 
      string_of_expr_t v  ^ " = " ^
      string_of_expr_t e
  | Call_t(f, argl) ->
    fst_of_four f ^ "(" ^ String.concat ", " (List.map string_of_expr_t argl) ^ ")" 
  | Noexpr_t -> ""
  | Add_at_t(_,s, e) -> s ^ " add " ^ string_of_attribute e
    (*| Assign_at_t (t, e, a) -> string_of_expr_t e ^ " = " ^ string_of_attribute_t a*)
  | Access_t (t, s1, s2) -> s1 ^ "[\"" ^ s2 ^ "\"]"

  (*and string_of_attribute_t a = 
    let (s, t, e) = a in 
      "[\"" ^ s ^ "\" " ^ string_of_valid_type t ^ " " ^ string_of_expr_t e ^ "]"*)
  
  let string_of_symb_table_var v = string_of_valid_type (snd_of_three v) ^ " " ^ fst_of_three v

  let rec string_of_stmt_t = function
    Block_t(b) -> string_of_block_t b
  | Expr_t(expr) -> string_of_expr_t expr ^ ";\n"
  | Return_t(expr) -> "return " ^ string_of_expr_t expr ^ ";\n";
  | If_t(e1, b1, b2) -> 
      (match b2.statements_t with
        [] -> "if (" ^ string_of_expr_t e1 ^ ")\n" ^ string_of_block_t b1
      | _  -> "if (" ^ string_of_expr_t e1 ^ ")\n" ^
              string_of_block_t b1 ^ "else\n" ^ string_of_block_t b2)
  | For_t(a1, e, a2, b) ->
      "for (" ^ string_of_expr_t a1 ^ "; " ^ 
                string_of_expr_t e ^ "; " ^
                string_of_expr_t a2 ^ ") {\n" ^ 
                string_of_block_t b ^ "}"
  | While_t(e, b) -> 
      "while (" ^ string_of_expr_t e ^ ") {\n" ^ 
      string_of_block_t b ^ "}"
  (*| Vdecl(t, id) -> 
      string_of_valid_type t ^ " " ^ id ^ ";"*)

  and string_of_block_t (b:block_t) = "{\n" ^
    String.concat ";\n" (List.map string_of_symb_table_var b.locals_t) ^ (if (List.length b.locals_t) > 0 then ";\n" else "") ^
    String.concat "" (List.map string_of_stmt_t b.statements_t) ^
    "}\n"

  let string_of_func_t fdecl = (string_of_valid_type fdecl.ret_t) ^ " " ^
    fdecl.fname_t ^ "(" ^ String.concat ", " (List.map string_of_symb_table_var fdecl.formals_t) ^ ")\n" ^
    (string_of_block_t fdecl.body_block_t)

  let string_of_prog_t prog = String.concat ";\n" (List.map string_of_symb_table_var (fst prog)) ^
    (if (List.length (fst prog)) > 0 then ";\n" else "") ^
    String.concat "\n" (List.map string_of_func_t (snd prog))
