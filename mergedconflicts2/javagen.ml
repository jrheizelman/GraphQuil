(* Written by Jon Paul and Steven Weiner*)

open Ast
open Sast
open Printf


let rec writeToFile filename pString = (* writes to file *)
  let file = open_out (filename ^ ".java") in
    fprintf file "%s" pString


and write_code filename p = (* adds class structure to java file and has the code written *)
  let (symbolvarlist, functionlist) = p in
  let stmtString = gen_function_list functionlist and
      symbolString = gen_var_list symbolvarlist in 
  let output = sprintf "
public class %s {
  %s
  %s
}
  " filename symbolString stmtString in
  writeToFile filename output


and gen_var_list varlist = 
  let output = List.fold_left (fun a b -> a ^ (gen_var b)) "" varlist in
  sprintf "%s" output


and gen_var var = 
  let (name, t, scope) = var in
  sprintf "%s" name


and gen_name = function 
  Literal_t (t) -> string_of_int t
 | Id_t (_, t, _) -> t 
 | Binop_t (datatype, e1, op, e2) -> gen_name e1 ^
  (match op with
    Add -> " + " ^ gen_name e2 
    | Sub -> " - " ^ gen_name e2 
    | Mult -> " * " ^ gen_name e2
    | Div -> " / " ^ gen_name e2
    | Mod -> " mod " ^ gen_name e2
    | Equal -> " == " ^ gen_name e2 
    | Neq -> " != " ^ gen_name e2
      | Less -> " < " ^ gen_name e2 
      | Leq -> " <= " ^ gen_name e2 
      | Greater -> " > " ^ gen_name e2 
    | Geq -> " >= " ^ gen_name e2
    | And -> " && " ^ gen_name e2
    | Or -> " || " ^ gen_name e2)
  |_-> "other"


and gen_declaration = function
    Int -> "int "
  | Char -> "char "
  | String -> "String "
  | Bool -> "boolean "
  | Void -> "void "
  | _ -> "other "


and gen_function_list functionlist = 
  let output = List.fold_left (fun a b -> a ^ (gen_func b)) "" functionlist in
  sprintf "%s" output

and gen_block body=
    let vars = body.locals_t and
    stmts = body.statements_t in
    let output = gen_stmt_list stmts in
    sprintf "%s" output


and gen_stmt_list stmts =
    let output = List.fold_left (fun a b -> a ^ (gen_stmt b)) "" stmts in
    sprintf "%s" output

and gen_stmt = function
  Block_t(block) -> gen_block block
  | Expr_t(expr) -> gen_expr expr
  | Return_t(toreturn) -> gen_return_stmt toreturn
  | If_t(a,b,c) -> 
        (match c.statements_t with 
          [] -> "\nif (" ^ gen_name a ^ ")\n" ^ gen_block b
          |_ -> "\nif (" ^ gen_name a ^ ")\n" ^ gen_block b ^ "else\n" ^ gen_block c
        )
  | For_t(expr1, expr2, expr3, block) -> "for ( int " ^ gen_name expr1 ^ "=" ^ gen_name expr2 ^ "; <=" ^ gen_name expr3 ^ ";" ^ gen_name expr1 ^ "++)"^ gen_block block
  | While_t(expr, block) -> "while (" ^ gen_name expr ^ ")" ^ gen_block block

and gen_expr = function
Literal_t(l) -> string_of_int l
| Noexpr_t -> sprintf "1hello"
| Id_t(t, name, scope) -> sprintf "2hello"
| Binop_t(t, expr1, op, expr2) -> sprintf "3hello"
| Unop_t(t, op, expr) -> sprintf "4hello"
| Call_t(symtablefunc, exprlist) -> sprintf "5hello"
| String_Lit_t(str) -> sprintf "6hello"
| Char_t(str) -> sprintf "7hello"
| Assign_t(t, expr1, expr2) -> "\n"^gen_name expr1 ^ "=" ^ gen_name expr2
| Bool_Lit_t(b) -> string_of_bool b
(*| Add_at_t(expr1, expr2, attr) -> sprintf "hello"*)
    
    and gen_return_stmt expr =
      let output = (gen_expr expr) in
      sprintf "return %s;" output

and gen_func func = 
  let returntype = func.ret_t and
      funcname = func.fname_t and
      params = func.formals_t and
      internals = func.body_block_t in
      let helper = function
        "main" -> sprintf "public static void main (String[] args) {"
      | _ -> (
        let paramshelper para = List.fold_left (fun a b -> a ^ ((gen_var b) ^ ", ")) "" para in
        let helper2 rt fn para = 
          let t = gen_declaration rt in
          sprintf "public %s %s (%s) {\n" t fn (paramshelper para)
        in helper2 returntype funcname params
      ) in
      let output = (helper funcname) ^ (gen_block internals) ^ "\n}"
      in sprintf "%s" output