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
  let output = List.fold_left (fun a b -> a ^ ((gen_var b)) ^ ";") "" varlist in
  sprintf "%s\n" output

and gen_var var = 
  let (name, t, scope) = var in
  sprintf "%s %s" (gen_type t) name

and gen_type = function
    Int -> " int "
  | Char -> " char "
  | String -> " String "
  | Bool -> " boolean "
  | Void -> " void "
  | _ -> " other "

and gen_function_list functionlist = 
  let output = List.fold_left (fun a b -> a ^ (gen_func b)) "" functionlist in
  sprintf "%s\n" output

and gen_func func = 
  let returntype = func.ret_t and
      funcname = func.fname_t and
      params = func.formals_t and
      internals = func.body_block_t in
      let helper = function
        "main" -> sprintf "public static void main(String[] args) {"
      | _ -> (
        let paramshelper para = List.fold_left (fun a b -> a ^ ((gen_var b) ^ ", ")) "" para in
        let helper2 rt fn para = 
          let t = gen_type rt in
          sprintf "public %s %s (%s) {\n" t fn (paramshelper para)
        in helper2 returntype funcname params
      ) in
      let output = (helper funcname) ^ (gen_block internals) ^ "\n}\n"
      in sprintf "%s\n" output

and gen_block block = 
  let vars = block.locals_t and
      stmts = block.statements_t in
  let output = (gen_var_list vars) ^ (gen_stmt_list stmts) in
  sprintf "%s\n" output

and gen_stmt_list stmts = 
  let output = List.fold_left (fun a b -> a ^ (gen_stmt b)) "" stmts in
  sprintf "%s\n" output

and gen_stmt = function 
    Block_t(block) -> gen_block block
  | Expr_t(expr) -> gen_expr expr
  | Return_t(toreturn) -> gen_return_stmt toreturn
  | If_t(condition, block1, block2) -> gen_if_stmt condition block1 block2
  | For_t(expr1, expr2, expr3, block) -> sprintf "for"
  | While_t(expr, block) -> sprintf "while"


and gen_return_stmt expr = 
  let output = (gen_expr expr) in
  sprintf "return %s;\n" output

and gen_if_stmt condition block1 block2 = 
  sprintf "if(%s) {\n%s\n}\nelse {\n%s\n}" (gen_expr condition) (gen_block block1) (gen_block block2)

and gen_expr = function
    Literal_t(lit) -> string_of_int lit
  | Noexpr_t -> ""
  | Id_t(t, name, scope) -> sprintf "%s" name
  | Binop_t(t, expr1, op, expr2) -> gen_binop t expr1 op expr2
  | Unop_t(t, op, expr) -> ""
  | Call_t(symtablefunc, exprlist) -> gen_call_func symtablefunc exprlist
  | String_Lit_t(str) -> Char.escaped '"' ^ str ^ Char.escaped '"'
  | Char_t(str) -> sprintf "'%s'" str
  | Assign_t(t, expr1, expr2) -> gen_assign t expr1 expr2
  | Bool_Lit_t(b) -> string_of_bool b
  | Add_at_t(expr1, expr2) -> string_of_expr_t expr1 ^ " add " ^ string_of_expr_t expr2

and gen_assign t expr1 expr2 = 
  let output = sprintf "%s = %s;" (gen_expr expr1) (gen_expr expr2) in
  sprintf "%s\n" output

and gen_binop datatype e1 op e2 = 
  match op with
    Add -> gen_expr e1 ^ " + " ^ gen_expr e2 ^ ";\n"
  | Sub -> gen_expr e1 ^ " - " ^ gen_expr e2 ^ ";\n"
  | Mult -> gen_expr e1 ^ " * " ^ gen_expr e2 ^ ";\n"
  | Div -> gen_expr e1 ^ " / " ^ gen_expr e2 ^ ";\n"
  | Mod -> gen_expr e1 ^ " mod " ^ gen_expr e2 ^ ";\n"
  | Equal -> gen_expr e1 ^ " == " ^ gen_expr e2
  | Neq -> gen_expr e1 ^ " != " ^ gen_expr e2
  | Less -> gen_expr e1 ^ " < " ^ gen_expr e2
  | Leq -> gen_expr e1 ^ " <= " ^ gen_expr e2
  | Greater -> gen_expr e1 ^ " > " ^ gen_expr e2
  | Geq -> gen_expr e1 ^ " >= " ^ gen_expr e2
  | And -> gen_expr e1 ^ " && " ^ gen_expr e2
  | Or -> gen_expr e1 ^ " || " ^ gen_expr e2
  | _ -> "other"

and gen_call_func symtablefunc exprlist = 
  let (name, t, s, scope) = symtablefunc in
  let namehelper name = 
    match name with
      "print" -> "System.out.print"
    |  _ -> name in
  let argshelper args = List.fold_left (fun a b -> a ^ ((gen_expr b) ^ ", ")) "" args in
  let output = sprintf "%s(%s);" (namehelper name) (argshelper exprlist) in
  sprintf "%s\n" output


(*
and gen_stmt stmt = (* generates statements in java *)
  let (a,b,c) = stmt in
  match b with
    Return_t(exp) -> gen_return_stmt exp
  | _ -> sprintf "other"
(*
and gen_expr expr = 
  match expr with (* generates expressions in java *)
    string -> sprintf "%s" (string_of_expr_t expr)
  | _ -> sprintf "other"

and gen_return_stmt exp = (* generates return statement in java *)
  (*let output = (gen_expr exp) in*)
  sprintf "return %s;" (string_of_expr_t exp)*)*)