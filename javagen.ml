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


and gen_statement stmt= function
(*Block_t(t) -> "{\n" ^ String.concat "" (List.map gen_statement stmt) ^ "}\n" *)
Expr_t(t)-> gen_name t ^ ";\n"
| _-> "this is a block"


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
    | Or -> " || " ^ gen_name e2
    | _-> "other " ^ gen_name e2)
  |_-> "other"


and gen_declaration = function
    Int -> " int "
  | Char -> " char "
  | String -> " String "
  | Bool -> " boolean "
  | Void -> " void "
  | _ -> " other "


and gen_function_list functionlist = 
  let output = List.fold_left (fun a b -> a ^ (gen_func b)) "" functionlist in
  sprintf "%s" output

and gen_block body=
    let output= Sast.string_of_block_t body in
    output

and gen_func func = 
  let returntype = func.ret_t and
      funcname = func.fname_t and
      params = func.formals_t and
      internals = func.body_block_t 
      and helper = function
        "main" -> sprintf "public static void main(String[] args)"
      | _ -> sprintf "hello there!" in
      let output = helper funcname
      in sprintf "%s \n%s\n" (gen_declaration returntype) (gen_block internals)


(*
and gen_stmt stmt = (* generates statements in java *)
  let (a,b,c) = stmt in
  match b with
    Return_t(exp) -> gen_return_stmt exp
  | _ -> sprintf "other"

and gen_expr expr = 
  match expr with (* generates expressions in java *)
    string -> sprintf "%s" (string_of_expr_t expr)
  | _ -> sprintf "other"

and gen_return_stmt exp = (* generates return statement in java *)
  (*let output = (gen_expr exp) in*)
  sprintf "return %s;" (string_of_expr_t exp)*)