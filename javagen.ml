open Ast
open Sast
open Printf

let rec writeToFile filename pString = (* writes to file *)
  let file = open_out (filename ^ ".java") in
    fprintf file "%s" pString

and write_code filename p = (* adds class structure to java file and has the code written *)
  let (symbolvarlist, functionlist) = p in
  let stmtString = gen_function_list functionlist
  and symbolString = gen_var_list symbolvarlist in
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

and gen_function_list functionlist = 
  let output = List.fold_left (fun a b -> a ^ (gen_func b)) "" functionlist in
  sprintf "%s" output

and gen_func func = 
  let funcname = func.fname_t in ignore func;

  sprintf "hello2"
(*
and gen_stmt_list stmts =
  let output = List.fold_left (fun a b -> a ^ (gen_stmt b)) "" stmts in
  sprintf "%s" output

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