open Sast
open Printf

let rec writeToFile filename pString = (* writes to file *)
  let file = open_out ("java/" ^ filename ^ ".java") in
    fprintf file "%s" pString

and write_code filename p = (* adds class structure to java file and has the code written *)
  (*let stmtString = gen_stmt_list p in*)
  let stmtString = match_type p in
  let output = sprintf "
  public class %s {
      %s
  }
  " filename stmtString in
    writeToFile filename output;
    output

and match_type = function
    fname_t -> sprintf "other"
  | formals_t -> sprintf "other"
  | ret_t -> sprintf "other"
  | body_block_t -> sprintf "other"

and gen_stmt_list stmts =
  let output = List.fold_left (fun a b -> a ^ (gen_stmt b)) "" stmts in
  sprintf "%s" output

and gen_stmt stmt = (* generates statements in java *)
  let (a,b,c) = stmt in
  match b with
    Return_t(exp) -> gen_return_stmt exp
  | _ -> sprintf "other"

and gen_expr = function (* generates expressions in java *)
    _ -> sprintf "other"

and gen_return_stmt exp = (* generates return statement in java *)
  let output = (gen_expr exp) in
  sprintf "return %s;" (string_of_expr_t exp)

(* similar functions have to be added for loops, declarations, literals, etc *)