open Intermediate
open Printf

let rec writeToFile filename pString = (* writes to file *)
  let file = open_out ("java/" ^ filename ^ ".java") in
    fprintf file "%s" pString

and write_code filename p = (* adds class structure to java file and has the code written *)
  let stmtString = gen_stmt_list p in
  let output = sprintf "
  public class %s {
      %s
  }
  " filename stmtString in
    writeToFile fileName output;
    output

and gen_stmt_list stmts =
  let output = List.fold_left (fun a b -> a ^ (gen_stmt b)) "" stmts in
  sprintf "%s" output

and gen_stmt = function (* generates statements in java *)
    Return(exp, _) -> gen_return_stmt exp
  | _ -> sprintf "other"

and gen_expr = function (* generates expressions in java *)
    _ -> sprintf "other"

and gen_return_stmt exp = (* generates return statement in java *)
  let output = (gen_expr exp) in
  sprintf "return %s;" expStr

(* similar functions have to be added for loops, declarations, literals, etc *)