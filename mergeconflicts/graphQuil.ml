open Unix

type action = Ast | SymbolTable | Sast | SAnalysis | Intermediate | Compile | Help

let usage (name:string) =
  "usage:\n" ^ name ^ "\n" ^
    "        -a source.lrx              (Print AST of source)\n" ^
    "        -sa source.lrx             (Print SAST of source)\n" ^
    "        -t source.lrx              (Print Symbol Table of source)\n" ^
    "        -s source.lrx              (Run Semantic Analysis over source)\n" ^
    "        -i source.lrx              (Print intermediate representation of source)\n" ^
    "        -c source.lrx              (Compile source)\n"
 
let usage (name:string) =
"usage:\n" ^ name ^ "\n" ^
" -a source.lrx (Print AST of source)\n" ^
" -t source.lrx (Print Symbol Table of source)\n" ^
" -s source.lrx (Run Semantic Analysis over source)\n"

let get_compiler_path (path:string) =
try
let i = String.rindex path '/' in
String.sub path 0 i
with _ -> "."
let _ =
  let action = 
  if Array.length Sys.argv > 1 then
    (match Sys.argv.(1) with
        "-a" -> if Array.length Sys.argv == 3 then Ast else Help
      | "-sa" -> if Array.length Sys.argv == 3 then Sast else Help
      | "-t" -> if Array.length Sys.argv == 3 then SymbolTable else Help
      | "-s" -> if Array.length Sys.argv == 3 then SAnalysis else Help
      | "-i" -> if Array.length Sys.argv == 3 then Intermediate else Help
      | "-c" -> if Array.length Sys.argv == 3 then Compile else Help
      | _ -> Help)
  else Help in   

  match action with
      Help -> print_endline (usage Sys.argv.(0)) 
    | (Ast | SymbolTable | Sast| SAnalysis | Intermediate | Compile ) ->
      let input = open_in Sys.argv.(2) in
      let lexbuf = Lexing.from_channel input in
      let program = Parser.program Scanner.token lexbuf in
      match action with
          Ast -> let listing = Ast.string_of_prog program
                 in print_string listing
        | SymbolTable -> let env = SymbolTable.symbol_table_of_prog program in
                    print_string (SymbolTable.string_of_symbol_table env ^ "\n")
        | SAnalysis -> let env = SymbolTable.symbol_table_of_prog program in
                    let checked = Semantic_check.check_program program env (SymbolTable.empty_tag_map) in
                    ignore checked; print_string "Passed Semantic Analysis.\n"
        | Sast -> let env = SymbolTable.symbol_table_of_prog program in
                    let checked = Semantic_check.check_program program env (SymbolTable.empty_tag_map) in
                      let listing_t = Sast.string_of_prog_t checked in
                        print_string listing_t
        (*| Intermediate -> print_string "intermediate" (*let env = SymbolTable.symbol_table_of_prog program in 
                          let checked = Semantic_check.check_program program env in
                          let inter = Intermediate.string_of_intermediate checked(*print_string "hello\n"*)*)*)
        | Compile -> let env = SymbolTable.symbol_table_of_prog program in
<<<<<<< HEAD
                     let checked = Semantic_check.check_program program env (SymbolTable.empty_tag_map) in
                     let (a, b) = checked in
                     Javagen.write_code "graphQuil" checked; print_string "compiled"
=======
<<<<<<< HEAD
                      let checked = Semantic_check.check_program program env (SymbolTable.empty_tag_map) in
                        let (a, b) = checked in
                          Javagen.write_code "graphQuil" b; print_string "compiled"
>>>>>>> 85ef9c1fc321b61fe539635e97f70248a9e3fb6e
        | Help -> print_endline (usage Sys.argv.(0)) (* impossible case *)
(*let action =
if Array.length Sys.argv > 1 then
(match Sys.argv.(1) with
"-a" -> if Array.length Sys.argv == 3 then Ast else Help
| "-t" -> if Array.length Sys.argv == 3 then SymbolTable else Help
| "-s" -> if Array.length Sys.argv == 3 then SAnalysis else Help
| _ -> Help)
else Help in
match action with
Help -> print_endline (usage Sys.argv.(0))
| (Ast | SymbolTable | SAnalysis ) ->
let input = open_in Sys.argv.(2) in
let lexbuf = Lexing.from_channel input in
let program = Parser.program Scanner.token lexbuf in
(match action with
Ast -> let listing = Ast.string_of_prog program
in print_string listing
| SymbolTable -> let env = SymbolTable.symbol_table_of_prog program in
print_string (SymbolTable.string_of_symbol_table env ^ "\n")
| SAnalysis -> let env = SymbolTable.symbol_table_of_prog program in
let checked = Semantic_check.check_program program env in
ignore checked; print_string "Passed Semantic Analysis.\n"
| Help -> print_endline (usage Sys.argv.(0))) (* impossible case *)*)
=======
                     let checked = Semantic_check.check_program program env (SymbolTable.empty_tag_map) in
                     Javagen.write_code "graphQuil" checked; print_string "compiled"
        | Help -> print_endline (usage Sys.argv.(0)) (* impossible case *)
>>>>>>> a94973ef4fa79aff2aa14e6d4067029a1b50c271
