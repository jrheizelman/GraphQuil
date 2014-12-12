open Unix

type action = Ast | Symtab | SAnalysis | Help

let usage (name:string) =
  "usage:\n" ^ name ^ "\n" ^
    "        -a source.lrx              (Print AST of source)\n" ^
    "        -t source.lrx              (Print Symbol Table of source)\n" ^
    "        -s source.lrx              (Run Semantic Analysis over source)\n" ^    



(*   let _ =
  let lexbuf = Lexing.from_channel stdin in
  Parser.program Scanner.token lexbuf *)


  let _ =
  let action = 
  if Array.length Sys.argv > 1 then
    (match Sys.argv.(1) with
        "-a" -> if Array.length Sys.argv == 3 then Ast else Help
      | "-t" -> if Array.length Sys.argv == 3 then Symtab else Help
      | "-s" -> if Array.length Sys.argv == 3 then SAnalysis else Help
      | _ -> Help)
  else Help in  
	match action with
      Help -> print_endline (usage Sys.argv.(0)) 
    | (Ast | Symtab | SAnalysis ) ->
      let input = open_in Sys.argv.(2) in
      let lexbuf = Lexing.from_channel input in
      let program = Parser.program Scanner.token lexbuf in
      (match action with
          Ast -> let listing = Ast.string_of_program program
                 in print_string listing
        | Symtab -> let env = Symtab.symtab_of_program program in
                    print_string (Symtab.string_of_symtab env)
        | SAnalysis -> let env = Symtab.symtab_of_program program in
                    let checked = Check.check_program program env in
                    ignore checked; print_string "Passed Semantic Analysis.\n" 
        | Help -> print_endline (usage Sys.argv.(0)))