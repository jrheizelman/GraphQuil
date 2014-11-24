ocamllex scanner.mll #create scanner.ml
ocamlyacc parser.mly #create parser.ml and parser.mli
ocamlc -c ast.mli # compile AST types
ocamlc -c parser.mli # compile parser types
ocamlc -c scanner.ml # compile the scanner
ocamlc -c parser.ml # compile the parser
ocamlc -c graphquil.ml # compile the interpreter
ocamlc -o graphquil parser.cmo scanner.cmo graphquil.cmo
