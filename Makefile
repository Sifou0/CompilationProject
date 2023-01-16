INTERFACES = parser.mli
SOURCES    = ast.ml parser.ml lexer.ml test.ml main.ml
GENERATED  =  lexer.ml parser.ml parser.mli parser.automaton parser.conflicts

tp: parser.mli $(SOURCES)
	ocamlc -c ast.ml
	ocamlc $(INTERFACES)
	ocamlc -o tp $(SOURCES)

lexer.ml :lexer.mll parser.mli ast.ml
	ocamllex lexer.mll

parser.mli : parser.mly ast.mli
	menhir --dump --explain --strict --infer parser.mly
#	menhir --dump --explain --strict --infer tpParse.mly

ast.mli: ast.ml
	ocamlc -c ast.ml

clean:
	rm -rf  tp testLex *.o *.cmi *.cmo *.cmx *~ $(GENERATED) out.txt
