all: regextkit

regextkit: ast.cmo lexer.cmo parser.cmo nfa.cmo dfa.cmo \
		print.cmo main.cmo
	ocamlc $^ -o $@

parser.mli parser.ml: parser.mly
	ocamlyacc parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

realclean: clean

clean: force
	rm -f regextkit *.cma *.cmo *.cmi *.o
	rm -f parser.mli parser.ml lexer.ml

ML = ast.ml ast.mli dfa.ml dfa.mli nfa.ml nfa.mli print.ml print.mli \
	lexer.ml lexer.mli main.ml parser.ml parser.mli

depend : $(ML) force
	(sed '/^###/q' Makefile; echo; ocamldep $(ML)) >new
	mv new Makefile

%.cmi : %.mli
	ocamlc -c $<

%.cmo : %.ml
	ocamlc -c $<

force:

###

ast.cmo : ast.cmi
ast.cmx : ast.cmi
dfa.cmo : nfa.cmi dfa.cmi
dfa.cmx : nfa.cmx dfa.cmi
lexer.cmo : parser.cmi lexer.cmi
lexer.cmx : parser.cmx lexer.cmi
main.cmo : parser.cmi nfa.cmi lexer.cmi dfa.cmi ast.cmi
main.cmx : parser.cmx nfa.cmx lexer.cmx dfa.cmx ast.cmx
nfa.cmo : ast.cmi nfa.cmi
nfa.cmx : ast.cmx nfa.cmi
parser.cmo : ast.cmi parser.cmi
parser.cmx : ast.cmx parser.cmi
print.cmo : nfa.cmi dfa.cmi ast.cmi print.cmi
print.cmx : nfa.cmx dfa.cmx ast.cmx print.cmi