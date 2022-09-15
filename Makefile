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

ML = ast.ml dfa.ml nfa.ml print.ml \
	lexer.ml main.ml parser.ml parser.mli

depend : $(ML) force
	(sed '/^###/q' Makefile; echo; ocamldep $(ML)) >new
	mv new Makefile

%.cmi : %.mli
	ocamlc -c $<

%.cmo : %.ml
	ocamlc -c $<

force:

###

ast.cmo :
ast.cmx :
dfa.cmo : nfa.cmo
dfa.cmx : nfa.cmx
lexer.cmo : parser.cmi
lexer.cmx : parser.cmx
main.cmo : parser.cmi nfa.cmo lexer.cmo dfa.cmo ast.cmo
main.cmx : parser.cmx nfa.cmx lexer.cmx dfa.cmx ast.cmx
nfa.cmo : ast.cmo
nfa.cmx : ast.cmx
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmx parser.cmi
print.cmo : nfa.cmo dfa.cmo ast.cmo
print.cmx : nfa.cmx dfa.cmx ast.cmx