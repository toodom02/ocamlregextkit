all: regextkit.cma

regextkit.cma: lexer.cmo parser.cmo utils.cmo \
		ast.cmo re.cmo nfa.cmo dfa.cmo
		ocamlc -a $^ -o $@ 

parser.mli parser.ml: parser.mly
	ocamlyacc parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

clean: force
	rm -f *.cma *.cmo *.cmi *.o
	rm -f parser.mli parser.ml lexer.ml

ML = ast.ml ast.mli re.ml re.mli nfa.ml nfa.mli dfa.ml dfa.mli \
	lexer.ml parser.ml parser.mli utils.ml utils.mli

depend : $(ML) force
	(sed '/^###/q' Makefile; echo; ocamldep $(ML)) >new
	mv new Makefile

DOCS = ast.mli re.mli nfa.mli dfa.mli utils.mli

docs : force
	ocamldoc -html -sort -t "Regex Toolkit" -d ./docs -css-style ./style.css $(DOCS)

%.cmi : %.mli
	ocamlc -c $<

%.cmo : %.ml
	ocamlc -c $<

force:

###

ast.cmo : \
    parser.cmi \
    lexer.cmo \
    ast.cmi
ast.cmx : \
    parser.cmx \
    lexer.cmx \
    ast.cmi
ast.cmi : \
    re.cmi
dfa.cmo : \
    utils.cmi \
    nfa.cmi \
    dfa.cmi
dfa.cmx : \
    utils.cmx \
    nfa.cmx \
    dfa.cmi
dfa.cmi : \
    nfa.cmi
lexer.cmo : \
    parser.cmi
lexer.cmx : \
    parser.cmx
nfa.cmo : \
    utils.cmi \
    re.cmi \
    nfa.cmi
nfa.cmx : \
    utils.cmx \
    re.cmx \
    nfa.cmi
nfa.cmi : \
    re.cmi
parser.cmo : \
    re.cmi \
    parser.cmi
parser.cmx : \
    re.cmx \
    parser.cmi
parser.cmi : \
    re.cmi
re.cmo : \
    re.cmi
re.cmx : \
    re.cmi
re.cmi :
utils.cmo : \
    utils.cmi
utils.cmx : \
    utils.cmi
utils.cmi :
