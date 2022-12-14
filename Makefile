all: regextkit.cma

regextkit.cma: lexer.cmo parser.cmo utils.cmo \
		tree.cmo re.cmo nfa.cmo dfa.cmo
		ocamlc -a $^ -o $@ 

parser.mli parser.ml: parser.mly
	ocamlyacc parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

clean: force
	rm -f *.cma *.cmo *.cmi *.o
	rm -f parser.mli parser.ml lexer.ml

ML = tree.ml re.ml re.mli nfa.ml nfa.mli dfa.ml dfa.mli \
	lexer.ml parser.ml parser.mli utils.ml utils.mli

depend : $(ML) force
	(sed '/^###/q' Makefile; echo; ocamldep $(ML)) >new
	mv new Makefile

DOCS = tree.ml re.mli nfa.mli dfa.mli

docs : force
	ocamldoc -html -sort -t "Regex Toolkit" -d ./docs -css-style ./style.css $(DOCS)

%.cmi : %.mli
	ocamlc -c $<

%.cmo : %.ml
	ocamlc -c $<

force:

###

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
    tree.cmo \
    nfa.cmi
nfa.cmx : \
    utils.cmx \
    tree.cmx \
    nfa.cmi
nfa.cmi : \
    tree.cmo
parser.cmo : \
    tree.cmo \
    parser.cmi
parser.cmx : \
    tree.cmx \
    parser.cmi
parser.cmi : \
    tree.cmo
re.cmo : \
    tree.cmo \
    parser.cmi \
    lexer.cmo \
    re.cmi
re.cmx : \
    tree.cmx \
    parser.cmx \
    lexer.cmx \
    re.cmi
re.cmi : \
    tree.cmo
tree.cmo :
tree.cmx :
utils.cmo : \
    utils.cmi
utils.cmx : \
    utils.cmi
utils.cmi :
