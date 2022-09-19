all: regextkit

regextkit: ast.cmo lexer.cmo parser.cmo utils.cmo \
		nfa.cmo dfa.cmo print.cmo main.cmo
	ocamlc $^ -o $@

parser.mli parser.ml: parser.mly
	ocamlyacc parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

TESTSRC := $(wildcard test/*.test)

test : $(TESTSRC:test/%.test=test-%)

test-%: force
	@echo "*** Test $*.test"
	./regextkit $$(sed -n 1p test/$*.test) $$(sed -n 2p test/$*.test) > out.txt
	sed -n -e '1,/^(\*<</d' -e '/^>>\*)/q' -e p test/$*.test | diff - out.txt
	@echo "*** Passed"; echo

clean: force
	rm -f regextkit *.cma *.cmo *.cmi *.o
	rm -f parser.mli parser.ml lexer.ml out.txt

ML = ast.ml ast.mli dfa.ml dfa.mli nfa.ml nfa.mli print.ml print.mli \
	lexer.ml lexer.mli main.ml parser.ml parser.mli utils.ml utils.mli

depend : $(ML) force
	(sed '/^###/q' Makefile; echo; ocamldep $(ML)) >new
	mv new Makefile

%.cmi : %.mli
	ocamlc -c $<

%.cmo : %.ml
	ocamlc -c $<

force:

###

ast.cmo : \
    ast.cmi
ast.cmx : \
    ast.cmi
ast.cmi :
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
    parser.cmi \
    lexer.cmi
lexer.cmx : \
    parser.cmx \
    lexer.cmi
lexer.cmi : \
    parser.cmi
main.cmo : \
    print.cmi \
    parser.cmi \
    nfa.cmi \
    lexer.cmi \
    dfa.cmi \
    ast.cmi
main.cmx : \
    print.cmx \
    parser.cmx \
    nfa.cmx \
    lexer.cmx \
    dfa.cmx \
    ast.cmx
nfa.cmo : \
    utils.cmi \
    ast.cmi \
    nfa.cmi
nfa.cmx : \
    utils.cmx \
    ast.cmx \
    nfa.cmi
nfa.cmi : \
    ast.cmi
parser.cmo : \
    ast.cmi \
    parser.cmi
parser.cmx : \
    ast.cmx \
    parser.cmi
parser.cmi : \
    ast.cmi
print.cmo : \
    nfa.cmi \
    dfa.cmi \
    ast.cmi \
    print.cmi
print.cmx : \
    nfa.cmx \
    dfa.cmx \
    ast.cmx \
    print.cmi
print.cmi : \
    nfa.cmi \
    dfa.cmi \
    ast.cmi
utils.cmo : \
    utils.cmi
utils.cmx : \
    utils.cmi
utils.cmi :
