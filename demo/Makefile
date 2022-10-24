all: compregex

compregex: compregex.cmo
		ocamlc ../regextkit.cma $^ -o $@

TESTSRC := $(wildcard test/*.test)

test : $(TESTSRC:test/%.test=test-%)
	@echo "*** All Tests Passed"
	@rm -f out.txt

test-%: force
	@echo "*** Test $*.test"
	./compregex -O $$(sed -n 1p test/$*.test) $$(sed -n 2p test/$*.test) > out.txt
	sed -n -e '1,/^(\*<</d' -e '/^>>\*)/q' -e p test/$*.test | diff - out.txt
	@echo "*** Passed"; echo

clean: force
	rm -f compregex *.cma *.cmo *.cmi *.o out.txt

%.cmi : %.mli
	ocamlc $(MLFLAGS) -c $<

%.cmo : %.ml
	ocamlc $(MLFLAGS) -c $<

force:

MLFLAGS = -I ..