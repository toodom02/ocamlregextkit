all:
	dune build ./demo.exe

TESTSRC := $(wildcard test/*.test)

test : $(TESTSRC:test/%.test=test-%)
	@echo "*** All Tests Passed"
	@rm -f out.txt

test-%: force
	@echo "*** Test $*.test"
	dune exec --no-print-directory ./demo.exe $$(sed -n 1p test/$*.test) $$(sed -n 2p test/$*.test) > out.txt
	sed -n -e '1,/^(\*<</d' -e '/^>>\*)/q' -e p test/$*.test | diff - out.txt
	@echo "*** Passed"; echo

clean: force
	rm -f out.txt

force: