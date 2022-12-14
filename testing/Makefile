all: 

profile: profile.cmo
		ocamlfind ocamlc ../regextkit.cma $^ -o $@ -package landmarks -linkpkg
		rm -f out.txt
		for number in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20; do \
			./profile -a $$number >> out.txt 2>&1; \
		done

test: test.cmo
		ocamlc ../regextkit.cma $^ -o $@
		./test > test.csv

clean: force
	rm -f profile test *.cma *.cmo *.cmi *.o out.txt

%.cmo : %.ml
	ocamlfind ocamlc -I .. -package landmarks -linkpkg -c $< 

force:
