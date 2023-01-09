all: 

profile:
		rm -f out.txt
		for number in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20; do \
			dune exec --no-print-directory -- ./profile.exe -a $$number >> out.txt 2>&1; \
		done

test: 
		dune exec ./test.exe > out.csv

clean:
		rm -f out.txt out.csv
