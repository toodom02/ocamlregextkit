all: 
	dune build

doc:
	dune build @doc
	rm -rf ./docs
	mv -v ./_build/default/_doc/_html ./docs

clean:
	dune clean
