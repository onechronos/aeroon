
all:
	dune build @all

clean:
	dune clean

doc:
	dune build @doc

open-doc: doc
	open _build/default/_doc/_html/index.html

WATCH?=@check
watch:
	dune build -w $(WATCH)

fmt:
	dune build @fmt --display=quiet --auto-promote
