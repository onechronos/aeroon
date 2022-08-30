
all:
	dune build @all

clean:
	dune clean

WATCH?=@check
watch:
	dune build -w $(WATCH)

fmt:
	dune build @fmt --display=quiet --auto-promote