TUTO = robot
DUNE = opam exec -- dune


.PHONY: test gui check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop lib

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

garbage:
	OCAMLRUNPARAM=b dune exec training/train.exe

exe:
	$(DUNE) exec gui/$(TUTO).exe
.PHONY: exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f ngrams.zip
	zip -r ngrams.zip . -x@exclude.lst

clean:
	dune clean

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh
