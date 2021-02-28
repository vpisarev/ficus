# Very simple Makefile for ficus (thanks to OCamlbuild)

.PHONY: all clean byte native profile debug sanity test

OCB_FLAGS = -pkgs str,unix
OCB = ocamlbuild $(OCB_FLAGS)
FICUS_GIT := $(shell git rev-parse HEAD | cut -c 1-8)

all: native # byte profile debug

clean:
	$(OCB) -clean
	@rm -f parser.ml
	@rm -f parser.mli
	@rm -f parser.output
	@rm -f config.ml
	@rm -f *.pgm
	@rm -f *.c

native:
	@echo 'let git_commit = "$(FICUS_GIT)"' > config.ml
	$(OCB) ficus.native
	@cp ficus.native ficus

byte:
	@echo 'let git_commit = "$(FICUS_GIT)"' > config.ml
	$(OCB) ficus.byte

debug:
	@echo 'let git_commit = "$(FICUS_GIT)"' > config.ml
	$(OCB) -tag debug ficus.d.byte

test: native
	./ficus -run ../test/test_all.fx
