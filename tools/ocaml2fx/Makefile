# Very simple Makefile for ficus (thanks to OCamlbuild)

.PHONY: all clean byte native profile debug sanity test

OCB_FLAGS = -pkgs str,unix
OCB = ocamlbuild $(OCB_FLAGS)

all: native # byte profile debug

clean:
	$(OCB) -clean
	@rm -f parser.ml
	@rm -f parser.mli
	@rm -f parser.output

native:
	$(OCB) ocaml2fx.native
	@cp ocaml2fx.native ocaml2fx

byte:
	$(OCB) ocaml2fx.byte

debug:
	$(OCB) -tag debug ocaml2fx.d.byte

test: native
	./ficus -C test fst.fcs
