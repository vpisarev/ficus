# Very simple Makefile for ficus (thanks to OCamlbuild)

.PHONY: all clean byte native profile debug sanity test

OCB_FLAGS = -pkgs str
OCB = ocamlbuild $(OCB_FLAGS)

all: native # byte profile debug

clean:
	$(OCB) -clean

native:
	$(OCB) ficus.native
	@cp ficus.native ficus

byte:
	$(OCB) ficus.byte

profile:
	$(OCB) -tag profile ficus.native
	@cp ficus.native ficus

debug:
	$(OCB) -tag debug ficus.byte

test: native
	./ficus -C test fst.fcs
