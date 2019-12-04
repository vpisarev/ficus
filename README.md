# Ficus

This is a new functional language with the first-class array support
and _planned_ object-oriented features. `ficus` compiler generates
a portable C/C++ code out of .fx files.

The code is distributed under Apache 2 license, see the [LICENSE](LICENSE)

The compiler has been written in OCaml and needs `ocaml`
(including `ocamlyacc` and `ocamllex` utilities),
`ocamlbuild` and `make` utility to build it.
In the near future it's planned to rewrite the compiler entirely in ficus.

The compiler was inspired by min-caml
(http://esumii.github.io/min-caml/index-e.html) by Eijiro Sumii et al.
