# Ficus

**WIP**

This is a new functional language with the first-class array support
and _planned_ object-oriented features. `ficus` compiler generates
a portable C/C++ code out of .fx files.

## License

The code is distributed under Apache 2 license, see the [LICENSE](LICENSE)

## How to build

The compiler has been written in OCaml and needs `ocaml`
(including `ocamlyacc` and `ocamllex` utilities),
`ocamlbuild` and `make` utility to build it.
Tested with OCaml 4.07-4.10.
(In the near future it's planned to rewrite the compiler entirely in ficus)

```
cd <ficus_root>/src
make
./ficus -run -O3 ../examples/fst.fx # can also try other scripts in <ficus_root>/examples
```

## How to use

run `ficus --help` to get more complete up-to-date information about command line parameters

here is brief summary:
```
ficus [-c|-app|-run|...] [-O0|-O1|-O3] [-I<extra_module_path>] <scriptname.fx> [-- <script arg1> <script arg2> ...]
```

* `-c` generates C code and writes it to `<scriptname>.c`. Use `-o` option to override the output name.
* `-app` generates C code, stores it to temporary file and tries to compile it with C compiler (`cc`). Use `FICUS_CFLAGS` and `FICUS_LINK_LIBRARIES` environment variables to pass extra options to C compiler, e.g. `-ffast-math -mavx2` `-lmimalloc` etc. The compiled app is stored to `<scriptname>`. Override the name with `-o` option.
* `-run` generates application in a temporary file and runs it. After execution the application is deleted.

## Ficus 1.0

![TODO](/doc/ficus1.0.png)

## Credits

The compiler was inspired by min-caml
(http://esumii.github.io/min-caml/index-e.html) by Eijiro Sumii et al.
