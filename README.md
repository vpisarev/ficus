# Ficus

**WIP**

This is a new functional language with the first-class array support
and _planned_ object-oriented features. `ficus` compiler generates
a portable C/C++ code out of .fx files.

## License

The code is distributed under Apache 2 license, see the [LICENSE](LICENSE)

## How to build

The compiler has been written in Ficus itself and needs C/C++ compiler and make utility.

```
cd <ficus_root>
make
FICUS_PATH=./lib FICUS_CFLAGS=-I./runtime ./ficus -run -O3 examples/fst.fx
```

`FICUS_PATH` and `FICUS_CFLAGS` are the two variables to setup to make ficus usable from any directory.

* The first one, `FICUS_PATH`, should be set to the path to the standard library (`<ficus_root>/lib`), but also can contain
other paths separated by `:` on Unix and `;` on Windows. Note that if a compiled module imports other modules
from the directory where it resides, that directory does not need to be included.

* The second one, `FICUS_CFLAGS`, is passed to C/C++ compiler to build the produced .c/.cpp files.
The generated files include ficus runtime headers, and the path to the runtime directory needs
to be specified via command line option `-cflags` or the environment variable.

## How to use

run `./ficus --help` to get more complete up-to-date information about command line parameters

here is brief summary:
```
./ficus [-app|-run|...] [-O0|-O1|-O3] [-I<extra_module_path>]* <scriptname.fx> [-- <script arg1> <script arg2> ...]
```

* `-app` (the flag is set by default) generates C code for the specified script as well as for the imported modules (one .c file per one .fx file), then run the compiler for each of the generated .c files and then link the produced object files into the final app. Use `FICUS_CFLAGS` and `FICUS_LINK_LIBRARIES` environment variables to pass extra options to C compiler, e.g. `-ffast-math -mavx2` `-lmimalloc` etc. The compiled app, as well as the intermediate `.c` and `.o` files, is stored in `__build__/<scriptname>/<scriptname>`. Override the output name with `-o` option.
* `-run` builds the app (see flags `-app`) and then runs it.

## Ficus 1.0

![TODO](/misc/ficus1.0.png)

(see https://github.com/vpisarev/ficus/issues/4 for the decryption and the status)

## Credits

The compiler was inspired by min-caml
(http://esumii.github.io/min-caml/index-e.html) by Eijiro Sumii et al.
