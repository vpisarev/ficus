# Ficus

**WIP**

This is a new functional language with the first-class array support
that also supports imperative and object-oriented programming paradigms.
`ficus` compiler generates a portable C/C++ code out of .fx files.

## License

The code is distributed under Apache 2 license, see the [LICENSE](LICENSE)

## How to build

The compiler is written in Ficus itself and needs C/C++ compiler and make utility.

```
cd <ficus_root>
make -j8
bin/ficus -run -O3 examples/fst.fx
```

You can add `<ficus_root>/bin` to the `PATH`. You can also customize ficus compiler behaviour by setting the following environment variables:

* `FICUS_PATH` can point to the standard library (`<ficus_root>/lib`), though ficus attempts to find the standard library even without `FICUS_PATH`. It can also contain other directories separated by `:` on Unix and `;` on Windows. The directories with imported modules can also be provided via one or more command-line options `-I <import_path>`. Note that if a compiled module imports other modules from the directory where it resides, that directory does not need to be explicitly specified.

* `FICUS_CFLAGS` is used by C/C++ compiler to build the produced .c/.cpp files. Alternative way to pass extra flags to C/C++ compiler is via `-cflags "<cflags>"` command-line option, e.g. `-cflags "-ffast-math -mavx2"`.

* `FICUS_LINK_LIBRARIES` contains the linker flags and the extra linked libraries. Alternative way to pass the extra linker flags to C/C++ compiler is via `-clibs "<clibs>"` command-line option.

## How to use

(run `ficus --help` to get more complete up-to-date information about command line parameters)

Here is a brief description with some most common options:
```
ficus [-app|-run|...] [-O0|-O1|-O3] [-verbose] [-I <extra_module_path>]* [-o <appname>] <scriptname.fx> [-- <script arg1> <script arg2> ...]
```

* `-app` (the flag is set by default) generates C code for the specified script as well as for the imported modules (one .c file per one .fx file), then runs the compiler for each of the generated .c files and then links the produced object files into the final app. The compiled app, as well as the intermediate `.c` and `.o` files, is stored in `__fxbuild__/<appname>/<appname>`. By default `<appname>==<scriptname>`. Override the app name (and the output path) with `-o` option.
* `-run` builds the app (see the flag `-app`) and then runs it. You can pass command-line parameters to the script after `--` separator.
* `-verbose` makes the compiler to report build progress and various information, which can be especially useful when building big apps

## Ficus 1.0

![TODO](/misc/ficus1.0.png)

(see https://github.com/vpisarev/ficus/issues/4 for the decryption and the status)

## Credits

The compiler was inspired by min-caml
(http://esumii.github.io/min-caml/index-e.html) by Eijiro Sumii et al.
