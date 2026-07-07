*NOTE: the previous [big ticket](https://github.com/vpisarev/ficus/issues/4) was renamed and closed.*

Now we have some more things to be done to finally release Ficus 1.0 (or at least Ficus 1.0 beta):

- [ ] proper stack trace reporting in the case of exceptions, asserts etc. The stack trace should report the original filenames & lines in the original ficus code, not in the generated .c code
- [ ] option to generate shared lib (or static lib), not just executable (the only available option for now). In the case of shared lib probably it should be option to compile ficus runtime as a separate shared library, in order to use several ficus-generated modules together in the same final app.
- [ ] as a part of the previous item, provide some method to export clean C and/or C++ API instead of ugly-looking  mangled names.
- [ ] (lower priority, but still important) option to generate binary python module
- [ ] (lower priority) dynamic loading of ficus-generated shared libraries
- [ ] automatically define platform-specific symbols that can be checked with preprocessor, like OS, CPU_ARCH etc.
- [ ] proper handling of complex modules. To avoid ambiguity, probably there should be `__init__.fx` in each ficus module directory (including subdirectories)
- [ ] add pragma `pragma "framework:<framework>"` to link Apple frameworks easily without using `-clibs` flag.
- [ ] add pragma `pragma "pkg:<pkg>"` to link packages via pkg-config more easily without using `-cflags` and `-clibs`
- [ ] partial implementation of LSP (Language Service Protocol): syntax highlighting, including inline C/C++ code, folding, jump to symbol definition, intellisense.
- [ ] (quite difficult to do) some debugger support, probably we should try to keep the original local variables' names as much as possible and provide helper scripts for GDB to visualize arrays, lists, variants etc.
- [ ] better support for records and variants with cases with record attributes; maybe if we have a variant case `SomeCase: {...}`, there should be automatically introduced `SomeCase.t` as type for the attribute record. Or some other way to pack and move data for the particular case.
- [x] prepare proper string interpolation and the corresponding `string()` and `print()` with complete support for Python 3 format.
- [ ] automatic printing of variants
- [ ] fix bug when some type declarations are thrown away.
- [ ] better support for exceptions at C/C++ level, especially exceptions with some attributes, at least simple attributes (integers, strings).
- [ ] check once again if `return` is properly implemented, maybe not
- [ ] (much) more unit tests
- [ ] some threading without openmp? we will definitely need `parallel for` & `@sync`. What about tasks and asynchronous execution?
- [ ] round-up the standard library #5
- [ ] full-scale "tensor" data type for nd dense arrays, which type is unknown at compile time. That will help to wrap OpenCV better, implement something like numpy and also improve implementation of deep learning inference engine. Also, tensor could be stored on an acceleration device (GPU, NPU), which is a critical feature.
- [ ] implement various scalar intrinsics (partially done): round(), floor(), sin(), cos(), popcount() ...
- [ ] maybe not for 1.0, but it would be nice to provide some introspection and meta-compiler features using something like Python decorators. Probably for that we would need a full-scale JIT