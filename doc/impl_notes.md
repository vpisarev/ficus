# Ficus Implementation Diary

## Ficus Intro

Ficus is supposed to be as simple as possible (but not simpler) functional language with all (or most of) modern features that should make it suitable for intensive processing of numeric arrays or hierarhical tree-like data structures. In other words, for AI.

Let's overview the currently available solutions for AI apps. For example, consider *Ocaml*. It's rather elegant language that is used for some very critical projects. It demonstrates very good performance on hierarchical data processing, thanks to its static typing, efficient compiler and very efficient memory manager. We want something similar, but more modern. *Kotlin* and *Swift* can be used as examples of such modern functional or semi-functional languages that use more conventional syntax and modern technologies. So, we want Ficus to be similar to them as well. Then the obvious question arises - why not just take one of those languages? The answer is that the languages provide many of the desured features, but not all of them, and in general they are quite similar (here is a very nice comparison: http://nilhcem.com/swift-is-like-kotlin/) and mostly suited for implementation of normal apps, whereas Ficus' primary focus will be on data crunching, top performance and compatibility with C/C++; basically, Ficus project has quite an ambitious goal to replace C/C++ and Python for AI apps, where the 2 languages dominate currently.

Here is brief comparison of the languages (where in the Ficus column we put the planned but not yet implemented features :) ):

|   | Ficus  | Kotlin  | Swift  | Ocaml | F#  | C++ | Python |
|---|---|---|---|---|---|--|--|
| syntax  | C-style | C-style | C-style  | ML-style  | ML-style  | C-style | Python-style |
| paradigms | functional, imperative, OOP | funcional, imperative, OOP | functional, imperative, OOP | functional, imperative, OOP | functional, imperative, OOP | imperative, OOP, partly functional | functional, imperative, OOP |
| multi-threading  | +  | +  | +  | –  | + | + | ± (GIL) |
| memory management  | RC  | GC  | RC | GC  | GC  | manual | RC |
| safe | + | + | + | + | + | – | + (but heavily relies on runtime checks) |
| [projected] performance of array processing algorithms | excellent | good | good | good with int's, fair with float's | good | excellent | poor (needs C++ kernels for good speed) |
| interface to C | embedded | + | + | possible | possible | embedded | possible |
| real multi-dimensional arrays | + | nested 1D arrays | nested 1D arrays | nested 1D arrays | nested 1D arrays | via 3d-party libs | + (numpy) |
| runtime footprint | tiny | JVM (or LLVM - w.i.p) | big, but smaller than JVM | small | .NET (big) | normally small; depends on the framework used | relatively small |
| std lib | looong way to go, but easy to connect 3rd-party C/C++ libs | excellent on JVM | excellent on Apple platforms; v. good on others | very good | excellent on MS platforms | v. good + many 3rd-party libs | excellent + many 3rd-party libs |
| numerical lib | looong way to go, but some basic algorithms are embedded into the language or are very easy to implement | + (Koma) | + (w.i.p, fast ai etc.) | ± | + | OpenCV and such | fantastic! (numpy, scipy + many ML/AI frameworks) |
| implemented in itself? | Yes (for now we are using Ocaml) | Yes | No (C++) | Yes | No (C#) | Yes | No (CPython is implemented in C) |
| REPL (interactive mode) | – (planned in 2.0) | – | + | + | + | – | + |

As you can see, even though Ficus is very far from other mature languages, it's indeed designed to replace C/C++ as a language of choice for data chrunching algorithms.

## Ficus Roadmap

### the first steps:
- [ ] initialize repo
- [ ] prepare AST for the very compact C-style functional language (see the next section)
- [ ] implement minimal lexer
- [ ] implement minimal parser
- [ ] implement minimal pretty printer
- [ ] add intrinsic operations + very simple basic library
- [ ] implement minimal type checker
- [ ] K-normal form (call it IR?)
- [ ] K-normalization
- [ ] basic optimization of the form: dead code elimination, tail recursion => loop, inline expansion, alpha/beta reduction, const folding
- [ ] C-form
- [ ] C-form generation
- [ ] C-form optimization (if needed)
- [ ] emit C code
- [ ] basic runtime

### minimal set of features for the version **0.1**:
- [ ] binary operations: +, -, *, /, %, **, &, |, ^, >>, <<, ==, !=, <=, >=, <, >, &&, ||, = (assign)
- [ ] unary operations: +, -, ~, !, *, ref
- [ ] val, var, fun (including lambda)
- [ ] data structures: numbers (int, float, double, [u]int8|16|32|64), void, bool, char, string, tuples, records (?), arrays, lists, cptr
- [ ] custom type definitions
- [ ] (?) exceptions (maybe postpone it?)
- [ ] (?) generic types (postpone it?)
- [ ] control flow ops: if, while, for (no comprehensions so far)
- [ ] tuple access, record access, array access (with ranges?)
- [ ] module import
- [ ] inline c code
- [ ] very basic standard library

### planned features for the version **1.0**:
- [ ] variants
- [ ] option type
- [ ] pattern matching
- [ ] array & list comprehensions
- [ ] array initializations
- [ ] automatically generated operations (==, string(), ...)
- [ ] parallel loops
- [ ] efficient thread-safe runtime using atomic operations when necessary
- [ ] good Unicode support (comments, string literals, string processing functions, conversion to/from UTF8, file I/O)
- [ ] classes
- [ ] interfaces (could be postponed till v1.1)
- [ ] separate compilation, let people build libs, not just apps (could be postponed till v1.1)
- [ ] more versatile standard library (can partly be postponed till v1.x)
- [ ] self-bootstraping implementation of Ficus compiler in Ficus (could be postponed till v1.x)
