# Ficus Implementation Diary

## Ficus Roadmap

### the first steps:
- [x] initialize repo
- [x] prepare AST for the very compact C-style functional language (see the next section)
- [x] implement minimal lexer
- [x] implement minimal parser
- [x] implement minimal pretty printer
- [x] support multiple modules (import and from-import directives)
- [x] add intrinsic operations + very simple basic library
- [x] implement minimal type checker
- [x] K-normal form (call it IR?)
- [x] K-normalization
- [ ] basic optimization of the form:
   - [x] dead code elimination
   - [x] tail recursion => loop substitution
   - [ ] inline function expansion
   - [x] alpha/beta reduction, flattening (helper transformations)
   - [x] const folding
   - [ ] (optionally) optimize out array index boundary checks during sequential array access
   - [ ] lambda lifting
- [ ] C-form
- [ ] C-form generation
- [ ] C-form optimization (if needed)
- [ ] emit C code
- [ ] basic runtime
- [ ] initial set of tests

### minimal set of features for the version **0.1**:
- [x] binary operations: +, -, *, /, %, **, &, |, ^, >>, <<, ==, !=, <=, >=, <, >, &&, ||, = (assign)
- [x] unary operations: +, -, ~, !, *, ref
- [x] val, var, fun (including lambda)
- [x] data structures: numbers (int, float, double, [u]int8|16|32|64), void, bool, char, string, tuples, records (?), arrays, lists, cptr
- [x] custom type definitions
- [x] exceptions
- [x] generic types
- [x] control flow ops: if, while, for + array comprehensions
- [x] tuple access, record access, array access
- [x] module import
- [x] inline c code
- [x] very basic standard library

### planned features for the version **1.0**:
- [x] variants
- [x] option type
- [x] pattern matching
- [x] array & list comprehensions
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
