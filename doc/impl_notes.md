# Ficus Implementation Diary

## Ficus Roadmap

### the first steps:
- [x] initialize repo
- [x] prepare AST for the very compact C-style functional language (see the next section)
- [x] implement minimal lexer
- [x] implement minimal parser
- [x] implement minimal pretty printer
- [ ] support multiple modules (import and from-import directives)
- [ ] add intrinsic operations + very simple basic library
- [ ] implement minimal type checker
- [ ] K-normal form (call it IR?)
- [ ] K-normalization
- [ ] basic optimization of the form:
   - [ ] dead code elimination
   - [ ] tail recursion => loop substitution
   - [ ] inline function expansion
   - [ ] alpha/beta reduction, flattening (helper transformations)
   - [ ] const folding
   - [ ] (optionally) optimize out array index boundary checks during sequential array access
   - [ ] lambda lifting
- [ ] C-form
- [ ] C-form generation
- [ ] C-form optimization (if needed)
- [ ] emit C code
- [ ] basic runtime
- [ ] initial set of tests

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
