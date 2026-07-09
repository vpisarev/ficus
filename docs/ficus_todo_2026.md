# Syntax and semantics

- [x] properly resolve instances of generic functions.
      Adjust the type checker not to stop at the first appropriate candidate.
- [ ] new syntax for generic types and its instances: `t list => list[t]`
  - [ ] Q: what would be a syntax for generic functions? `fun add[u, v, r](a: u, b: v): r {...}`?
- [ ] new syntax for fold: `fold acc = 0 for x <- arr {acc + x}` => `fold acc = 0 for x <- arr {acc += x}`
- [ ] add syntax to append elements to lists, vectors during fold?
      We already have runtime support for vector writers for vector comprehensions,
      maybe need to add list writer as well. E.g. `fold val l = [] for x <- 1:n {if isprime(n) {l+=x}}`.
      It partially duplicates list comprehensions, but fold is more universal.
      In fact, list comprehensions can then be converted to just fold's that are,
      in their turn, are converted to simple loops.
- [ ] because of new syntax for generic types, we can get rid of `(x :> new_type)` type cast syntax and switch to normal `new_type(x)`.
- [ ] currently we use Matlab-style .op for elemwise-operations, but it looks weird
      sometimes for those who are not familiar with Matlab. Shall we drop all .op
      operations and use Python-style `@` for matrix multiplication? We also use `@` for
      preprocessor in Ficus, but we should be able to differentiate between binary `@`,
      unary `@` in front of macros and macro interpolation `@{...}`.
- [x] (turned out to be a misdiagnosis: unescaped `f"{find("x")}"` always worked; only the C/Python-style
      escaped `f"{find(\"x\")}"` fails, with a misleading error. Locked by `basic.fstring_nested_literals`.)
      ~~not quite a new syntax: support string literals inside f-strings interpolations (see CLAUDE.md)~~
- [ ] revise records:
  - [ ] keep/drop/revise record update syntax?
        `var pt = Point {x=5, y=10}; pt .= {y = pt.y+5}`.
        actually, we can simply write `pt.y += 5`. The syntax above might be useful for
        non-destructive record update (`pt.{y = pt.y + 5}`). But is it really useful in
        practice?
  - [ ] when we have variant type with a record case, there is no way to pack data
        for the particular case: `type employee_t = Engineer: {age: int; computer: string; claude_subscr: string} | Manager: {age: int; tablet: string; team: list[employee_t]}`, how do we operate on all Engineer fields as a whole?
        It's suggested to automatically introduce `Engineer.t` type. It should be possible to
        construct employee_t directly from Engineer.t: `val e = Engineer(data: Engineer.t)`
- [ ] rename `half` to `fp16`, because `half` is quite generic name.
- [ ] add `bf16`.
- [ ] add more array reductions, e.g. `sum(for x <- arr {x**2}, 0.)`.
      Shall we somehow infer automatically the initial value, not to pass explicit `0.` as initializer?
- [ ] Add hygienic macros? Unlike functions, macros don't compute anything (except for `@{...}` parts),
      but rather their bodies are inserted as expressions into the program. In order to compute something
      inside the macro immediately, i.e. during compilation, `@{...}` can be used.
      ```
      // @for is a representation of `for p1 <- e1, ..., pn <- en {body}` statement parameterized by the body type.
      // It has .clauses (list of (pattern, expression) tuples) and .body (expression) members
      // @gensym(prefix) generates unique symbol starting with the specified prefix
      macro sum[t](for_: @for[t], ~init: t = t(0)):t {
         fold @gensym(s) = init for for_.clauses {s += for_.body}
      }

      macro EXPECT_EQ[t](a: t, b: t): void = EXPECT_EQ_(a, b, @string(a), @string(b), @file, @line) 
      ```
- [ ] get rid of `` `...` `` notation. See macros above.
- [ ] do we want a ternary selection operator? `(a ? b : c)` (we now use `if(a) {b} else {c}`)
- [ ] support variadic functions, e.g. `println(a, b, c)`; `max(a, b, c, d)`.
      Almost the same syntax as with variadic tuples, just without `()` around `...` or `t ...`.
      This variadic parameter should be the last non-keyword parameter.
      Also, it would be nice to add `tup.n...` notation to skip the first n arguments
      (here the new notation for generic types is used): 
      ```
      // we don't even need macros for that. variadic function max(args...) must be
      // more generic than max() with a fixed number of arguments (2)
      fun max[t](args: t ...): t {
          fold r = args.0 for x <- args.1... {r = max(r, x)}  
      }

      // variadic function print(args ..., ...) should be 'more generic'
      // than print(x) with a fixed number of arguments (1)
      fun print(args: ..., ~sep: string=", "): void {
          for x@i <- args {
              if i > 0 {print(sep)}
              print(x)
          }
      }

      fun println(args: ..., ~sep: string=", "): void {
          // pass all variadic args to another function
          print(args, sep=sep)
          print("\n")
      }
      ```
- [ ] remove restriction for modules to start with a capital letter; in fact, it's not a mandatory thing already,
      see `examples/fst.fx` that imports `testmod.fx`. But this rule should probably be propagated to std lib
- [ ] require that complex modules included `__init__.fx` in the root and any subdirectories,
      just like Python. otherwise it's impossible to differentiate between just a tree of
      files and complex hierarchical modules.
- [ ] support more than 5-dimensional arrays. Maybe 7-dimensional arrays?
- [ ] shall we add a syntax for saturating +,-,*,/ (including floating-point types)?
- [ ] syntax like `@parallel_if(condition) for {...}` to run loop as sequential if the problem is small enough.
      Compilers cannot always figure the bounary properly.
- [ ] implement `einsum` and support it at compiler level. Probably, it should be done in a generic way.
      there must be a certain class of special functions that compute all elements of the output array independently from each other.
      the functions should expose certain API. Then we can fuse those functions with subsequent element-wise operations (array comprehensions,
      where inputs array(s) are outputs of those special functions).
- [ ] support numpy-style broadcasting, e.g. [@broadcast for a <- A, b <- B {a*b}].
      It makes sense to explicitly specify that we need broadcasting, because in certain scenarios
      size mismatch is undesirable and is an error that must be reported (via exception).
- [ ] it would be nice to have automatically generated functions to serialize/deserialize
      arbitrary data structures with user-provided hooks for sub-structures.
      E.g. be able to represent any structure in json or yaml or python's pickle
      structure or a flatbuffer. We already have automatically generated functions to
      print structures, so that would be a further extension of it.
- [ ] add dynamically-typed `tensor` type? Currently we have array, which is statically typed and CPU-only (for now).
      In many cases static typing really helps, we get performance close to C/C++. In some cases static typing is inconvenient,
      e.g. for OpenCV bindings. Maybe we need to add `tensor` type that is a black box, sitting in CPU, GPU or NPU memory and
      there is a set of operations on it (with fusion etc.)
- [ ] try to accept `(op)` everywhere where `__opname__` is accepted, e.g. `Complex.(*)(a, b)`
- [ ] (maybe not) introduce infix `++` as concatenation operator. This should solve several problems with incorrect typing.

# Code generation, runtime
- [x] (we now put compiler modification date, its binary size and the compiler flags as the 'signature')
      put compiler version (git commit?) into `__fxbuild__/<something>` directories,
      so that after compiler is updated, all the previously generated .c files are discarded.
- [ ] we now use compact rpmalloc. Shall we replace it with bigger, but hopefully better supported mimalloc?
- [ ] we use atomic reference counting, and many of ficus data key structures are immutable or have immutable headers,
      but still multi-threaded program may crash, e.g. when two different threads are writing into the same mutable location,
      e.g. array consisting of arrays, e.g. float [,] [,] (2D array of 2D arrays of floats):
      `for k <- 0:100 { val i=rng.uniform(0, n), j = rng.uniform(0, n); arr[i, j] = array((rng.iniform(1, 10), rng.uniform(1, 10)), rng.uniform(-1.f, 1.f))}`.
      That is, 'complex' mutable fields should probably be written in transaction-style way, even if it's slower.
      For general number cranching it should not affect speed (maybe do something like
      '"Cache-Sensitive Software Transactional Memory" by Robert Ennals'?)

# Convenience stuff:

- [ ] better diagnostic of errors. This is too generic request, some details are needed.
  - [ ] for example, when a reserved name/keyword is used as identifier
- [ ] more convenient error messages, similar to gcc/clang or other compilers when we display code
      line and put `^` mark below it where the error occured
- [x] (added `tools/update_compiler.py`) regenerate bootstrap sources with a special compiler key or
      at least some python script (tools/update_compiler.py?)
- [ ] compiler as a library? Currently compiler uses many global variables.
      It would be nice to be able to create a compiler instance and make some experiments with it
      (build AST, K-form, generate final .c code etc.)

# Optimizations:

- [ ] instantiate (not necessarily generic) functions that take callbacks known at compile time.
      The most classic example is qsort — if we pass known comparison function to it,
      we can create a copy of qsort with that function inlined.
- [ ] compile ficus compiler itself and all programs by default with `-Ofast`.
      Currently, it's broken for some reason
- [ ] extend constant folding to support math functions
- [ ] generalization of the previous part: can we include some simple k-form interpreter for a subset of
      language to const-fold expressions that use user functions, including recursive functions operating on
      immutable data structures, scalars, calling intrinsics etc?
