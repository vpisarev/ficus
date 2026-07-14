# Working in the Ficus repo

Ficus is a self-hosted functional language (arrays first-class; also imperative
& OOP) that compiles to portable C/C++. The compiler is written in Ficus and
bootstraps itself.

## Build & run

```sh
make -j8                                  # builds bin/ficus
bin/ficus -run file.fx -- arg1 arg2       # compile + run
bin/ficus -run -O0 -no-openmp -B DIR f.fx # opt level, no OpenMP, build root
bin/ficus -no-c file.fx                   # parse + typecheck only (fast)
bin/ficus -o name file.fx                 # binary at ./name (rm it after),
                                          # intermediates in __fxbuild__/name/
```

- `bin/ficus` finds the stdlib on its own; `-B <dir>` must already exist
  (`mkdir -p` first, or you get a misleading "failed to write some k-forms").
- Diagnostics go to **stdout**, exit code 1. `-run` hides a runtime SIGSEGV as
  exit 1 — to see a real crash, build with `-o` and run the binary directly.
- IR dumps (all work with `-no-c`): `-pr-ast`, `-pr-k0`, `-pr-k` (with `-O3`),
  `-pr-resolve` (overload-resolution trace). Dumps contain every imported
  module; the file under test is the **last** section. Gensyms: `name@1234`.
- `$CC`/`$CXX` pick the C compiler (default `cc`). macOS OpenMP is bundled at
  `runtime/lib/macos_arm64/libomp.a`.
- Build dirs self-invalidate (`.fxstamp`), and fxtest wipes `build/fxtest` on a
  compiler change — no manual `rm -rf` between measurements is ever needed.

**Bootstrap regen** — after editing any `compiler/*.fx` OR any of the 54
bootstrap stdlib modules (`ls compiler/bootstrap/` when in doubt): run
`python3 tools/update_compiler.py`; it copies only changed modules and asserts
the self-hosting **fixpoint** (non-zero exit = determinism regression).
`--check` = dry run; CI runs it on every push touching compiler/lib/runtime —
a stale bootstrap is a red master. This is the only routine self-build of the
compiler, so it alone catches stdlib changes that break compiling the compiler.
Compile-time-only record layout changes (`options_t`, `scope_t`) legitimately
regenerate many bootstrap modules; generated C for *programs* must not change.

## Testing — the fxtest ladder (tools/fxtest/README.md)

```sh
python3 tools/fxtest/fxtest.py all        # unit + negative + ir + cfold + corpus(O0/O3)
bin/ficus -run test/test_all.fx           # the UTest suite directly
```

Layers: T2 corpus differential (O0 vs O3), T3 golden diagnostics
(`test/negative/`), T4 IR snapshots (`test/ir/`), T5 randomized suites
(`test/rand/`). Plus `fxtest.py determinism` and `sanitize` (ASan+UBSan).
Harness is Python-3-stdlib-only. Compiler bugs found while on another task are
recorded and fenced (not fixed) in `docs/found_bugs.md`.

**Diagnostics (diag-1).** The type checker recovers and reports MANY errors
per run: a failed definition/arm/branch is reported, its symbol poisoned with
`TypErr` (the declared type when annotated — the annotation firewall), and
`TypErr` operands suppress cascades structurally (`val x = undef; x+1` = ONE
diagnostic). Output: sorted by `(module,line,col)`, primary-line dedup,
`-fmax-errors=N` cap (default 100), Levenshtein `did you mean 'x'?` for
unknown names. Frontend diagnostics (lexer/parser/typecheck/K-normalization,
gated by `Ast.compiler_stage`) carry a gcc-style caret excerpt; middle/backend
ones don't (locations drift). **Every `test/negative/` golden includes the
excerpt**; the T3 harness compares excerpt lines verbatim — use
`--update-golden` when adding cases. New legality checks belong in typecheck
(caret + `-no-c`-visible), not C-gen.

**Spans (reform-prep-1).** `loc_t` is a true span now, not a point: the lexer
returns each batch's `(begin, end)` and the parser stamps every token with it,
so a caret shows the token width (`^~~~`) and `loclist2loc` folds real node
spans. **Gotcha (FB-020): `typ_t` carries NO source location**.

## Writing Ficus (.fx) — gotchas

Ficus is underrepresented in training data; don't improvise from OCaml/Rust
intuition — read `doc/ficustut.md` and existing code. Verified traps:

- Reserved words can't be identifiers: `ref`, `nan`, `nanf`, `null` (+ the
  obvious keywords). Misuse gives a confusing "pattern is expected" error.
- **Generics (generics-1) are bracketed & prefix, params UPPERCASE and
  DECLARED**: `list[T]`, `Map.t[K, V]`, `ref[T]`, `T?` (option); `type
  tree_t[K, D] = …`, `fun add[U, V, R](a: U, b: V): R`, `operator ==[T](…)`,
  `class t[K, D]`. The old `'t list` / `('k,'d) map` / implicit-`'t` forms were
  REMOVED (parser rejects them with a "declare type parameters" / "postfix type
  application was removed" hint). Arrays stay POSTFIX — only the quote drops:
  `T [+]`, `T [,]`, `T []`. `list[A, B]` == `list[(A, B)]` (types don't overload
  on arity → args fold to a tuple). No expression-position `f[T](x)` yet — infer
  from args or annotate: `(Empty : Map.t[string, int])`. The `'`-prefix survives
  ONLY for compiler-internal auto-generated tyvars (`'targ` for untyped params,
  `__var_tuple__`); you never write it. Details: `docs/generics1_report.md`.
- Casts parenthesize *with* the operand: `(x :> uint8)`, chained
  `((x :> uint32) :> int32)`. Simple types also work functionally: `uint32(x)`.
- `println` takes ONE argument: `println(f"{a} {b}")` / `println((a, b))`.
- f-string `{}` interpolation: nested string literals go UNESCAPED —
  `f"{find("x")}"` works; the escaped `\"` spelling fails misleadingly.
- Record *type* fields use `;`; *construction* uses `,` (`pt_t {x=1, y=2}`).
- Small unsigned ints promote to `int` (`200u8 + 100u8 == 300`); wrap
  explicitly: `((a + b) :> uint8)`. `uint64` wraps natively.
- **16-bit floats (types-1):** `fp16` (IEEE half; the old name `half` is a
  stdlib alias) and `bf16` (bfloat16). Both are storage-only — arithmetic
  promotes to `float32`, so `fp16(x)`/`bf16(x)` convert and `float(v)` reads
  back. Literals: `1.5h` = fp16, `1.5bf` = bf16. `bf16` is `TypFloat(17)`
  internally (`Ast.BF16`; fp16 is the real 16) — match the literal `17`, never
  `BF16`, in patterns. Runtime f32→bf16 is fast round-half-up, and constant
  folding does NOT model 16-bit rounding (so `float(bf16(const))` shows the
  unrounded value — use a runtime value to observe real rounding). `fp32`/`fp64`
  exist as aliases for `float`/`double`.
- `/` truncates toward zero; `%` takes the sign of the dividend
  (`-17 % 5 == -2`). `int(x)` truncates. `floor`/`ceil`/`trunc`/`round`
  return `int` **by design** (array forms: `int [+]`).
- Border access: mode before the bracket — `a.clip[i]`, `a.wrap[i]`,
  `a.zero[i]` (arrays, Vector, strings).
- Comprehensions: 1D `[for i <- 0:n {..}]`; 2D `[for i <- 0:m for j <- 0:n
  {..}]`; zip is comma; index binding `for x@i <- a` / `for x@(i,j) <- m`.
  Lists: `[:: 1, 2, 3]`, cons `::`, list-comp `[:: for x <- l {..}]`.
- **fold (fold-1 reform)** is now imperative: `fold acc=init for x <- a {acc +=
  x}` desugars to `{ var acc=init; for x <- a {..}; acc }`. The accumulator is a
  real mutable **var**, updated in the body — either ASSIGNED (`acc = f(acc,x)` /
  `acc += x`) or, for a reference-mutable accumulator, mutated in place
  (`fold r=vector() for v <- vs {r.append(v)}`). Yielding the accumulator as the
  tail value (the OLD form, `fold s=0 for x {s+x}`) is now a **type error** (the
  void body has nowhere to put the value) — there is no separate "never assigned"
  warning. Multiple accumulators: `fold a=0, b=1 for ..` (each its own
  var; a tuple pattern also works). `break`/`continue`/`return` are legal in the
  body (it is a plain `for`). Simultaneous tuple assignment `(a,b)=(b,a+b)` is a
  language feature (parse-time desugar via a temp; the Fibonacci/swap idiom).
  Named reduction sugars (`all`/`exists`/`count`/`find`/`filter`/`vector`,
  spelled `name(for ...)`) are a SEPARATE construct, unchanged by the reform.
- Strings are UTF-32: `s.length()` counts chars, `s[i]` is a `char`,
  `s[i:j]` a substring; `string([for c <- cs {c}])` builds from chars.
- Use `String.fx` / `Re.fx` instead of hand-rolling string processing.
  `String.fx` is Python-like string processing. `Re.fx` is regexp engine,
  more limited than Perl or Python, but containing enough good stuff, see `test/test_re.fx`.
- `array(n, init)` / `array((m, n), init)`; ranges `a:b`, `a:b:step`; slices
  `a[i:j]`, `a[::-1]`. `Sys.getenv(name, defval)`; `s.to_int(): int?`.
- A `match` arm body may be a block: `| pat => val r = ...; expr`.
- `break`/`continue`/`return` (bare or with a value) are legal as a
  `match`-arm / `if`-branch value, exactly like `throw` (pseudo-type `TypErr`
  unifies with valued siblings; lowering unchanged, cleanup runs). Legality
  is positional-agnostic: they need an enclosing loop; legal inside a `fold`
  body (post-fold-1 it is a plain `for`), rejected inside `@parallel` bodies;
  bare `return` in a non-void function is a return-type mismatch.
- Shift count must be `int`: write `x >> int(n)`.
- **Overload resolution: the least-generic viable candidate wins**, regardless
  of declaration/import order. No unique winner at a fully-determined call =
  ambiguity error; disambiguate with `Module.__mul__(a, b)` (`Module.(*)`
  doesn't parse yet). Free type vars in the argument types → tie falls back
  to first-match; don't rely on it. Exact keywordless match beats an
  all-defaulted-keywords candidate.
- `[]` is "some collection" (list/vector/array): `val n: int = []` is a
  typecheck error; unpinned kind → K-normalization asks for an annotation.
- **Return annotations are viability filters**: an inferred (free) return
  unifies with any expected type and can steal under-constrained sites.
  Generic operators annotate with a FRESH var (`: complex[T3]`, `: T3 [+]`,
  `: (T3 ...)`); enforced by `tools/lint_op_returns.py lib` (CI).
  `-Wimplicit-rettype` warns on module-level functions with inferred returns
  (user modules by default; `=all` adds stdlib — self-gated by
  `tools/rettype_gate.sh`, scope excludes NN/Onnx/Protobuf/OpenCV; keep it
  clean). `-Wall` umbrella; `-Werror` promotes. Annotations erase in K-form.
  Compiler sources not gated yet (annotate-3, ~340).
- Annotation spellings: nullary fn type is `(void -> T)` not `(() -> T)`; a
  module's own type parses bare or qualified; uniform tuple `(int...)`;
  any-dims array `T [+]`; input-dependent widened scalar → fresh `: T3`.
- Comparing `-pr-resolve` censuses: gate on the (name | winner | outcome)
  multiset, not raw text (line:col shifts and expected-type sharpening are
  noise).

## Measurement traps

- Signed overflow **wraps** (`-fwrapv` everywhere, folder matches). Don't rely
  on UB; don't drop the flag.
- UB signature at -O2/-O3: a value *prints* right but a branch on it goes
  wrong → the C compiler exploited UB. Repro in pure C; `-fwrapv`/
  `-fno-strict-*` to confirm.
- ASan+UBSan: `fxtest.py sanitize` (or the -cflags/-clibs sanitize combo).
  Over-reads into valid adjacent heap may NOT trap — reference-checked suites
  are the complement.
- IR snapshot fails on CI only, `:ast` differs but `:k0`/`:k` match → suspect
  the harness's header extraction (path-length wrap), not the compiler.

**Iterate tiny**: 10–20 lines → `bin/ficus -run f.fx` → fix → extend. Compiler
errors are ground truth; if it rejects something the tutorial calls legal,
minimal repro into `docs/found_bugs.md` and route around (don't fix
`compiler/` unless asked).

**Compiler code style — functional over imperative.** The house style is
accumulator recursion / immutable bindings / `match`: fewer side effects,
safer under the atomic-refcount runtime. An imperative rewrite is not an
improvement by default (`break`/`continue` are checked in each block's cleanup
section — often micro-slower). Mirror the surrounding module.

Test-file naming: a test `.fx` must not collide case-insensitively with a
stdlib module (`char.fx` shadows `Char`); numeric prefixes are safe. Subdir
modules import as `Dir.Module`; same-dir siblings by bare name.

## Environment notes

- macOS python is 3.9 (no `tomllib` — the harness ships a fallback parser).
- macOS `sed` is BSD (no `\b`) — use `perl -pe` for word-boundary edits.
