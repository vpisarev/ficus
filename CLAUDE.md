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
- Build dirs self-invalidate (`.fxstamp`: compiler identity + codegen options),
  and fxtest wipes `build/fxtest` when `bin/ficus` changes — no manual `rm -rf`
  between measurements is ever needed.

**Bootstrap regen** — after editing any `compiler/*.fx` OR any of the 54
bootstrap stdlib modules (`ls compiler/bootstrap/` when in doubt): run
`python3 tools/update_compiler.py`. It regenerates `compiler/bootstrap/*.c`,
copies only changed modules, and asserts the self-hosting **fixpoint**
(non-zero exit = determinism regression). `--check` = CI dry run (runs on
every push touching compiler/lib/runtime — a stale bootstrap is a red master).
This is the only routine self-build of the compiler, so it also catches stdlib
changes that break compiling the compiler; `fxtest all` cannot.

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

## Writing Ficus (.fx) — gotchas

Ficus is underrepresented in training data; don't improvise from OCaml/Rust
intuition — read `doc/ficustut.md` and existing code. Verified traps:

- Reserved words can't be identifiers: `ref`, `nan`, `nanf`, `null` (+ the
  obvious keywords). Misuse gives a confusing "pattern is expected" error.
- Casts parenthesize *with* the operand: `(x :> uint8)`, chained
  `((x :> uint32) :> int32)`. Simple types also work functionally: `uint32(x)`.
- `println` takes ONE argument: `println(f"{a} {b}")` / `println((a, b))`.
- f-string `{}` interpolation: write nested string literals UNESCAPED —
  `f"{find("x")}"` works; the escaped `f"{find(\"x\")}"` fails with a
  misleading "braces are not closed".
- Record *type* fields use `;` (`{x: int; y: int}`); *construction* uses `,`
  (`pt_t {x=1, y=2}`).
- Small unsigned ints promote to `int` for arithmetic (`200u8 + 100u8 == 300`);
  wrap explicitly: `((a + b) :> uint8)`. `uint64` wraps natively.
- Integer `/` truncates toward zero; `%` takes the sign of the dividend
  (`-17 % 5 == -2`). `int(x)` truncates. `floor`/`ceil`/`trunc`/`round`
  return `int` **by design** (array forms: `int [+]`).
- Border access: mode before the bracket — `a.clip[i]`, `a.wrap[i]`,
  `a.zero[i]` (arrays, Vector, strings).
- Comprehensions: 1D `[for i <- 0:n {..}]`; 2D `[for i <- 0:m for j <- 0:n
  {..}]`; zip is comma `for x <- a, y <- b`; index binding `for x@i <- a` /
  `for x@(i,j) <- m`; fold `fold acc=init for x <- a {..}`. Lists:
  `[:: 1, 2, 3]`, cons `::`, list-comp `[:: for x <- l {..}]`.
- Strings are UTF-32: `s.length()` counts chars, `s[i]` is a `char`,
  `s[i:j]` a substring; `string([for c <- cs {c}])` builds from chars.
- Use `String.fx` / `Re.fx` instead of hand-rolling string processing
  (`find/replace/split/join/strip/...`; `Re.compile` + `replace/findall/...`).
- `array(n, init)` / `array((m, n), init)`; ranges `a:b`, `a:b:step`; slices
  `a[i:j]`, `a[::-1]`. `Sys.getenv(name, defval)`; `s.to_int(): int?`.
- A `match` arm body may be a block: `| pat => val r = ...; expr`.
- `break`/`continue`/`return` (with a value or bare) are legal in *expression*
  position — as a `match`-arm or `if`-branch value (`| _ => continue`,
  `if c {..} else {break}`, `| 0 => return v`) — exactly like `throw`
  (ctrlflow-1/FB-015). A jumping arm/branch has pseudo-type `TypErr`, so it
  unifies with any value-producing sibling; K-form/codegen unchanged (they
  still lower to void jumps and route through the block cleanup chain, so
  ref-counted locals live at the jump are freed). Legality is unchanged and
  gives targeted messages: `break`/`continue` need an enclosing loop
  (`cannot use 'break' outside of loop`); `break`/`continue` in `fold` or
  `@parallel` bodies stay rejected in BOTH positions (no widening); a bare
  `return` in a non-void function is a return-type mismatch, not a parse error.
- Shift count must be `int`: write `x >> int(n)` for a non-int count.
- **Overload resolution: the least-generic viable candidate wins**, regardless
  of declaration/import order. No unique winner at a fully-determined call =
  ambiguity error; disambiguate with a qualified call (`Module.__mul__(a, b)`;
  `Module.(*)` does not parse yet). With free type vars in the argument
  types, ties fall back to first-match — don't rely on that in new code.
  Exact keywordless match beats a candidate viable only via defaulted
  keyword args.
- `[]` is typed "some collection" (list/vector/array): `val n: int = []` is a
  typecheck error; if the kind is never pinned, K-normalization asks for an
  annotation.
- **Return annotations on generic operators/functions are viability filters,
  not decoration**: a free (inferred) return type unifies with any expected
  type and can steal under-constrained call sites. Annotate operators with a
  FRESH var (`: 't3 complex`, `: 't3 [+]`, `: ('t3 ...)` — "returns SOME
  such") to keep mixed-type widening while rejecting foreign contexts.
  Enforced: `tools/lint_op_returns.py lib` (CI).
- `-Wimplicit-rettype` warns on module-level functions with inferred returns
  (nested/lambdas/@ccode/auto-generated exempt). Scope: all USER modules;
  `=all` includes stdlib (self-gate: `tools/rettype_gate.sh`, CI; keep clean
  when adding stdlib functions — scope excludes NN/Onnx/Protobuf/OpenCV).
  `-Wall` = umbrella; `-Werror` promotes all warnings to a nonzero exit.
  Return annotations erase in K-form (byte-identical generated C). The
  compiler sources are not gated yet (annotate-3, ~340 functions).
- Annotation spellings: nullary function type is `(void -> T)`, not
  `(() -> T)`; a module's own type parses bare (`t`) or qualified (`Date.t`);
  uniform tuple return `(int...)`; any-dims array `T [+]`; when the result
  scalar type varies with input (widening via `*0.f`), a fresh `: 't3` is the
  honest annotation (safe when the arguments are already pinned).
- Comparing `-pr-resolve` censuses: normalize first — gate on the
  (name | winner | outcome) multiset, not raw text (line:col shifts and
  expected-type sharpening are noise).

## Measurement traps

- Signed overflow **wraps** (built with `-fwrapv`, folder matches). Don't rely
  on UB; don't drop the flag.
- UB signature at -O2/-O3: a value *prints* right but a branch on it goes
  wrong → the C compiler exploited UB (the predicate folded, the value
  materialized). Repro in pure C; `-fwrapv`/`-fno-strict-*` to confirm.
- ASan+UBSan: `-cflags "-fsanitize=address,undefined -fno-omit-frame-pointer"
  -clibs "<same>"` (or `fxtest.py sanitize`). Note: over-reads into valid
  adjacent heap may NOT trap — reference-checked suites are the complement.
- IR snapshot fails on CI only, `:ast` differs but `:k0`/`:k` match → suspect
  the harness's header extraction (path-length wrap, FB-013), not the
  compiler.

**Iterate tiny**: 10–20 lines → `bin/ficus -run f.fx` → fix → extend. Compiler
errors are ground truth; if it rejects something the tutorial calls legal,
minimal repro into `docs/found_bugs.md` and route around (don't fix
`compiler/` unless asked).

**Compiler code style — prefer functional over imperative.** Most of the
compiler is accumulator recursion / immutable bindings / `match`, and that is
the house style to match: historically more robust (fewer side effects → fewer
state bugs, easier to reason about, safer under the atomic-refcount runtime).
An imperative rewrite is not automatically an improvement — e.g. converting an
accumulator recursion to a `for`-loop with `break`/`continue` is more
imperative but *not* faster (often micro-slower: `break`/`continue` are checked
in each block's cleanup section) and longer to write than `{}`. Reach for
mutation/loops only where the functional form is genuinely awkward; when in
doubt, mirror the surrounding module.

Test-file naming: a test `.fx` must not collide case-insensitively with a
stdlib module (`char.fx` shadows `Char`); numeric prefixes (`001_name.fx`)
are safe. Subdir modules import as `Dir.Module`; same-dir siblings by bare
name.

## Environment notes

- macOS python is 3.9 (no `tomllib` — the harness ships a fallback parser);
  Linux is fresher.
- macOS `sed` is BSD (no `\b`) — use `perl -pe` for word-boundary edits.
