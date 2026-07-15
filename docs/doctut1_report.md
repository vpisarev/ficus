# doctut-1 report — the tutorial becomes machine-verified

`doc/ficustut.md` is now compiled by CI. `tools/fxtest/doctut.py` extracts every
fenced code block, classifies it, and feeds the checkable ones to the real
compiler (`bin/ficus -no-c`). A future syntax reform that breaks a documented
example now breaks the `fxtest.py doctut` leg.

## The leg

- **`tools/fxtest/doctut.py`** (stdlib-only, mirrors `negative.py`): fence
  extraction (column-0 **and** list-indented fences), directive-driven
  classification, section-accumulation for continuations, per-block pass/fail
  with the **tutorial** line number and the compiler's first diagnostic (path
  stripped).
- Wired as **`python3 tools/fxtest/fxtest.py doctut`** (opt-in, like `lsp`) and
  added to the **nightly** CI axis (`.github/workflows/ci.yml`) — cheap (~80
  `-no-c` compiles), but kept out of the per-PR leg to stay lean.
- Classification is by an **HTML-comment directive on the line above the fence**
  (invisible in the rendered PDF): bare block = checked standalone;
  `<!-- doctut: fragment -->` = deliberately partial (skipped);
  `<!-- doctut: continue -->` = extends the section's prior checkable blocks;
  `shell`/`run` also supported. Shell transcripts (`$ …`), info-string blocks
  (```` ```text ````, ```` ```ML ````) and C code are auto-skipped by content.

## Block census (239 fenced blocks)

| Class | Count | Checked? |
|---|---:|---|
| standalone (no directive) | 80 | ✅ compiled |
| continuation (`continue`) | 7 | ✅ compiled with section context |
| fragment (`fragment`) | 142 | skipped (schema / intentional-error demo / prose-context snippet) |
| shell / non-Ficus (auto) | 10 | skipped by content |
| **total verified & green** | **87** | |

The high fragment ratio is inherent — the tutorial is full of illustrative
snippets that reference names defined in prose, grammar schemas
(`fold acc1=initval1 …`), and *deliberate* error demos (`val err = [1,2,3,4,5.]`).
156 directive lines were added (149 `fragment` + 7 `continue`); all are invisible
in the rendered document. Future work can convert `fragment`→checkable by
supplying minimal context, shrinking the skip set.

## Second pass: auditing the fragments (Vadim-prompted) — 7 more bugs

The first pass classified blocks greedily: *fails to compile standalone → mark
`fragment`*. That is unsafe — it conflates "deliberately partial" with "a
self-contained example that happens to have a real bug", **hiding the bug under a
skip marker**. Prompted by Vadim spot-checking `list_find`, a dedicated audit
(compile every `fragment`, bucket the diagnostic, ignore the ones explained by a
prose-introduced name / schema placeholder / intentional-error demo, inspect the
rest) surfaced **7 genuine bugs** that had been silently fenced:

| Block | Bug | Fix |
|---|---|---|
| `list_find` usage | arguments swapped: `list_find(pred, l)` | `list_find(l, pred)` |
| `fibseq` (name-reuse) | recursive `fib_(i+1, a+b, a, b::result)` drops the `n` arg (5-param fn, 4 args) | add `n` |
| Numbers overflow | `(a :> uint64) * b` — `uint64 * int` has no overload | `* (b :> uint64)` |
| `Hadamard` | `mtx[5,:]*2` is `float[] * int` | `*2.` (float scalar) |
| `File` size | `f.seek(0, …)` passes `int` where `int64` is required | `0i64` |
| list-of-types vector | code used mutable `vector(...)` but the prose describes the immutable `rrbvec` (fast concat/slice); `+`/slice unsupported on `vector` | `rrbvec(...)`; `big_list` → a real `[:: …]` list; `rng(0,N)` → `rng.uniform(0,N-1)` |
| `list_map` | `f(a)` — `a` undefined (lambda param is `x`) | `f(x)` (also `continue`-chained to `list_foldl`/`list_rev`) |

After fixing, the directive set was **regenerated cleanly** and re-audited to
convergence: every remaining `fragment` now fails only for a legitimate reason
(prose-introduced name, cross-section dependency, external module, grammar
schema, or a deliberate error demo). **Lesson / recommendation**: never let
"doesn't compile" silently imply "fragment" — a fragment must be *deliberately*
partial. Worth adding a `doctut --audit` mode later that flags any `fragment`
failing with a type/resolution error (not a plain undefined-name/parse error) as
a review candidate, so this class can't regress.

## Failure triage — 29 real code fixes (all case 1: the snippet was wrong)

The rewrite was done without compiling, and several reforms had leaked through.
Every fix below is a code-block change; the compiler is ground truth.

### Old value-yielding `fold` (now a type error under fold-1) — 9 sites
The Folding section itself was migrated, but the old form survived elsewhere:
| Line | Section | Fix |
|---|---|---|
| 1447 | Special fold-like ops | `if x%2==0 {false} else {all_odd}` → `if x%2==0 {all_odd = false}` |
| 1456 | Special fold-like ops | `{false; break}` → `{all_odd = false; break}` |
| 1534 | Comprehensions (matmul) | `{s + A[i,k]*B[k,j]}` → `{s += …}` |
| 1715 | Lambda (integrate) | tuple-accumulator body `(sum+…, right)` → `sum += …; left = right` |
| 1763 | Closures (make_coin) | `{s ^ rng.next()}` → `{s ^= rng.next()}` |
| 2229 | Arrays (sumpixels) | `{sum + x}` → `{sum += x}` |
| 2303 | Array access (matmul) | `{s + A[i,k]*B[k,j]}` → `{s += …}` |
| 2856 | Lists | `{i*i :: l}` → `{l = i*i :: l}` |
| 3100 | **Pattern Matching** (diff_stat) | 4-accumulator fold yielding tuples → per-arm `n_exact += 1` etc. |

### Postfix type application (removed by generics-1) — 8 sites
| Line | Was | Now |
|---|---|---|
| 830 | `string ref` | `ref[string]` |
| 930 | `detected_object_info_t list` | `list[detected_object_info_t]` |
| 982 | `(string, int) Map.t` | `Map.t[string, int]` |
| 1833 | `detection_t vector` | `vector[detection_t]` |
| 3332 | `(expr, expr list)` | `(expr, list[expr])` |
| 3336 | `Seq: expr list` | `Seq: list[expr]` |
| 3358 | `entry_type list` | `list[entry_type]` |
| 3465 | `(Empty : int rbtree)` | `(Empty : rbtree[int])` |
| 3082–83 | `object_t list` ×3 | `list[object_t]` |

### Typos / wrong forms — 12 sites
| Line | Fix |
|---|---|
| 1277 | stray `)` in `rng.uniform(0u8, 255u8))` → one paren removed |
| 1577 | `when` in an **array** comprehension → list `[:: for … when …]` (array size is fixed before the loop) |
| 1580 | `continue` in an array comprehension → list `[:: for …]` |
| 1848 | keyword `scale =` → `pix_scale =` (matches the parameter name) |
| 2109 | field type `box: rect_t` → `box: Rect` (the type is `Rect`) |
| 2721/2724 | `dilate3x3`: `img.[y,x]` → `img.clip[y,x]`, and `max(img.clip[y+1,x]), …)` unbalanced parens fixed |
| 3667 | `ord(c) - ord(#"0")` → `int(c) - int('0')` (`ord`/`#"…"` do not exist) |
| 375/384/396 | U+2212 `−` → ASCII `-` (in the operator listings, incl. `−=`) |
| 1304 | em-dash `<—` → `<-` |

## Prose edits (for your review — facts corrected, voice untouched)

1. **Types chapter, `bf16` entry** (was silent on the literal suffix, unlike
   every sibling entry): appended *"The literals have `bf` suffix."* — verified
   against `lib/LexerUtils.fx:89,254` (`h`→fp16, `bf`→bf16). The `fp16` `h`-suffix
   claim was already correct.

That is the only pure-prose change. All other edits are inside code blocks.

## Appendix A regenerated (WP-3)

- The `ficus -h` dump was v0.1.0-era. Replaced with the **real current output**
  (`v1.0.0-alpha`), which now documents `-pr-resolve`, `-Ofast`, `-Wall`,
  `-Werror`, `-Wimplicit-rettype[=all]`, `-Wno-unused`, `-fmax-errors=N`,
  `-diag-format=json|human`, and the corrected `--` app-args line (the old dump
  used an em-dash). The `<!-- TODO(vadim) -->` marker is removed.
- fp16 `h`-suffix claim verified against the lexer (correct, unchanged).

## Found bug → resolved by Vadim during the session

- **FB-030** — the `.=` record-update-assignment (`rec .= {…}`) documented in the
  "Modifying/updating record" section is rejected by the compiler even in the
  simplest case (`r .= {x=5}`): `Parser.fx:1668` calls `parse_exp_list(ts, …)` on
  `ts` (still at `.=`) instead of `rest` (after `{`). **Vadim removed `.=` from
  the tutorial** (the example now uses direct field assignment / `velocity ./= 2`,
  and `.={...}` was dropped from the operator listing), so that block is now
  compiled by the leg. The latent parser bug is recorded in `docs/found_bugs.md`.

## Follow-on: `Builtins.fx` tuple-operator cleanup (Vadim-directed)

While verifying the tuple/record examples we found that plain `*` and `/` on a
2-tuple meant **complex** multiply/divide (`(10,4)/(2,2) → (3,-1)`), and `*` on a
4-tuple meant the **quaternion** product — a pre-`Complex.fx` experiment
("make `(T,T)` a complex number") that Vadim confirmed was a mistake now that
`complex[T]` (`lib/Complex.fx`) is the real type.

- **Removed** the tuple `operator * / (T*2)` (complex) and `operator * (T*4)`
  (quaternion) from `lib/Builtins.fx`. No stdlib/corpus/test/example used them
  (`fxtest all` still PASSED; `update_compiler.py --check` reports **0 bootstrap
  modules change** — the compiler never instantiated them).
- **Added** `operator / [T1, T2, T3](a: (T1...), b: T2): (T3 ...)` (scalar divides
  a uniform tuple), mirroring the existing `./` broadcast, so `v /= n` scales a
  short vector; lint-clean (`lint_op_returns.py`). On a (tuple, tuple) call the
  now-gone complex form no longer competes, and plain `*`/`/` on two tuples is an
  error (use `.*`/`./`).
- **Open for Vadim**: plain `+`/`-` are still element-wise on tuples but `*`/`/`
  are not (only the dot forms + the new `/`-by-scalar) — add element-wise plain
  `*`/`/` for two tuples if you want full symmetry.

## Mini-tests

No new `test/test_*.fx` cases were added: the corrected examples are now locked
by the doctut leg itself, and the existing UTest suite already covers the
underlying features (fold, generics, comprehensions). The only genuinely shaky
feature (`.=`) is a found-bug, not a passing behavior to pin.

## Verification

- `fxtest.py doctut` — **87/87 green** (80 standalone + 7 continuation), after the
  fragment audit converted 6 hidden-bug blocks into verified ones.
- `fxtest.py all` (unit + negative + ir + cfold + corpus O0/O3) — **PASSED**,
  including after the `Builtins.fx` tuple-operator cleanup.
- `update_compiler.py --check` — **0 bootstrap modules change** (the removed
  operators were never instantiated by the compiler); no regen required.
- `lint_op_returns.py lib` — clean (the new broadcast `/` carries a fresh return).
