# annotate-2 report — return-type sweep + `-Wimplicit-rettype`

Branch `annotate-2` off master. Extends annotate-1 (106 generic operators) with
(WP-1) an opt-in `-Wimplicit-rettype` warning + the compiler's first non-fatal
**warning subsystem**, (WP-2) explicit return types on **all** module-level
functions in the annotated stdlib scope, and (WP-3) a CI gate. Design rationale:
`docs/language_changes_brief.md` §1.7; handoff `docs/annotate2_handoff.md`.

## Scope decision (Vadim, 2026-07-09)

`-Wimplicit-rettype` flags **every** module-level function without a return
annotation — 710 across all libraries, far more than the 276 overloaded-generic
functions the `lint --funs` worklist tracked. Vadim's call: **annotate all of
`lib/` except the application libraries NN/, Onnx/, Protobuf/, and `OpenCV.fx`**;
the small `DSP/`, `Drawing/`, `Image/` modules are IN scope. That is **558**
functions across **24 files** (final: 559, incl. one nested helper for lint
parity). Excluded libraries are left for a later pass.

## WP-1 — warning subsystem + `-Wimplicit-rettype`

Three layers, mirroring the error machinery:

- **Infrastructure** (`compiler/Ast.fx`): `compile_warning(loc, msg)` already
  existed but only printed; it now also increments a counted `all_compile_warns`
  (reset in `init_all`). The driver (`compiler/Compiler.fx`) prints a one-line
  `N warning(s) generated` summary and, under `-Werror`, folds a nonzero count
  into the failing exit path (`process_all` returns `ok && !(Werror && warns>0)`).
  This unifies the pre-existing unused-value / `@parallel` warnings under one
  counter too. Seed for session-2's multiple-errors-per-run work.
- **Flags** (`compiler/Options.fx`): `-Wimplicit-rettype`, `-Wall` (umbrella,
  currently just enables this one), `-Werror` (promote ALL warnings). Modeled on
  the existing `print_resolve`/`W_unused` fields; adding fields to `options_t`
  propagated the struct layout into 9 bootstrap modules as expected.
- **The check** (`compiler/Ast_typecheck.fx`, in the `DefFun` case of
  `check_eseq`): fires once per source definition. Detection is **signature-
  level** — the parser leaves an unannotated return as a fresh `TypVar(ref None)`
  (`Parser.parse_fun_params`), captured BEFORE `check_deffun` resolves it — so it
  works for **generic template functions too**, which are never body-checked at
  their definition site (only per-instantiation). For non-generic functions the
  message prints the concrete inferred type for a copy-paste fix; for generics it
  is `<unknown>` (honest: the template return isn't inferred at the definition).

**Scoping — user code by default, `=all` for the stdlib self-gate.** Ficus
typechecks every imported module from source, so warning about *stdlib* would
bury the user in noise about library code they didn't write. The distinction
that matters is **stdlib vs user code**, not root vs imported: a user wants
every module of *their own* multi-file project checked, not just the file named
on the command line. So the compiler tags each module stdlib-or-not by whether
its path is under `<ficus_root>/lib` (`Ast.ficus_std_path`, set in
`process_all`), and:

- **`-Wimplicit-rettype`** (and `-Wall`) covers **all user modules** (everything
  outside stdlib) — the whole project, transitively, not just the root.
- **`-Wimplicit-rettype=all`** additionally covers stdlib — how the stdlib gates
  *itself*. `tools/rettype_gate.sh` uses it (a stdlib file compiled as root is
  silent under the plain flag, since it *is* stdlib).

(Initial WP-1 was root-only; revised after Vadim's observation that, with stdlib
now annotated, root-only under-warns — it misses the user's own non-root
modules. Proven: with one stdlib annotation removed, plain `-Wimplicit-rettype`
stays silent while `=all` flags it; and `test/test_all.fx` under the plain flag
now surfaces its imported test modules `test_oop`/`myops`/… , not just
`test_all.fx`.)

**Exemptions:** nested functions and lambdas (not `ScModule` scope; `__lambda__`
name), `@ccode` functions (must spell their type anyway), and auto-generated
constructors (`is_constructor(df_flags)`). Verified: compiling a file with record
/ variant types produces no spurious warnings for auto-generated
`string`/`__eq__`/… (those carry concrete return types, not fresh vars).

Tests: `test/negative/220_implicit_rettype.fx` (a new `// flags: -Wall -Werror`
header in the T3 harness lets a warning case exercise the nonzero-exit path)
locks the message format AND the `-Werror` promotion; an annotated sibling
function in the same file confirms silence. Flag-off is zero behavior change:
the determinism leg passes and the bootstrap regen shows **no stdlib module
changed** (return annotations erase in K-form → byte-identical generated C).

## WP-2 — the sweep (558 functions, 24 files)

Method (Vadim's iterate-and-insert loop, tool `tools/retann.py`): compile with
`-Wall`, read the inferred type the warning prints, re-spell it into source
(the only transform is stripping the `@NNN` gensym — both `Date.t` and bare `t`
parse inside their own module), insert `): <type>`. The **compiler is the safety
net**: a wrong annotation fails typecheck, so the tool cannot silently change
behavior; resolution shifts are caught by the census.

- **248 concrete** functions (inferred type is a spellable concrete type) were
  inserted mechanically by `retann.py`.
- **310 generic** functions (`<unknown>` at the definition) were annotated by
  convention, per family, verified per-file to 0 warnings + 0 errors.

Per-file counts (annotations added):

| module | n | module | n | module | n |
|---|---|---|---|---|---|
| Builtins | 227 | UTest | 53 | Array | 54 |
| Math | 43 | Date | 34 | PP | 19 |
| Complex | 13 | Hashset | 11 | String | 9 |
| Hashmap | 9 | Filename | 8 | Dynvec | 7 |
| Set | 6 | Map | 6+1 | List | 6 |
| Deque | 6 | LexerUtils | 5 | Json | 5 |
| Color | 4 | Sys | 3 | File | 3 |
| Image/Decoder | 3 | Image/Encoder | 3 | Drawing/Shapes | 2 |

Family conventions applied (extending annotate-1's):

- Mechanical: `string`→`string`, `print`/`println`→`void`, predicates
  (`is*`/`odd`/`even`/`all`/`exists`/`empty`)→`bool`, `size`/`length`/`channels`
  →`int` (or the exact int-tuple for ND), assertions (all 53 UTest
  `ASSERT_*`/`EXPECT_*`/`test_failed_*`)→`void`.
- Generic pass-throughs (`min`/`max`/`abs`/`clip`/`value`/`hd`/`last`/`nth`,
  scalar `dot`/`normInf`, Complex `conj`/`exp`/trig)→ the bound input var (`'t`,
  `'t [+]`, `'t complex`), NOT a fresh var — these return one of their arguments
  / the same element type.
- Conversions map to their fixed element type across the shape: `int(x:'t)`→`int`,
  `int(x:('t...))`→`(int...)`, `int(x:'t [+])`→`int [+]`; `sat_*`/`round`
  likewise.
- Numeric folds annotated with **the type the body computes today** (census-
  guarded): `Array.sum`/`mean`→`double`, `Array.norm*`→`double`, `Array.sum(_,v0:'s)`
  →`'s`; container ops return their container type; tree rebalancers return the
  tree type; `Map.blackify`→`(tree, bool)` (a tuple, not a bare tree — found by
  reading the body).

### Judgment cases (for Vadim)

1. **Width-varying tuple-norms** (`Builtins.normL1`/`normL2sqr`/`normL2`/`norm`
   over `('t...)`, 8 overloads): the body widens via `*0.f`, so the result is
   `float` for `int`/`float` input but `double` for `double` input — not
   expressible as `'t` or any fixed type. Annotated with a **fresh scalar var
   `'t3`**. This is safe (unlike a free return on an unconstrained function): the
   argument is already pinned to `('t...)`, so the fresh return cannot broaden
   viability at scalar/list accumulator sites (the FB-007 hazard). Verified to
   typecheck and run correctly at int/float/double instantiations, and to be
   census-neutral. If a tighter contract is wanted, this is the one family to
   revisit. (Array's `norm*` do NOT need this — they use `0.` (double) and are
   uniformly `double`.)
2. **`floor`/`ceil`/`trunc`/`round` return `int`** (FB-019): the scalar defs are
   explicitly `: int`, so the array forms were annotated `int [+]` to match. This
   contradicts CLAUDE.md's "floor/ceil return float/double" — a design/intent
   question flagged for you (deliberate round-to-int vs a lossy bug). Annotations
   track current behavior and move together if the code changes.
3. No functions were **parked** unannotated — the "type-level helper" concern did
   not bite: `scalar_type(_: 't)` returns the concrete `scalar_t`, `elemtype`
   lives only in the excluded OpenCV.

### Census (acceptance gate)

Baseline `bin/ficus -no-c -pr-resolve compiler/fx.fx`: **350 sites / 343 ranked
/ 7 fallbacks / 0 errors**. After WP-1: **351 / 344 / 7 / 0** — the +1 is WP-1's
own new code (`get_orig_id(df_name) == std__lambda__` adds one `__eq__(id,id)`
site, ranked to the same winner), NOT a stdlib effect. After the full sweep:
**351 / 344 / 7 / 0**, with the **(name | winner | outcome) multiset IDENTICAL**
to the WP-1 baseline (verified with a location-normalized comparator, since
unrelated compiler edits shift call-site line:col). The only visible movement is
a few `print`/`string` call sites whose *expected* type sharpened from `<unknown>`
to `void`/`string` because their enclosing function now has a declared return —
same winner, same outcome, same fallback set. **The sweep is resolution-neutral.**

## WP-3 — CI gate

`tools/rettype_gate.sh` compiles every in-scope stdlib file as a root with
`-Wall -Werror` (`-no-c`, a few seconds), wired into `ci.yml` on the gcc/ubuntu
leg beside the existing operator lint. Scope excludes NN/Onnx/Protobuf/OpenCV.

The compiler-based gate **supersedes** `lint_op_returns.py --funs`: the textual
linter cannot see function **nesting**, so it false-positives on nested generic
helpers (`Map.mem_`) that `-Wimplicit-rettype` correctly exempts. No
"parked-by-decision" allowlist was needed (nothing was parked), so the planned
`--funs` allowlist extension is moot; `lint_op_returns.py lib` (operators) stays
as the faster separate guard. The **test suite** is NOT gated yet: test files mix
interface methods, operator overloads, and deliberate resolution-edge-case
helpers (`test_resolve.fx`, `myops.fx`) whose annotation needs care not to
perturb what they test — a scoped follow-up.

### annotate-3 planning data

`compiler/*.fx` has **340** module-level functions that would trip
`-Wimplicit-rettype` today (per-file-as-root count). That is the gradual
annotate-3 worklist; the compiler is intentionally NOT gated yet.

## Closing checklist

- `make` + `bin/ficus -run test/test_all.fx` → 136 tests PASS.
- `fxtest all` → PASSED (74 negative incl. the new 220; 212 golden updated as the
  `__negate__` candidate now prints `'t [+]` instead of `<unknown>` — a strictly
  better diagnostic).
- `update_compiler.py` fixpoint holds; only 9 **compiler** bootstrap modules
  changed (WP-1 options_t propagation) — **zero stdlib** modules (annotations
  erase in K-form).
- `fxtest determinism` PASS, `fxtest sanitize` (ASan+UBSan) PASS.
- Census unchanged-or-explained (resolution-neutral, above).

## New bugs / findings (`docs/found_bugs.md`)

- **FB-018** `Array.diag(d: 't[])` uses `size(a)` before `a` is defined (should be
  `size(d)`); the array form is unusable, only errors on instantiation — exposed
  by the sweep. Fenced, not fixed.
- **FB-019** `floor`/`ceil`/`trunc`/`round` return `int` (contra CLAUDE.md);
  design question, see above.

## Lessons for CLAUDE.md (added)

- Nullary function type is `(void -> T)`, not `(() -> T)` (empty-tuple error).
- A module's own type parses as a return annotation both qualified (`Date.t`) and
  bare (`t`).
- `-Wimplicit-rettype`/`-Wall`/`-Werror` exist; stdlib (minus NN/Onnx/Protobuf/
  OpenCV) is gated by `tools/rettype_gate.sh`; warnings are counted with an
  end-of-run summary.
- Census diffs must be location-normalized (unrelated edits shift call-site
  line:col); gate on (name | winner | outcome), not raw text.
