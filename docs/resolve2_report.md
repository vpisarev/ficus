# resolve-2: `TypVarCollection` — `[]` is "some collection", not "anything"

Branch `resolve-2` (off `resolve-1`, 2026-07-08). One-evening experiment,
agreed with Vadim after the `[]`-typing investigation at the end of the
resolve-1 session; it worked on the first build.

## The problem

The empty-collection literal `[]` (`LitEmpty`, lexed at `Lexer.fx:536`) was
typed by `Ast.get_lit_typ` as a **fully free** `make_new_typ()`. Consequences:

1. **FB-007/S3**: a `[]`-initialized fold accumulator has a fully free type at
   its first use, so an over-general overload candidate (`+('t, 't complex)`)
   could capture it (`'t := nnop_t list`), tie incomparably with list-concat
   `+('t list, 't list)`, and win by env-order at the under-constrained
   fallback — the `NN/FromOnnx.fx:1012` / `vision_classify.fx` failure that
   kept the scalar-left `lib/Complex.fx` operators fenced.
2. `val n: int = []` **passed the type checker** (`-no-c`); the misuse was
   caught only at K-normalization ("[] is misused ...") — or at codegen.

## The fix

New var-form in `typ_t` (next to `TypVarTuple`/`TypVarArray`/`TypVarRecord`):

    | TypVarCollection      // "some list, vector or array; elements unknown"

`get_lit_typ(LitEmpty)` now returns `TypVar(ref Some(TypVarCollection))`.
`maybe_unify` binds a collection-var only to `TypList`/`TypVector`/
`TypArray`/an array-var `'t [+]`/a plain free var/another collection-var
(plus the usual `TypErr` escape); **everything else is a unification
failure**. Since typedef aliases are expanded by `check_typ` before
unification, `type intlist = int list; val x: intlist = []` still works.

Support arms, mirroring the existing var-form family:

- `Ast.fx`: `typ_t` constructor; `get_lit_typ`; `deref_typ.find_root` stops at
  it; `typ2str` renders it `[...]`; `walk_typ` identity arm.
- `Ast_typecheck.fx`: `occurs` (no payload → false); the two `maybe_unify`
  arms (direct + symmetric flip, placed after the `TypVarRecord` arms);
  `freeze_varform_typs` freezes it to an opaque constant (neither `'t list`
  nor `'t [+]` covers "could be list OR vector OR array"; only plain `'t`
  does); `typ_has_free_vars` counts it as free, so `[]`-sites remain
  "under-constrained" for the resolve-1 tie policy (no new ambiguity errors).
- `Ast_pp.fx`: precedence class + `[...]` rendering.
- `K_normalize.fx`: an unresolved collection-var now gets a dedicated
  message ("[] denotes an empty collection (list, vector or array), but which
  one cannot be inferenced here..."); the `lit2klit` "[] is misused" check
  stays as the backstop.

No parser/lexer changes: there is (for now) no source syntax for the
var-form; it only enters through `[]`.

## What it unlocked (same branch)

- **`lib/Complex.fx` scalar-on-the-left variants UNFENCED**:
  `op (a: 't, b: 't complex)` for `+ - * /`. At a list site the candidate now
  dies at the viability trial (`'t := [...]` collection-var, then
  `'t complex` vs `nnop_t list` fails), so it never competes with
  list-concat. The latent sign bug in the fenced `-` body was fixed on the
  way (`s - c` = `complex(a - b.re, -b.im)` — `im` must be negated).
  `examples/vision_classify.fx` (the original FB-007 victim) typechecks with
  the variants live; `examples/fst.fx` now exercises `1.f - *c` in the fxtest
  corpus.
- **FB-007/S3 test UNFENCED**: `fb007.s3_free_accumulator` in
  `test/test_resolve.fx` (free `[]` fold accumulator + `[:: x*x] + prog`
  with CplxHelper's over-general `+` in scope) passes.
- **New negative golden** `test/negative/215_empty_collection_scalar.fx`:
  `val n: int = []` is a typecheck-time error now.
- The `++`/`.`-concatenation candidate (language_changes §4) is demoted from
  "needed for correctness" to a pure taste question (Vadim: "maybe not").
- **Imaginary literals `N.fi` / `N.i` / `N.hi` work now** (follow-up commit):
  the lexer desugared them into `complex(0, N)` with an **int** zero
  (`LitInt(0)`), so the tokens typed as `complex(int, float)` and no
  constructor matched — one of the reasons the original 2021 demo
  `val c = ref (1+1.fi)` was fenced. The zero real part is now a float
  literal of the same width as the imaginary part (`Lexer.fx`, the `c == 'c'`
  branch). `examples/fst.fx` runs the demo as `1.f + 1.fi` and prints the
  mathematically correct values (`abs((1+1i)*2)=2.8284271`,
  `1-(2+2i)=-1.0-2.0i`).
- Mixed-type operator variants (`op (a: 't1, b: 't2 complex)`) were tried by
  Vadim and reverted: their bodies do `'t1 op 't2` arithmetic, which is
  pinned at declaration/instantiation boundaries — exactly FB-007/S1, i.e.
  session-2 deferral work. Scalar variants stay homogeneous (`'t op
  't complex`), so `1 + 1.fi` (int + float complex) is correctly rejected;
  write `1.f + 1.fi`.

FB-007 is now reduced to **S1 only**: mixed-type arithmetic (`int + 't`)
inside generic bodies is pinned at declaration check — that is genuine
session-2 deferral work. True deferral of under-constrained calls in general
also remains session-2 scope; TypVarCollection removes the `[]` collision
class, not the mechanism.

## Verification

- `make` — clean first build (the exhaustive matches over `typ_t` that needed
  a new arm were exactly the ones edited).
- `bin/ficus -run test/test_all.fx` — 135/135.
- `python3 tools/fxtest/fxtest.py all` — unit + negative(73) + ir + cfold +
  corpus(O0/O3) green. One deliberate golden diff: `207_add_int_string.err`
  gained one `Candidates:` line (the unfenced scalar-left `+`, correctly
  reporting "argument 2 of type 'string' does not match ''t complex'").
- `determinism`, `sanitize` — green.
- `python3 tools/update_compiler.py` — **fixpoint holds**. Bootstrap diff = 9
  modules: the 4 edited ones (Ast, Ast_pp, Ast_typecheck, K_normalize) +
  Parser/C_form/Compiler/K_copy_n_skip/K_form, whose diffs are pure `typ_t`
  **constructor-tag renumbering** (TypVarCollection sits mid-type, all later
  tags shift; the shared runtime destructor switch gains one case). Verified
  by inspection: the four 16-line diffs are identical mechanical shifts.
- Census: `-pr-resolve` over `compiler/fx.fx` — **350 sites: 343 ranked, 7
  under-constrained fallbacks, 0 ambiguity errors — identical to resolve-1**,
  and the 7 fallback sites are the same (C_pp AssocLeft/AssocRight,
  adjust_decls; none involves `[]`). Corpus invariance on the compiler holds.
- Typecheck perf on `compiler/fx.fx`: 6.73s vs 6.60s on resolve-1 (~+2%;
  the two new `maybe_unify` arms sit after the tuple/record var-form arms,
  so every non-collection pair pays two extra pattern probes).

## Design notes / follow-ups

- The stronger reform-epoch option stays open (language_changes §6): `[]` =
  empty *list* only, plus a dedicated empty-array spelling (e.g. `[.]`).
  TypVarCollection is compatible with it — it would just narrow further.
- There is no source syntax for "some collection" yet. If generic functions
  ever want `fun f(a: 't [..])`-style constraints, the var-form is ready.
- `freeze_varform_typs` freezes a collection-var to an opaque constant — the
  correct reading ("some specific collection, kind unknown") but it can only
  matter if a collection-var flows into a *signature* via inference; source
  signatures cannot contain it.
