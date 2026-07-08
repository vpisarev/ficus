# E1 census — env-order vs generality winner (WP-E)

Instrument: `-pr-resolve` (D2). For every lookup whose expected type is a function
type and whose viable **function** set has >1 entry, the trace prints the
candidates, the **env-order winner** (the first viable in env-list order — exactly
what today's `find_first` commits to) and the **generality winner** (the unique
least-generic candidate per `compare_fun_generality`, or `<none>` when tied /
unordered). Raw dump: `e1_compiler_raw.txt` (regenerate with
`bin/ficus -no-c -pr-resolve compiler/fx.fx`).

The compiler itself (`compiler/fx.fx`, ~54 modules incl. the whole stdlib
preamble) is the corpus here — the largest real Ficus program available.

## Headline numbers (compiler/fx.fx)

| metric | count |
|---|---|
| call sites with viable set > 1 | 344 |
| winners **agree** (generality confirms env-order) | 117 |
| winners **disagree** | 227 |
| — of which generality = `<none>` (tie/unordered) | 227 |
| — of which generality picks a **different concrete** winner | **0** |

**The go/no-go result for tranche A: zero sites where specificity ranking would
select a different concrete function than today.** Every disagreement is a
`<none>` — the comparator failing to *confirm* env-order's pick, never
*contradicting* it. So a resolver that ranks by specificity and falls back to
first-match on a tie stays byte-for-byte corpus-invariant on the compiler.

## Anatomy of the 227 `<none>` sites

By viable-set size: 226 have exactly 2 viable candidates, 1 has 21.
By name: `__eq__` ×141, `string` ×83, `length`/`join`/`__cmp__` ×1 each.

Two root causes, neither a real divergence:

1. **Auto-generated record/variant generics vs a concrete type (~224 sites)** —
   `__eq__`/`string`/`<=>`/`hash`/`print` for records are typed with `{...}`
   (`TypVarRecord`), e.g. `template<__var_record__> operator ==(a:{...}, b:{...})`.
   `maybe_unify` treats a record-var **symmetrically** (it binds to a concrete
   record in either direction), so the two one-way generality trials both
   succeed and the pair reads as `EqGeneric`. The concrete overload *should* be
   ranked strictly more specific; the comparator can't see it yet. This is
   **FB-017** (a documented limitation of the new comparator, to be closed in the
   surgery session by skolemizing the `TypVar*` family). Env-order already picks
   the concrete at every one of these sites, and a completed comparator would
   too — so this is comparator *incompleteness*, not a resolution flip.

2. **Genuine identical-signature duplicates (2 sites)** — `length(string)->int`
   and `join(string, string list)->string` are declared in **both** `Builtins.fx`
   and `String.fx` (String.fx re-declares them as `@inline` wrappers). Distinct
   `deffun_t`s, byte-identical signatures → truly `EqGeneric`. See E4.

## The 117 agreements

These are the clean concrete-beats-generic sites the comparator *can* order:
`repr(char)` vs `repr('t)`, `string(string,fmt)` vs `string('t,fmt)`,
`print(string)` vs `print('t)`, etc. — env-order and generality both pick the
concrete. These are the cases that already work and that the comparator's D1
unit tests (`test/test_gencmp.fx`) lock as the contract.

## Self-contained divergence NOT in the compiler corpus

The compiler's own code contains **no** site where a concrete overload declared
*before* a generic one would flip. User code can hit it trivially (**FB-016**):

```
fun f(x: int) = x + 1
fun f(x: 't)  = x
val a = f(5)      // == 5 today (generic wins by env order); a complete
                  // specificity resolver returns 6 (concrete wins)
```

`-pr-resolve` on this shows `env-order winner: f('t)`, `generality winner:
f(int)`, `winners agree: false` with a real (non-`<none>`) generality winner —
the shape the census found **zero** of inside the compiler. This is the
tranche-B "a real program's resolution flips" bucket.

## Caveats of the instrument

- The viable set is captured at `lookup_id_opt` entry with the expected type as
  the resolver sees it; when the argument type is still under-constrained (a
  free var), many generic overloads are trivially viable (the one 21-viable
  site, and the container-`size`/`string` families). Env-order picks the first
  and the subsequent unification narrows it — faithful to today's behavior.
- Only function candidates are collected; a same-named value/constructor is
  ignored. The trace is gated to fire only when the expected type is a `TypFun`,
  so value uses that merely share a function's name (e.g. a parameter named
  `size`) do not produce spurious entries.
