# E1 census, re-run after the FB-017 comparator fix (resolve-1, phase 1)

Same instrument and corpus as `e1_census.md` (`bin/ficus -no-c -pr-resolve
compiler/fx.fx`), regenerated after:

- phase 0: the two genuine identical-signature duplicates (`length(string)`,
  `join(string, string list)` in both `Builtins.fx` and `String.fx`) were
  disambiguated at their only exposed call sites (inside `String.fx`), so those
  two viable>1 sites no longer exist;
- phase 1: `compare_typ_generality` freezes the var-form generics (`{...}`
  `TypVarRecord`, `(...)`/`('t ...)` `TypVarTuple`, `'t [+]` `TypVarArray`)
  into opaque sentinels on the rigid side of each trial
  (`freeze_varform_typs`), closing FB-017.

Raw dump: `e1_post_fb017_raw.txt`.

## Headline numbers (compiler/fx.fx)

| metric | pre (e1_census.md) | post |
|---|---|---|
| call sites with viable set > 1 | 344 | 342 |
| winners **agree** | 117 | **341** |
| generality = `<none>` (tie/unordered) | 227 | **1** |
| generality picks a **different concrete** winner | 0 | **0** |

- The ~224 FB-017 sites (`__eq__` ×141, `string` ×83, ...record/var-form
  generics vs a concrete overload) now all AGREE with env-order: the concrete
  candidate ranks strictly less generic, confirming FB-017 was comparator
  incompleteness, never a resolution flip.
- The 2 pre-census duplicate sites (`length`, `join`) are gone (phase 0).
- The single remaining `<none>` is the known **under-constrained** site
  (`Builtins.fx:128:38`, `__cmp__` with expected type
  `(<unknown>, <unknown>) -> <unknown>`, 21 trivially-viable candidates) —
  exactly the case the amended tie policy sends to the env-order fallback
  (deferral is session 2).

**Go/no-go for phase 2 confirmed**: zero sites where specificity ranking would
select a different concrete function; ranking + env-order fallback at
under-constrained ties is corpus-invariant on the compiler.
