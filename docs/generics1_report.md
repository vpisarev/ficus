# generics-1 report — bracketed generic notation (`list[T]`)

Branch `generics-1`. Brief: `docs/generics1_brief.md`; design: `docs/language_changes_brief.md` §3.1; census: `docs/generics_census.md`. Second construct of the syntax-reform epoch (after fold-1).

## What shipped

Generics moved from the ML/SML notation to a Python/C++/Java-familiar one:

```
't list                 -> list[T]
('k, 'd) Map.t          -> Map.t[K, V]
't ref                  -> ref[T]
't option / 't?         -> T?            (option[T] also works)
't [+] / 't [,]         -> T [+] / T [,] (arrays stay postfix; only the quote drops)
type ('k,'d) tree_t =   -> type tree_t[K, D] =
fun f(x: 't, y: 'r)     -> fun f[T, R](x: T, y: R)      (params DECLARED after the name)
operator == (a: 't ..)  -> operator ==[T](a: T ..)
class ('k,'d) t         -> class t[K, D]
```

Decisions (locked with Vadim during the session):
- **Type parameters are UPPERCASE** (`'t`->`T`, `'k`->`K`, ...): the dominant
  industry convention, and in type position uppercase is free (builtins are
  lowercase, modules arrive qualified, user types are multi-letter), so a single
  uppercase letter separates a parameter from lowercase values AND concrete types
  -- exactly what the apostrophe used to signal.
- **Parameters are DECLARED explicitly** after the name (`fun f[T]`, `type t[T]`)
  and referenced bare. Without the apostrophe, a bare name can't be classified as
  "a new parameter" vs "a type in scope" implicitly, so it must be declared.
- **P1 namespaces**: type parameters resolve only in type position and never
  collide with values (as before, minus the apostrophe).
- **Application is by bracket content**: `Name[...]` is a type application unless
  the brackets hold array dims (commas / `+` / empty); `Name[]` is a 1-D array,
  `Name[T]` an application.
- **No arity overloading for types** => a single-parameter constructor applied to
  several args folds them into a tuple: `list[A, B]` == `list[(A, B)]` (mirrors
  the old `('a, 'b) list`). User types already collapse N args into one tuple
  formal in the typechecker; the parser now does the same for the builtins
  list/vector/rrbvec/ref.
- **No expression-position instantiation** (`f[T](x)`): deferred indefinitely
  (would need parser disambiguation vs indexing; the annotation channels cover
  every use today). The bracketed notation is what makes a future `f[T](x)` even
  possible.

The internal `'`-prefixed convention for **auto-generated** type variables
(untyped parameters `'targ`, `__var_tuple__`, `__var_array__`) is unchanged --
only the *source* `'t` was removed.

## How it was staged

Fold-1's keyword-reform playbook, minus the temp keyword (in type position the
two notations coexist without a grammar conflict, so no staging keyword was
needed):

0. **Census** (`docs/generics_census.md`): ~2170 tyvar sites, lib-dominated; the
   compiler is nearly monomorphic. The uppercase-collision watchlist came out to
   essentially ONE site (`test_basic.fx` variant params vs `A`/`B`/`C`
   constructors).
1. **Parser accepts the new notation** alongside the old. `apply_typ_targs` /
   `parse_typearg_list_` reinterpret a bracketed application over the parsed head
   (mirroring the postfix path so both notations erase to identical `typ_t`);
   `parse_bracket_tyvars_` reads declared `[T,...]` lists; `reg_deffun` seeds the
   declared params into its env + collected set. **Both notations produce
   byte-identical `typ_t`** (verified by AST diff -- the migrator's oracle).
2. **The migrator** (`tools/generics_migrate.py`, single-use, removed at the
   flip): compiler-assisted and span-based. Because `typ_t` carries no location
   (FB-020), parsing under `-pr-generics-sites` emitted per-span edit records
   (ANNO / FUNPARAMS / TYPEHEAD) that the script applied textually; `--check`
   verified the `-pr-ast0` type structure was unchanged before/after.
3. **Migrate the corpus** in batches (stdlib/tests/examples by Vadim, then
   compiler/*.fx via the migrator). After migrating the compiler's OWN sources
   the bootstrap sat at a clean fixpoint -- **zero generated-C change**, the
   strongest possible proof the notation is purely surface. A whole-tree
   migrator sweep then filled the postfix applications the hand pass had left.
4. **The flip**: old notation removed, parser rejects it with the three targeted
   errors above; `typ2str` switched to the new notation; all scaffolding deleted.
   Stragglers the sweep had skipped (files needing special flags / not
   standalone-compilable) were migrated by hand; a whole-tree scan confirms zero
   old-notation sites remain.

## Acceptance / verification

- Full fxtest ladder green at every phase (unit, negative, T4 IR, cfold, corpus
  O0/O3). Bootstrap fixpoint holds throughout; the migrated stdlib/compiler `.c`
  is byte-identical to pre-migration (notation erases -- T2 corpus exact match).
- Directed test `test/test_generics1.fx` (10 cases) exercises every context:
  generic functions, multi-param records, self-referential variants, generic
  classes with methods, body-local annotations, 2-D array params, qualified
  multi-param application, the inference-annotation channel, generic operators.
- Negative goldens 012/013/207/703/706 and T4 :ast snapshots regenerated for the
  new-notation diagnostics; :k0/:k snapshots unchanged (erasure past parsing).

## Bugs

- **FB-029 FIXED** as a side effect: Map's emptiness-check overload went from the
  malformed `empty(s: 't t)` to `empty[K, D](m: Map.t[K, D])`.
- A **compiler gap surfaced and fixed**: methods of a generic class did not see
  the class's type params in the new notation (the old notation harvested them
  from the `self: ('k,'d) t` type via the `'`-scan; the new uppercase params are
  not `'`-prefixed). `reg_deffun` now binds the class's `dvar_templ_args` into
  the method's seed explicitly.
- No new fenced bugs. (A suspected duplicate-`fx_init_List` link error was a
  false alarm -- a stale old-notation `List.fx` in a scratch dir shadowing the
  stdlib module, not a compiler bug.)

## Out of scope (as planned)

The cast reform (`(x :> T)` -> `T(x)`), `Module.(op)` grammar, and anything
expression-position (`f[T](x)`) -- separate later constructs.
