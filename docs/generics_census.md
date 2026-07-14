# generics-1 — notation census (Phase 0)

Sizing the `'t`-notation migration and, critically, producing the **shadowing
watchlist** for the uppercase-parameter scheme. Method: a rough tokenizer
(`strip_noise` — drops `//`, `/* */`, `"..."`, char literals) over
`lib/ compiler/ test/ examples/ tools/`, then `'ident` tokens classified by
line-shape. Counts are approximate (line-shape heuristic; string/comment
stripping is best-effort) — the distribution and the watchlist are the reliable
outputs, not the last digit.

## Volume

| area     | clean tyvar tokens |
|----------|--------------------|
| lib      | ~1960              |
| test     | ~165               |
| examples | ~38                |
| compiler | few dozen tokens; **~4–15 real generic definitions** |

The compiler is **nearly monomorphic** — it operates on concrete AST types
(`exp_t`, `typ_t`, …), so most of its ~440 raw apostrophes are inside string
literals / doc comments (e.g. code examples, tyvar names built as strings). Real
generic sites: `Ast.ref2str(r: 't ref)`, `Lexer.add_pair(x: 't, defval: 't)`,
`Ast_typecheck.find_next_(...): 't?`, `search_path(...): 't?`, and a handful
more. **Implication: the risky self-build batch of Phase 3 is tiny.**

## Context split (line-shape heuristic, whole corpus)

| context        | share |
|----------------|-------|
| fun signature  | ~89%  |
| body-local     | ~7%   |
| type def       | ~3.5% |
| val/var anno   | ~1%   |

Type parameters overwhelmingly live in **function signatures** — i.e. the
migration is dominated by synthesizing `[params]` lists for generic functions
(which have no declaration site today), not by type definitions (which already
carry an explicit param list the migrator only reshuffles).

## Name distribution → uppercase mapping

| src   | count | → | src   | count | → | src    | count | → |
|-------|-------|---|-------|-------|---|--------|-------|---|
| `'t`  | ~1480 | `T` | `'r` | 54  | `R` | `'t2` | 25 | `T2` |
| `'k`  | 182   | `K` | `'b` | 48  | `B` | `'t1` | 22 | `T1` |
| `'d`  | 116   | `D` | `'ta`| 38  | `Ta`| `'s`  | 10 | `S` |
| `'t3` | 61    | `T3`| `'tb`| 38  | `Tb`| `'acc`| 8  | `Acc`(hand) |
| `'a`  | 60    | `A` | | | | tail: `'rt 'ts 'z 'idx 'sq 'res 'm 'w 'x 'u 'from 'to` (≤4 each) |

Rule (from the plan): single letter → single uppercase; numbered `'tN`→`TN`;
paired `'ta`→`Ta`; descriptive singletons (`'idx 'sq 'res 'acc 'from 'to` and
`'elem_type 'axes 'prefix1` from earlier probe) hand-reviewed (keep, or shorten
to `T`/`E`).

## Postfix-application heads (real type ctors; approximate counts)

`list` (~236), `tree_t` (~97, `('k,'d) tree_t` / `'t tree_t`), `rrbvec` (~71),
`complex`/`cplx` (~67), `vector` (~41), `array` (~35), `cmp_t` (~24), `ref` (~7),
`option` (via `'t?`), plus user types (`hashentry_t`, `hashset_entry_t`, …).
Multi-param heads: `('k,'d) tree_t`, `('a,'b) ct`, `('k,'v) Map.t`.

## Shadowing watchlist (the migration-risk output)

Collision surface for **uppercase P1** params, checked exhaustively:

- **Single-uppercase concrete TYPE names**: none. (Types are lowercase; `type t`
  only in `Json.fx` with no `'t`; `K_form.fx:20 type i=int` is inside a comment.)
- **Single-uppercase MODULE files**: none.
- **Single-uppercase EXCEPTIONS**: none.
- **Single-uppercase variant CONSTRUCTORS**: `A`/`B`/`C` — appear in exactly one
  definition: `test/test_basic.fx:250`
  `type ('a,'b) ct = A: ('a,'a) … | B: ('b,'b) … | C: ('a,'b) ct`.
  Post-uppercase, params `'a,'b`→`A,B` collide (visually; P1 keeps them
  namespace-distinct and the compiler is correct) with constructors `A,B,C` of
  the same type. **Action: rename the params at this site** (e.g. `ct[T,U]`).
  (`C` in `lib/NN/OpNN.fx` is a *value* — channel count — and that module has no
  `'c`; no collision.)

**Net: exactly one hand-rename** (a test file), otherwise the migration is a
pure mechanical quote-drop + uppercase. Confirms the brief's ~0 expectation, and
uppercase drops residual risk further (values lowercase, params uppercase never
even look alike).
