# RRB vector capability census (types-1 WP-2, report-only)

**Question (Vadim):** promoting `Dynvec.t` to a first-class `vector` means
building a layer *symmetric* to what RRB vectors already have — how big is that
layer? This census maps capability × location and counts the LoC, so the
container reform (§3.5 of `language_changes_brief.md`) can be sized. Facts only;
the decision is Vadim's. Container renames (`vector`→`rrbvec`,
`Dynvec.t`→`vector`) are out of scope here.

Terminology: **rrb** = today's immutable `vector` (relaxed-radix-balanced tree,
`runtime/ficus/impl/rrbvec.impl.h`, derived from hypirion/c-rrb). **Dynvec** =
`lib/Dynvec.fx`, a mutable array-doubling buffer, the promotion candidate.

## 1. LoC per layer

| Layer | File(s) | size |
|---|---|---|
| Runtime C — the RRB engine | `runtime/ficus/impl/rrbvec.impl.h` | **1255 LoC** (+ ~80 lines of types/macros in `ficus.h:1030-1109`) |
| Compiler — codegen | `compiler/C_gen_code.fx` | ~48 vector/rrb lines (access/border/slice/concat/size/iterate/write) |
| Compiler — typecheck | `compiler/Ast_typecheck.fx` | ~30 vector lines |
| Compiler — other passes | Ast, Ast_pp, K_form, K_normalize, K_mangle, K_annotate, C_form, C_gen_types, C_gen_std, Parser | ~40 lines total (2-6 each) |
| Stdlib — operators | `lib/Vector.fx` | **44 LoC** (broadcast/compare operators only) |
| Stdlib — string/repr/print | `lib/Builtins.fx` | ~40 LoC (3 funcs) |
| **The candidate** | `lib/Dynvec.fx` | **89 LoC** (mutable buffer; a plain user `class 't t`) |

The RRB engine dominates (1255 LoC of C) but it is *reused as-is* — the reform
question is the **~120 lines of compiler wiring** + **~85 lines of stdlib** that
make rrb a first-class type, which Dynvec would need mirrored (against a
mutable, not persistent, backing).

## 2. Capability × location (rrb vs Dynvec)

| Capability | Where rrb implements it | Dynvec today | Gap for first-class `vector` |
|---|---|---|---|
| Element access `v[i]` | typecheck `Ast_typecheck.fx:2622` → codegen `C_gen_code.fx:2387` `FX_RRB_ELEM` → runtime `fx_rrb_find` | manual `v.data[i]`; no `[]` sugar | add `[]` access |
| Border `.clip/.wrap/.zero` | generic `border_t` (Ast.fx:257) → codegen `C_gen_code.fx:2398` `FX_RRB_ELEM_{CLIP,WRAP,ZERO}` → `fx_rrb_find_border` | none | wire border codegen (AST enum is generic) |
| Slices `v[a:b:s]` | typecheck `Ast_typecheck.fx:2625` → codegen `C_gen_code.fx:2429` `fx_rrb_slice`/`fx_rrb_inverse` | none | build |
| Comprehension `vector(for…)` | Parser.fx:573-593; `ExpMap`+`ForMakeVector`; typecheck `Ast_typecheck.fx:2732`; writer codegen `C_gen_code.fx:3020` | none | build (incl. a `ForMake*` variant) |
| Literal `vector([e0,…])` | `ExpMkVector` (Ast.fx:305); typecheck `Ast_typecheck.fx:2940` (supports `\e` splat). **Not `[< >]`.** Empty literal **rejected** (K_normalize.fx:350) | `Dynvec.create(...)` call only | build literal syntax |
| Writer protocol | `FX_RRB_START_WRITE`/`WRITE`(`_FAST`)/`END_WRITE` (C_gen_code.fx:3020) → runtime | `push` is the analog, not a compiler protocol | build writer hook if comprehensions wanted |
| Iteration `for x <- v` | codegen `C_gen_code.fx:1204` `fx_rrb_start_read`+`FX_RRB_NEXT`, size `FX_RRB_SIZE` | manual `for i <- 0:size(v)` | build iterator hook |
| `+` / concat | typecheck `Ast_typecheck.fx:2192` → codegen `C_gen_code.fx:1589` `fx_rrb_concat` (O(log n)) | none (`push(v,arr)` closest) | build operator |
| Elementwise/compare ops | `lib/Vector.fx` (44 LoC, sugar over `vector(for…)`) | none | build stdlib |
| `string`/`repr`/`print` | `lib/Builtins.fx:370,408,1127` (generic dispatch, char-vector @ccode) | none | add overloads for the type |
| `[]` / TypVarCollection | member (Ast_typecheck.fx:223-240; `[]` at K_normalize.fx:98) | ordinary class, **not** a collection | add to `TypVarCollection` + `[]` set |
| Compiler type + lifecycle | `TypVector`/`KTypVector`/`CTypVector`; mangling `"V"`; ctprops `fx_rrb_{make,copy,free}` (C_gen_types.fx:136) | inherits generic class free/copy | add a real compiler type node at every layer |
| `size()` | `FX_RRB_SIZE` | `Dynvec.size` | parity |
| flatten `[:]` | typecheck `Ast_typecheck.fx:2556` | none | optional |
| Pattern match | **none** (no `PatVector`/`PatArray`/`PatList`) | none | neither — nothing to build |
| Type-specific hash | **none** | none | neither |
| Serialization/File hooks | **none** | none | neither |

## 3. Runtime primitives already present (`rrbvec.impl.h`, reusable)

`fx_rrb_find` / `fx_rrb_find_border` (access, border), `fx_rrb_start_read`/
`fx_rrb_next` (iterate), `fx_rrb_start_write`/`write`/`end_write` (build),
`fx_rrb_make`/`make_empty`/`copy`/`free` (lifecycle, refcounted nodes),
`fx_rrb_append`/`push_tail` (append), `fx_rrb_concat` (+ rebalance/plan helpers,
O(log n)), `fx_rrb_slice`/`fx_rrb_inverse` (slice incl. negative stride),
`fx_rrb_check_tree` (debug).

## 4. Recommendation skeleton (facts; decision is Vadim's)

A minimal-viable first-class `vector` (promoting Dynvec) must build, in order of
how load-bearing they are: (1) a **compiler type node** at every layer
(`TypVector`-equivalent + K/C forms + mangling + ctprops) — today Dynvec is a
plain `class` handled by generic record machinery, so this is the foundational
new surface; (2) **`TypVarCollection` + `[]`-membership** so `[]` and generic
collection code accept it; (3) **element access `[]`, iteration (`for`), and
`size`** — the everyday read surface; (4) **literal + comprehension syntax**
(a `ForMake*` variant and the writer protocol) and **`string`/`print`**. Can
lag: **slices, border modes, `+`/concat, elementwise operators, flatten** —
each is independent and additive. Never needed (neither type has them): pattern
matching, type hash, serialization.

**Key asymmetry to decide first:** rrb `vector` is **immutable/persistent**
(every op returns a new tree; no in-place element write path exists), whereas
Dynvec is **mutable** (in-place `push`/`pop`, `v.data[i]=…`). A symmetric
promotion of Dynvec introduces an *in-place mutation* surface that rrb never
had — so "symmetric to rrb" is not literal: the read/build/iterate protocols
mirror, but the write model is new. That mutation model (does first-class
`vector` expose `v[i] = x` and grow-in-place, and how does that interact with
the refcount/copy-on-assign runtime?) is the design question that gates the
size of the rest.
