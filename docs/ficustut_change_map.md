# ficustut.md — reform change-map (approve before the rewrite)

Source: 4474 lines, ~50 sections. Depth legend: **S** = surface (notation in
examples/prose, meaning intact) · **M** = section partially rewritten ·
**L** = section rewritten · **NEW** = section/chapter added. Untouched
chapters are listed at the end. Reforms deliberately NOT reflected (language
unchanged): casts `:>` (13 sites — stay), `.op` family (D3 pending),
backtick forms (macros pending), records/modules syntax (D4 pending).

| Section (lines) | Changes | Depth |
|---|---|---|
| Usage (59–94) | Add: `-Wall` / `-Werror` / `-Wimplicit-rettype` (one paragraph, discipline knob, not a requirement); `-diag-format=json` one-liner; multiple-diagnostics-per-run behavior note | M |
| Tokens (213–424) | Apostrophe no longer starts a type-var token (now: compiler-generated names in dumps only); `fp16`/`bf16` join the type keywords; `vector`/`rrbvec` naming | S |
| Values and Variables (424–545) | `[]` is "some collection"; pin via annotation or `list([])`/`vector([])`/`array([])`/`rrbvec([])`; unpacking section gains a cross-ref to tuple ASSIGNMENT (new, see Tuples) | M |
| Types (649–988) | All type-var notation → `[T]`-style; `T?`; `ref[T]`; postfix arrays keep (quote drops); the four-container lineup introduced here briefly | M |
| Code Blocks & Control Flow (988–1360) | `break`/`continue`/bare `return` legal as match-arm / if-branch values (throw-like), with a short example; while/for text intact | M |
| **Folding (1360–1420)** | **Full rewrite**: imperative body (`s += x`, assignments), accumulators are variables, tuple accumulators = several vars, `break`/`continue` legal, `val fold` shortcut kept; bounding_box example becomes the showcase (branch assignments instead of the else-arm returning an unchanged 4-tuple — the reform's best advertisement) | **L** |
| Special fold-like ops (1420–1479) | Text stands (all/exists/find/filter exist as-is); examples restyled | S |
| Comprehensions (1479–1601) | fold-in-comprehension examples restyled; `[]` note | S |
| Functions (1601–1856) | NEW subsection "Overload resolution": least-generic-wins, ambiguity error + qualified call `Module.__op__`, keywordless-beats-defaulted; "Implicit conversion of argument types" (currently 4 lines) absorbs the wrap-in note; "Using return for earlier exit" updated (bare `return` parses; expression positions) | M |
| **Numbers (1856–1911)** | **Expand** (~55 lines today): signed overflow wraps (defined, `-fwrapv`); `>>` arithmetic/logical by signedness, shift count `int`; `floor/ceil/trunc/round → int` by design; `fp16`/`bf16` (storage, float-promoted compute, bf16 rounding); unsigned promotion recap | **L** |
| Tuples (1913–2017) | "Modifying tuples" gains **simultaneous tuple assignment** `(a, b) = (b, a+b)`, swap `(x[i], x[j]) = ...`, `_` components — new sub-section | M |
| Records (2017–2146) | `[T]` notation in generic record examples only | S |
| Arrays (2146–2398) | Notation in examples; cross-ref to the container-choice section | S |
| **Memory Management (2398–2473)** | Add the **container semantic model** (§1.8: the O(1)-assignment axiom and the derived table — array/string/rrbvec/vector representations); vector read-lock (structural mutators throw under iteration) explained here or in the Vector chapter (see Q2) | **L** |
| **Generic Programming (2602–2714)** | **Full rewrite of the declaration story**: explicit `fun f[T](...)` / `type name[K, D]` instead of "just write 'ident"; UPPERCASE convention (case free — declaration disambiguates); `### 't [+]` heading → `### T [+]`; the existing `(dilate3x3 : float [,] -> float [,])` example gets PROMOTED as the official instantiation channel (annotation as inference hint) | **L** |
| Lists (2714–2944) | `list[T]` notation; `[]`/`list([])`; cons/patterns intact | M |
| **NEW: Vectors** (place per Q2) | The mutable growable `vector`: construction (`vector(...)` family mirrors `array`), push/pop/back, slice-assignment (`v[a:b] = []`, insertion at `[i:i]`), slices are copies (why — the axiom), iteration + read-lock semantics with the thrown-error example, `rrbvec` as the immutable sibling (renamed; brief) | **NEW** |
| **Choosing between list, vector, array (2944–2953)** | Rewrite the TBD stub for FOUR containers (and fix the "Choosting" typo): decision table — mutability, growth, slicing cost, pattern matching, memory overhead, random vs sequential access | **L** |
| Pattern Matching (2953–3169) | Notation in examples; note: `break`/`continue` as arm values cross-ref | S |
| Variants (3169–3479) | Generic variants → `[T]` declarations (`type tree_t[K, D] = ...`); option `T?` | M |
| Exceptions (3479–3593) | Add: no-payload exceptions propagate from `@parallel` loops (the rule + why) — or in Parallel chapter (Q3) | S/M |
| Modules (3593–3751) | Untouched (naming reform deferred); notation in examples | S |
| OOP (3892–4167) | Notation in examples; text intact | S |
| Parallel Programming (4167–4217) | `@parallel` restrictions recap (fold: named reductions future); exception-no-payload rule if not in Exceptions; read-lock interplay one line | M |
| Stdlib Overview (4304–4390) | `Hashmap/Hashset`, `Map/Set` entries checked against current API; `Json` entry notes the fixed serializer + compact mode; `Vector`→`Rrbvec` module naming | S |
| Appendix A (4390–end) | LSP: one short subsection — `tools/FicusLsp.fx`, editor configs pointer to docs/lsp.md (Q4) | M |

Untouched: Introduction, License, Installation, Expressions, Text Strings,
Regular expressions, Preprocessing (backticks stay documented as-is),
Interoperability with C, Map-reduce concept.

## Questions before the rewrite

- **Q1 (depth of Folding)**: rewrite teaches ONLY the new form (old form gets
  one migration note "bodies used to be value-typed"), or a "changed in 2026"
  box? My default: only the new form, zero legacy — new readers should never
  learn the old one existed.
- **Q2 (Vector chapter placement)**: standalone chapter after Arrays, or a
  section inside a restructured "Sequences" grouping (Arrays/Vectors/Lists)?
  My default: standalone "Vectors" chapter right after Arrays, read-lock
  explained there, Memory Management keeps the axiom/table.
- **Q3 (parallel exceptions)**: rule lives in Exceptions or Parallel
  Programming? Default: Parallel (that's where the reader hits it), one
  cross-ref from Exceptions.
- **Q4 (LSP in the tutorial)**: mention at all (it's tooling, not language)?
  Default: yes, three sentences in Appendix A — it changes the day-1
  experience.
- **Q5 (verification)**: after the rewrite, an Opus session extracts every
  code block and compiles/runs them as `fxtest.py doctut` — approved as a
  follow-up? (Blocks that are deliberately partial get a `// fragment`
  marker convention.)

Process note: I patch section-by-section against your uploaded file and
deliver the complete updated ficustut.md + a per-chapter reviewer summary;
anything I cannot verify against current behavior gets
`<!-- TODO(vadim): verify -->` instead of invention.
