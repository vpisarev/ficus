# Handoff: types-1 — `fp16`/`bf16` + the rrb capability census (Opus)

**You are Claude Code (Opus) working in the Ficus repo.** Branch `types-1`
off master. Two independent deliverables: a small type-name reform (the
first rename of the epoch, using the fold-1 playbook in miniature) and a
sizing report for the container reform. Context:
`docs/language_changes_brief.md` §3.4/§3.5, `docs/fold1_report.md`
(the staging playbook), repo CLAUDE.md.

## WP-1 — `half` → `fp16`, add `bf16`

**Rename** (staged, no temp keyword needed — type names can alias):
1. add `fp16` as the canonical name for today's `half` (lexer/stdlib alias,
   both accepted, `fp16` is what diagnostics/dumps print);
2. migrate the corpus textually (`half` as a TYPE name only — mind
   identifiers/strings/comments mentioning "half" in prose; the census below
   gives the site list);
3. flip: `half` removed, with a targeted "did you mean 'fp16'?" error via
   the existing did-you-mean machinery (it should fire naturally once the
   name is gone — verify, don't special-case unless needed). Two-stage
   bootstrap regen per the fold-1 playbook if the compiler sources mention
   the type (check first — they may not).

**`bf16` (new scalar type)**: mirror the `half` implementation end-to-end —
Vadim's sizing: "достаточно просто", because the half machinery (16-bit
storage, convert-to-float compute, promote rules, printing, literals if any)
generalizes; bf16 differs in the conversion (top-16-bits of float32,
round-to-nearest-even on the way down — get the rounding right, truncation
is NOT acceptable for the default conversion). Scope: storage + conversions
+ float-promoted arithmetic + string/print + array element type + the
numeric coercion table entries. NO SIMD paths, no `.fi`-style literal sugar
unless half already has one (mirror exactly). Directed tests: round-trip
float→bf16→float on boundary patterns (rounding ties, subnormals→zero?,
inf/nan preserved), arithmetic promotes like fp16, arrays of bf16, printing.
Language_changes §3.4 → implemented (leave the snippet in the report).

## WP-2 — the rrb capability census (report only, sizes the container reform)

Question being answered (Vadim): promoting `Dynvec.t` to a first-class
`vector` means building a layer SYMMETRIC to what rrb vectors already have —
how big is that layer?

Method: `grep -ri rrb` (case-insensitive) across `runtime/ compiler/ lib/
test/` + read `lib/Vector.fx` and the runtime impl. Deliverable
`docs/rrb_census.md`: a capability × location table —

| capability | rrb: where implemented (runtime / compiler pass / stdlib) | Dynvec today | gap for first-class vector |

covering at least: element access (incl. border modes `.clip/.wrap/.zero` —
CLAUDE.md says Vector has them), slices, comprehensions (`[< ... >]`? verify
the literal/comp syntax), writers (used by vector comprehensions), pattern
support (expected: none), iteration/for support, `string()`/print/hash
autogen, `[]`/TypVarCollection membership, `+`/concat and other operators,
serialization hooks if any. Plus LoC counts per layer (runtime C vs compiler
vs stdlib) — the honest size of the symmetric work. End with a one-paragraph
recommendation skeleton (facts only, the decision is Vadim's): what a
minimal-viable first-class vector needs vs what can lag.

## Ground rules

Standard ladder per WP; `update_compiler.py` fixpoint; `-pr-resolve` census
unchanged; rettype gate stays clean (new bf16 stdlib functions get return
annotations); found_bugs for anything discovered; don't push. Report
`docs/types1_report.md`. Container RENAMES (`vector→rrbvec`,
`Dynvec.t→vector`) are explicitly OUT of scope — they wait on the census +
Vadim's decision, and must precede the generics notation migration when they
come.
