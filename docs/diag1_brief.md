# Brief: diag-1 (session-2) — type-error recovery, multiple diagnostics, caret rendering

**You are Claude Code working in the Ficus repo.** Branch `diag-1` off master.
Context: `docs/language_changes_brief.md` §1.7 (closed signatures = the
recovery firewall) and §6; `docs/ctrlflow1_report.md` (TypErr mechanics
confirmed end-to-end); `docs/annotate2_report.md` (the warning subsystem this
builds on); repo `CLAUDE.md`. This is the LSP-groundwork session: after it,
one compiler run reports MANY useful diagnostics instead of dying on the
first, with gcc/clang-style source excerpts.

## Assets already in place (do not rebuild)

- **`TypErr` is a battle-tested poison type**: it unifies with anything
  WITHOUT binding the other side (`Ast_typecheck.fx` ~341: `TypErr => {}`),
  and the whole `throw`/ctrlflow-1 machinery rides it. Recovery = report +
  return `TypErr` + continue, NOT a new mechanism.
- **The warning subsystem** (annotate-2): counted non-fatal diagnostics,
  end-of-run summary, `-Werror` promotion. Errors join the same accounting.
- **Declared signatures stdlib-wide** (annotate-1/2): a definition whose BODY
  fails can keep its declared interface, so callers typecheck meaningfully —
  the firewall that makes multiple errors *useful* rather than cascade noise.

## Design (settled with Vadim)

- **Structural cascade suppression is the primary mechanism, limits are the
  backstop.** An error is reported only if its operands/context are
  TypErr-free; TypErr itself flows silently. `val x = undefined; val y = x+1;
  f(y)` = exactly ONE diagnostic.
- **Recovery granularity, two tiers**:
  - Tier 1 (required): per top-level definition. Wrap each definition's check
    (the `check_eseq` item loop at module level) so a `CompileError` records
    the diagnostic, poisons the defined symbol (declared signature if present
    — the §1.7 dividend; `TypErr` otherwise), and moves to the next
    definition.
  - Tier 2 (evaluate, implement if the cost is honest): per statement/
    expression *inside* a body — continue past a failed statement within the
    same function. Report the audit in the final doc: how many raise sites /
    what invariants (env consistency after a failed `val` — bind the pattern
    vars to TypErr so later uses don't produce spurious "undefined"). If Tier
    2 turns invasive, land Tier 1 + the audit and stop.
- **Limits (all three as options, defaults):** `-fmax-errors=100` global
  (gcc-style; final line "further diagnostics suppressed"), 20 per module,
  5 per function. Structural suppression should make the caps rarely bind;
  the per-function cap exists for pathological bodies, not as the mechanism.
- **Ordering & dedup**: diagnostics sorted by (file, line, col) at emission
  per module; exact-duplicate (loc, message) reported once.
- **Exit semantics unchanged**: any error → exit 1 (after the full report);
  `-Werror` unchanged.

## Phases

### Phase A — exploration (no code)
- Map every `throw CompileError` raise site class in `Ast_typecheck.fx`
  (unify failures, lookup failures, pattern checks, ...) — the Tier-2 cost
  estimate. Map how `check_eseq` iterates module definitions (Tier-1 seam).
- **`loc_t` contents**: does it carry columns (begin/end)? Do lexer token
  locs? How are source lines retrievable (held in memory vs re-read)? And:
  classify the existing diagnostic population by EMITTING STAGE
  (parser/typecheck vs K-form vs C-gen) — Phase D's precision tiers hang off
  this census.
- Which legality checks currently fire at C-gen stage (e.g. `'continue' may
  not be used inside parallel for`, per ctrlflow-1) and which of those are
  liftable to typecheck (pure nesting facts) vs genuinely lowering-dependent.

### Phase B — recovery core (Tier 1, then Tier 2 per the audit)
Per the design above. Poisoned-definition rule: with a declared return type,
the signature stands (callers check against it); without one, the symbol
types as TypErr (callers stay quiet — suppression, not cascade). This is
where §1.7's "annotations are firewalls" becomes executable truth.

### Phase C — diagnostic quality
- **Edit-distance "did you mean"** on not-found identifiers/fields/modules:
  Levenshtein over the visible candidates (env names of the right namespace),
  threshold scaled by length (≤1 for short names, ≤2 longer), at most 2
  suggestions: `error: 'lenght' is undefined; did you mean 'length'?`.
  Reuse `possible_matches` where resolution already collects it.
- **Lift liftable legality checks** from Phase A's list into typecheck (each
  becomes golden-able and `-no-c`-visible); leave the genuinely
  lowering-dependent ones with a note.
- Keep the resolve-1 candidate listings (`fun2sigstr`) untouched — they are
  already the good format; new messages match their style.

### Phase D — caret rendering with STAGE-HONEST precision (the todo-2026 item)

Location trustworthiness is a function of the reporting STAGE (Vadim): AST
locations (parser/typecheck) are accurate by construction; K-form/C-form
locations are inherited through normalization/inlining/fusion and DRIFT,
sometimes badly. A confident caret on a drifted location is worse than no
caret (false precision sends the user to an innocent line). Therefore the
diagnostic printer takes `(loc, precision)` and each emitting stage declares
its honesty level:

- **`exact` (parser/typecheck, ~95% of user-facing diagnostics)**: full
  gcc/clang-style excerpt — line-number gutter, source line, `^` at the
  column (`^~~~` span if loc carries an end). Tabs expanded consistently.
  Phase A verifies what `loc_t`/lexer token locs actually carry; if columns
  are missing, implement the line-excerpt form now and file column plumbing
  as an estimated follow-up — do not silently drop the phase.
- **`line` (K-form stages)**: `file:line` + the source line, NO caret; plus a
  **semantic anchor**: `in function 'foo'` (demangle `name@NNN` gensyms to
  the source name), and where cheaply available `near the call to 'bar'` /
  the involved variable's name — names survive K transformations, coordinates
  do not.
- **`anchor` (C-gen stage)**: semantic anchor + `file:line` presented as
  approximate. No excerpt.

Explicit non-goal: do NOT attempt to fix location propagation through K
optimizations in this session — the format admits the drift instead. Note:
Phase C's lifting of legality checks into typecheck and resolve-2's move of
the `[]` diagnostic already shrink the K/C-stage diagnostic population; the
remainder is rare enough that the anchor form is adequate. One shared
implementation in the printer serves errors AND warnings.

### Phase E — tests
- Directed multi-error suites: N independent errors in one module → N
  diagnostics, one run; a cascade chain → exactly one; a broken body behind a
  declared signature → callers clean; a broken body with NO declared
  signature → callers silent (suppressed, not cascaded).
- Negative-golden format migration: existing goldens gain the caret excerpt —
  this is a BULK golden update; make the harness comparison caret-aware
  deliberately (update `fxtest` T3 to compare full multi-line diagnostics),
  and review the diff as a format change, not noise. New goldens for:
  multi-error output (ordering locked), max-errors truncation line,
  did-you-mean, each lifted legality check.
- The ladder + determinism + census (`-pr-resolve` unchanged) + bootstrap
  fixpoint; sanitize leg. Diagnostics are compiler OUTPUT only — generated C
  for compiling programs must be byte-identical (bootstrap diff = edited
  modules only).

## Ground rules

Standard (CLAUDE.md): ladder per phase, `update_compiler.py` fixpoint, census
unchanged, found_bugs for anything discovered, don't push. STOP signals: if
Tier-2 recovery demands restructuring `maybe_unify`/`unify_` themselves or
touching K-form and below, stop and write it up — recovery lives strictly
above unification. Final report `docs/diag1_report.md`: the Phase-A maps
(raise-site classes, loc_t verdict, liftable checks), tier-2 decision with
numbers, before/after sample outputs (one screenshot-style block), CLAUDE.md
diff, and the open follow-ups (column plumbing if deferred; LSP notes — what
of this session's machinery the LSP will consume directly).
