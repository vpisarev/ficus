# lsp-1 report — structured diagnostics + `-diag-format=json` (Phase 0 + 0.5)

Branch `lsp-1` off master. Brief: `docs/lsp1_brief.md`. Plan checkpoint: **Phase 0
(compiler json) + Phase 0.5 (capability tests + stdlib gap-fills) done, full
ladder green, not pushed.** The Ficus server (Phase 1) and editor smoke (Phase 2)
are the next checkpoint. This document is the review artifact for the first two
phases.

## The Phase-0 finding that reshaped the work

The brief assumed diag-1 left "one printer carrying structured `(loc, precision,
severity, message, suggestions)`" where json would be a print-time switch. It did
not. Recon (three parallel explorers over the diagnostic subsystem) found the
diagnostic model is **string-baked at raise time**:

- `CompileError` was `(loc_t, string)` where the string is the whole human line —
  `"{loc}: error: {msg}"` + the caret excerpt (`loc_excerpt`) + the
  `\n\twhen instantiating…` context tail — all flattened in `Ast.compile_err`.
  The terminal printer just `println`s it.
- **Severity** is the literal word `error:`/`warning:`; there was no field.
- **"did you mean"** is string-concatenated into the message by callers.
- **Warnings** take a *different* path (`compile_warning` prints immediately;
  never enters the accumulate-then-print queue), plus ~6 other raw `println`
  diagnostic sites (lexer/parser errors, parser warning, top-level catch).
- **Precision** is binary (frontend caret vs none), not the brief's ternary.

That is the brief's own STOP condition ("if emission turns out decentralized,
report the sites"). Reported to Vadim; decision: do the **structured-diagnostic
refactor** (the right foundation) rather than a print-time hack.

## Phase 0 — what shipped

**Structured twin (`compiler/Ast.fx`).** A `diag_t {severity, raw_msg,
suggestions, precision, context}` now rides alongside the human string on the
exception: `CompileError(loc, human_str, diag_t)`. Human mode prints `human_str`
(byte-identical — all 92 pre-existing negative goldens unchanged); json mode
serializes the `diag_t`. Precision is captured at raise time (`diag_precision()`:
frontend→`exact`, else `anchor` when a context tail exists, else `line`), because
`compiler_stage` advances as the driver runs.

**Suggestions as a first-class array.** `suggest_similar` was split into
`suggest_similar_names` (the candidate `list[string]`, also fed to `diag_t` for
editor quickfixes) + `suggest_tail` (the human "; did you mean 'x'?" text). The
two typecheck did-you-mean sites now pass `suggestions=…` to `compile_err`; the
human tail is unchanged.

**The flag (`compiler/Options.fx`).** `diag_format: string = "human"` on
`options_t` (compile-time-only layout change → regenerates bootstrap, generated C
for programs unchanged), parsed as `-diag-format=json|human` (validated).

**The json emitter (`compiler/Ast.fx`).** `diag2json` emits one JSON object per
line with a **hand-rolled RFC-8259 escaper** (`json_escape_str`) — no `lib/Json.fx`
dependency added to the compiler, per the brief. Errors still dedup/sort/cap via
`print_all_compile_errs`; the human **summary lines are suppressed** in json mode
(the client counts). Warnings emit their json inline. Lexer/parser errors
(`Compiler.fx`) grew a json branch (`emit_error_json`) while keeping their exact
human form.

### The jsonl, for one diagnostic

```
$ bin/ficus -no-c -diag-format=json test/negative/250_json_did_you_mean.fx
{"file": "…/250_json_did_you_mean.fx", "line0": 8, "col0": 9, "line1": 8, "col1": 15, "precision": "exact", "severity": "error", "message": "the appropriate match for 'lenght' of type '<unknown>' is not found; did you mean 'length'?", "suggestions": ["length"]}
```

The span (col 9–15) is exactly the `lenght` identifier — a clean quickfix target —
and the candidate is in the structured `suggestions` array. A warning maps to
`"severity": "warning"`; a parse error takes the `emit_error_json` path with an
exact frontend span. Three directed goldens lock the schema
(`test/negative/250_json_did_you_mean`, `251_json_warning`, `252_json_parse_error`,
each `// flags: -diag-format=json`).

## Phase 0.5 — capability report

1. **Byte-exact stdio.** `File.read(f, uint8[N])` is byte-exact; the two missing
   halves were added to `lib/String.fx`: `utf8_length` (encoded byte count for
   `Content-Length`, via the newly-exported `fx_str2cstr_size` — cheaper than a
   full cstr copy) and `from_utf8` (decode an in-memory `uint8[]`, complementing
   `File.read_utf8` which only takes a filename). Directed tests in
   `test/test_utf8io.fx` (N bytes ≠ N code points; multi-byte decode; framing
   invariant).
2. **Json.fx coverage.** Serializer bug **fixed** (own concern, MF-001): the old
   `repr()` added quotes without escaping `"`, `\`, or control chars → invalid
   JSON for realistic payloads. Now RFC-8259-correct for values *and* keys, plus
   a compact/minified mode `Json.string(js, compact=true)`. Directed tests in
   `test/test_json.fx` (escaping, `\u00XX` controls, compact single-line,
   parse→serialize idempotence). The **surrogate-pair parse gap** is logged
   (MF-001) and deliberately not fixed (it touches the shared lexer).
3. **Child process + exit code.** Proven end-to-end (the server's core primitive):
   `File.popen("bin/ficus -no-c -diag-format=json <file>", "r")` → drain jsonl via
   `readln` → `pclose_exit_status`. Error file → exit 1 + one jsonl line; clean
   file → exit 0 + zero lines.

No blockers beyond small additions → no STOP; the Ficus server remains the
default (Python fallback unused).

## A latency win folded in (Vadim)

`Compiler.fx` now skips `k_optimize_all` (the middle-end) when `-no-c` is set and
neither `-pr-k` nor codegen is requested — a diagnostics run stops right after
K-normalization (frontend), significantly speeding the analysis path the LSP
drives. It also keeps `compiler_stage` at frontend for every diagnostic we care
about (exact carets).

## Verification

- `fxtest all` (unit **204** + negative **95** + ir + cfold + corpus O0/O3) PASS;
  determinism 3/3; sanitize clean.
- **Human negative goldens byte-identical** (the refactor's core invariant).
- **Bootstrap fixpoint holds** — the `options_t`/`diag_t` layout churn regenerated
  38 modules (compile-time-only class); the stdlib helpers are DCE'd from the
  compiler build as unused, and the `ficus.h` declaration doesn't alter generated
  C. T2 corpus exact-match confirms program C is unchanged.

## Deferred to the next checkpoint

Phase 1 (`tools/FicusLsp.fx`: JSON-RPC/stdio framing, publishDiagnostics,
codeAction from `suggestions`) and Phase 2 (editor smoke + `docs/lsp.md`). Open
items: MF-002 (thread parser did-you-mean into `suggestions`), MF-001
(surrogate pairs), and v1.5 unsaved buffers via `-substitute-file`.
