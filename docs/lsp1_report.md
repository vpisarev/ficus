# lsp-1 report — structured diagnostics + `-diag-format=json` (Phase 0 + 0.5)

Branch `lsp-1` off master. Brief: `docs/lsp1_brief.md`. Status: **all four phases
done — Phase 0 (compiler json), 0.5 (capability tests + stdlib gap-fills), 1 (the
Ficus server), 2 (editor smoke + docs). Full ladder + lsp leg green, not pushed.**
This document is the review artifact.

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

## Phase 1 — the server (`tools/FicusLsp.fx`)

A minimal LSP server **written in Ficus**, `-Wall`-clean, ~330 lines. JSON-RPC 2.0
over stdio with `Content-Length` framing built on the Phase 0.5 byte-exact
primitives (`read_exact` = `File.read(uint8[N])` + `String.from_utf8`; writes size
the header with `String.utf8_length`). `Re` parses the `Content-Length` header.

**Design.** A single-threaded synchronous loop: each analysis
(`File.popen` → drain jsonl → `pclose_exit_status`) runs to completion (~0.13 s)
before the next message is read, so an unbounded pileup of compiler processes is
**impossible by construction** — the brief's requirement met without a kill/debounce
mechanism, because there is never more than one child in flight.

**Protocol subset.** `initialize`/`initialized`/`shutdown`/`exit` (exit code via
`throw Exit(shutdown ? 0 : 1)`); `textDocumentSync {openClose, save}`;
`textDocument/didOpen`+`didSave` → analyze → `publishDiagnostics`;
`textDocument/codeAction` → a quickfix per suggestion; `didChange` accepted+ignored
(v1 saves); `didClose` clears. Diagnostics convert Ficus 1-based `[col0,col1)` →
LSP 0-based; `line`/`anchor` precision → whole-line range + anchor in the message.
Suggestions ride each diagnostic's `data` field, so codeAction is **stateless**
(reads them back from the client-echoed `context.diagnostics`). A nonzero child
exit with no parseable jsonl → one internal-error diagnostic (crash-safe); any
diagnostics (errors OR warnings) publish regardless of exit code; a clean run with
none clears the file.

### One request/response over the wire

```
--> {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///…/typo.fx",…}}}
<-- {"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"uri":"file:///…/typo.fx","diagnostics":[
      {"range":{"start":{"line":1,"character":8},"end":{"line":1,"character":14}},
       "severity":1,"source":"ficus",
       "message":"…for 'lenght'…; did you mean 'length'?",
       "data":{"suggestions":["length"]}}]}}
--> {"jsonrpc":"2.0","id":2,"method":"textDocument/codeAction","params":{…,"context":{"diagnostics":[<that diag>]}}}
<-- {"jsonrpc":"2.0","id":2,"result":[
      {"title":"Change to 'length'","kind":"quickfix","edit":{"changes":{"file:///…/typo.fx":[
        {"range":{"start":{"line":1,"character":8},"end":{"line":1,"character":14}},"newText":"length"}]}}}]}
```

**Tests** (`tools/fxtest/lsp_driver.py`, Python-stdlib `unittest`, wired as
`fxtest.py lsp`): typo → one diagnostic with the exact range + one quickfix; fix →
cleared diagnostics; warning file → severity 2; garbage notification → server
survives; missing compiler → internal-error diagnostic. 6/6 pass; the leg builds
the server into `build/fxtest/` (nothing stray in the tree).

## Phase 2 — editor smoke + docs

`docs/lsp.md`: working neovim (`vim.filetype.add` + `vim.lsp.start`) and Emacs
(eglot) configs, what works in v1, known limitations (save-only; code-point vs
UTF-16 columns; ASCII-only percent-decoding), and the v1.5/v2 roadmap.

## Deferred / open items

- **MF-002** — thread the parser's module did-you-mean into `suggestions` (it is
  message-only today; import quickfixes want it structured).
- **MF-001** — Json.fx surrogate-pair *parse* combining (shared lexer change).
- **v1.5** — unsaved buffers via a compiler `-substitute-file orig=tmp` flag so a
  temp buffer analyzes as if at the real path (imports/module naming resolve),
  making `didChange` live.
- **v2** — go-to-definition via a compiler symbol dump.
