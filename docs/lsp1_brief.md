# Brief: lsp-1 — structured diagnostics + the first Ficus language server

**You are Claude Code working in the Ficus repo.** Branch `lsp-1` off master.
Context: `docs/diag1_report.md` (the centralized diagnostic printer this
builds on), repo CLAUDE.md. Architecture (settled with Vadim, measurement-
backed): a thin PERSISTENT server process (protocol dispatcher, written IN
FICUS — see Phase 0.5) + a FRESH `bin/ficus -no-c -O0` process per analysis.
Measured: `-no-c` on a real program is **0.127 s including the stdlib
preamble** — fast enough for per-keystroke analysis, let alone on-save. No
compiler-state refactoring (`compiler_t`, reset hardening) is in scope; the
compiler stays single-shot, isolation and parallelism come free from the
process model, and a compiler crash can never kill the server.

## Phase 0 — `-diag-format=json` (compiler)

diag-1 centralized emission in one printer carrying
`(loc+span, precision, severity, message, notes/did-you-mean)`. Add an output
mode switch there (and ONLY there): one JSON object per diagnostic, one per
line (jsonl on stdout), fields:
`{"file", "line0", "col0", "line1", "col1", "precision": "exact|line|anchor",
"severity": "error|warning", "message", "suggestions": ["s", ...],
"anchor": "in function 'foo'" (when precision != exact)}`.
The human format is untouched and remains the default; `-Werror`/limits
semantics unchanged; the end-of-run summary is suppressed in json mode (the
client counts). Negative-golden harness untouched (goldens stay on the human
format); add 2–3 directed json-mode goldens locking the schema. Escaping:
messages contain quotes/braces from source — use a real JSON emitter, not
string concatenation (hand-roll the tiny escaper inside the printer rather
than importing lib/Json.fx into the compiler — no new bootstrap dependency
for one emitter).

## Phase 0.5 — capability gate for a FICUS server (decision point)

The server is written **in Ficus** (Vadim: stdlib has `Json.fx`) — it becomes
the first long-running Ficus program and the start of the dogfooding loop:
open `docs/missing_features.md` (append-only, found_bugs-style, ergonomics
gaps — "wanted X, had to Y, missing Z"; no fixes on the spot) and feed it
throughout. Three capability checks BEFORE writing the server; report each:

1. **Byte-exact stdio — PRE-VERIFIED (Vadim): `File` reads byte-counted
   UTF-8 from stdin/files.** LSP framing is `Content-Length: N` in BYTES of
   the UTF-8 payload, so "read exactly N bytes → decode UTF-8" is precisely
   the right primitive. Session task shrinks to a directed test (a message
   with multi-byte characters — N bytes ≠ N chars — framed and parsed
   correctly) plus verifying byte-counted WRITE for responses (compute
   Content-Length from the UTF-8 byte length of the reply, not
   `s.length()`).
2. **Json.fx spec coverage.** Directed tests against the short spec:
   `\uXXXX` escapes incl. surrogate pairs, deep nesting, number forms,
   escaped quotes/backslashes, large payloads. Fill small gaps found (own
   commit, rettype-gated); anything structural — report first.
3. **Child process with captured stdout — PRE-VERIFIED (Vadim):
   `File.popen` exists (the stdlib itself uses it — `uname -msr` for OS
   detection).** Session task shrinks to a directed test: spawn
   `bin/ficus -no-c -diag-format=json <file>`, read the full jsonl through
   the pipe, handle a non-zero exit (diagnostics present) vs zero (clean),
   and confirm nothing blocks on large outputs (a many-diagnostics file).

If any check reveals a blocker beyond "small addition", STOP and report — the
Python fallback (stdlib-only, ~100 lines of framing) remains available as a
bridge, but the default is Ficus.

## Phase 1 — the server (`tools/FicusLsp.fx` or `lib`-adjacent; Vadim names it)

Ficus, compiled by the repo's own `bin/ficus`, `-Wall`-clean (it joins the
corpus and the rettype gate). JSON-RPC 2.0 over stdio with `Content-Length`
framing (byte-exact, per Phase 0.5). Protocol subset:
- `initialize`/`initialized`/`shutdown`/`exit` — capabilities:
  `textDocumentSync: {openClose: true, save: true}`, no completion, no
  hover (v1).
- `textDocument/didOpen` and `didSave` → run `bin/ficus -no-c
  -diag-format=json -Wall <file>` (find `bin/ficus` relative to the
  workspace root, overridable via `initializationOptions.ficusPath`),
  parse jsonl, map to `publishDiagnostics` (LSP is 0-based, Ficus loc is
  1-based — convert; `precision: line|anchor` → range = whole line, anchor
  text appended to the message).
- `textDocument/codeAction`: each diagnostic with `suggestions` yields a
  quickfix per suggestion — replace the identifier span with the suggestion
  (the span is exact for these by construction). This is did-you-mean
  becoming a button.
- Debounce/serialize: one analysis in flight per file, latest request wins;
  kill the previous process if still running (0.13 s makes this mostly moot,
  but unbounded pileup must be impossible by construction).
- `didChange` is ACCEPTED and ignored in v1 (sync on save). Unsaved-buffer
  analysis is v1.5: it needs a compiler flag to substitute a file's content
  from a temp path (`-substitute-file orig=tmp`) so imports/module naming
  still resolve from the real location — note it in the report, do not build
  it now.

Tests: a driver script (Python stdlib `unittest` is fine for the TEST harness only) driving the server over a
pipe: initialize handshake; open a file with a known typo → exactly one
diagnostic with the expected range and one quickfix; fix applied → clean
publish (empty diagnostics array — don't forget to CLEAR on success);
a warning-only file → severity mapped; a file that crashes the compiler
(fenced repro if any exists) → server survives and reports an internal-error
diagnostic. Wire into fxtest as `fxtest.py lsp` (optional leg).

## Phase 2 — editor smoke + docs

A minimal VS Code client is NOT in scope; verify against a built-in generic
client instead (neovim `vim.lsp.start` config snippet and/or an Emacs eglot
one) — include the working config in `docs/lsp.md` (how to point an editor
at the server binary, what works in v1, the v1.5/v2 roadmap: unsaved
buffers via -substitute-file; go-to-definition via a compiler symbol dump;
incremental analysis only if latency ever demands it). CLAUDE.md: two lines
(the json flag exists; the lsp fxtest leg).

## Ground rules

House standard: ladder + determinism + sanitize + fixpoint (Phase 0 touches
the diagnostic printer — human-format goldens must not change except the
suppressed summary if it was ever in goldens); census untouched; found_bugs
for discoveries; don't push. STOP: if Phase 0 cannot be done inside the one
printer (emission turns out decentralized somewhere), report the sites
instead of scattering json writers. Report `docs/lsp1_report.md` with a
screencap-style transcript of the jsonl for one diagnostic and the editor
config that worked.
