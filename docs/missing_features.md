# Missing features & ergonomics gaps

Append-only log (found_bugs-style) of stdlib/language ergonomics gaps hit while
writing real Ficus programs — "wanted X, had to Y, missing Z". Not compiler bugs
(those go in `docs/found_bugs.md`); not fixed on the spot unless trivial and in
scope. Started during lsp-1 (the LSP server is the first long-running Ficus
program and the start of the dogfooding loop).

## MF-001 — Json.fx parser does not combine UTF-16 surrogate pairs (lsp-1)

`lib/Json.fx` parses `\uXXXX` escapes (via `LexerUtils.getstring`) but decodes a
surrogate pair `𐀀` as **two** separate code points (0xD800, 0xDC00)
rather than combining them into the single U+10000 astral character. JSON
producers that escape non-BMP characters as surrogate pairs (e.g. VS Code sending
an emoji in a hover/label) would be mangled.

**Why not fixed now:** it is a real semantic change to the shared lexer string
routine (`lib/LexerUtils.fx`), used by both Json parsing and Ficus source
lexing — structural enough to design separately. Directly-embedded UTF-8 astral
characters (the common case) parse fine; only the `\u`-escaped surrogate-pair
spelling is affected. Revisit when LSP clients that emit surrogate pairs actually
appear in testing.

**Fixed in lsp-1 (own commit):** the Json.fx *serializer* — it previously used a
naive `repr()` that added quotes without escaping `"`, `\`, or control chars, so
`Json.string` emitted invalid JSON for realistic payloads (Windows paths, quoted
messages). Now it escapes per RFC 8259 and offers a compact/minified mode
(`Json.string(js, compact=true)`).

## MF-002 — parser "did you mean" suggestions not exposed structurally (lsp-1)

Typecheck did-you-mean suggestions are now carried as a structured
`suggestions` array on the diagnostic (for editor quickfixes). The **parser**'s
module-name suggestion (`Parser.fx`, "module Strig is not found; did you mean
'String'?") still only bakes the suggestion into the message text — the json
`suggestions` array is empty for it, because parser errors raise a `ParseError`
exception (`(loc, string)`) that never gained the structured twin `CompileError`
did. Low priority: v1 codeAction targets identifier typos, not imports.

**How to apply:** when import quickfixes are wanted, give `ParseError` the same
structured payload (or route parser diagnostics through `compile_err`), then
thread the candidate list from the parser suggestion site.

## MF-003 — compiler message typo "unxpected token" (lsp-1)

The parser emits `"unxpected token '->'. ..."` (missing 'e'). Cosmetic; left
as-is to avoid churning the human negative goldens that lock it. Fix opportunistically
next time those goldens are regenerated for another reason.
