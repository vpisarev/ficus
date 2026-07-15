// expect: "severity": "error"
// flags: -diag-format=json
// lsp-1: locks the -diag-format=json schema for a typecheck error. One JSON
// object per line (jsonl): exact span (col0..col1 covers the identifier), a
// clean message, and the did-you-mean candidate exposed as a structured
// "suggestions" array (what an editor turns into a quickfix).
val doubled = 21 * 2
val r = lenght + doubled
println(r)
