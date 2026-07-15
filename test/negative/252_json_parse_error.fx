// expect: "severity": "error"
// flags: -diag-format=json
// lsp-1: locks json-mode for a PARSE error (a different emission path than a
// typecheck error -- it aborts parsing rather than flowing through the error
// queue). Still one jsonl object with an exact frontend span and no suggestions.
fun f(x: int) = x ->
