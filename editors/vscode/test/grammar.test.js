// Offline regression test for the Ficus TextMate grammar, on the real engine
// (vscode-textmate + vscode-oniguruma) VS Code itself uses. Run:
//     cd editors/vscode && npm install && node test/grammar.test.js
// Covers the bugs fixed in lsp-1: interpolation must apply only to f-strings
// (a plain "{" is literal, like Python), {{ }} are literal braces in f-strings,
// and /* */ block comments nest.
const fs = require("fs");
const path = require("path");
const vsctm = require("vscode-textmate");
const oniguruma = require("vscode-oniguruma");

const NM = path.join(__dirname, "..", "node_modules");
const GRAMMAR = path.join(__dirname, "..", "ficus.tmLanguage.json");

const wasmBin = fs.readFileSync(path.join(NM, "vscode-oniguruma/release/onig.wasm"));
const onigLib = oniguruma.loadWASM(wasmBin.buffer).then(() => ({
  createOnigScanner: (p) => new oniguruma.OnigScanner(p),
  createOnigString: (s) => new oniguruma.OnigString(s),
}));

const registry = new vsctm.Registry({
  onigLib,
  loadGrammar: (scope) =>
    scope === "source.ficus"
      ? Promise.resolve(
          vsctm.parseRawGrammar(fs.readFileSync(GRAMMAR, "utf8"), GRAMMAR)
        )
      : null,
});

let failures = 0;
function check(desc, cond) {
  console.log((cond ? "  ok   " : "  FAIL ") + desc);
  if (!cond) failures++;
}
const isString = (scopes) => scopes.some((s) => s.startsWith("string."));
const isComment = (scopes) => scopes.some((s) => s.startsWith("comment"));
const isInterp = (scopes) => scopes.some((s) => s.includes("meta.template"));
function scopesAt(tokens, col) {
  for (const t of tokens) if (col >= t.startIndex && col < t.endIndex) return t.scopes;
  return [];
}
const ended = (r) => r.ruleStack === vsctm.INITIAL || r.ruleStack.depth <= 1;

(async () => {
  const grammar = await registry.loadGrammar("source.ficus");

  {
    // plain string with a single brace: braces literal, code resumes (Ast_pp.fx)
    const line = 'pp.str(if ordered {"{"} else {"@ordered {"})';
    const r = grammar.tokenizeLine(line, vsctm.INITIAL);
    check('plain "{": "else" after it is code',
      !isString(scopesAt(r.tokens, line.indexOf("else"))));
    check('plain "{": "if ordered" is code',
      !isString(scopesAt(r.tokens, line.indexOf("if"))));
    check('plain "{": line ends outside a string', ended(r));
  }
  {
    // println("{") must not swallow the following line
    const r1 = grammar.tokenizeLine('println("{")', vsctm.INITIAL);
    const r2 = grammar.tokenizeLine('println("}")', r1.ruleStack);
    check('println("{") closes its string', ended(r1));
    check('next line is fresh code', !isString(scopesAt(r2.tokens, 0)));
  }
  {
    // f-string interpolation
    const line = 'val s = f"{a} and {b}!"';
    const r = grammar.tokenizeLine(line, vsctm.INITIAL);
    check("f-string: {a} is interpolation",
      isInterp(scopesAt(r.tokens, line.indexOf("{a}") + 1)));
    check("f-string: literal '!' is string",
      isString(scopesAt(r.tokens, line.indexOf("!"))));
  }
  {
    // {{ }} literal braces (Python-style), real {x} still interpolates
    const line = 'val s = f"a {{literal}} b {x}"';
    const r = grammar.tokenizeLine(line, vsctm.INITIAL);
    check("f-string: {{...}} is NOT interpolation",
      !isInterp(scopesAt(r.tokens, line.indexOf("literal"))));
    check("f-string: {x} after {{}} still interpolates",
      isInterp(scopesAt(r.tokens, line.indexOf("{x}") + 1)));
  }
  {
    // nested block comment
    const line = "/* a /* b */ c */ code_after";
    const r = grammar.tokenizeLine(line, vsctm.INITIAL);
    check("nested comment: content after inner close still comment",
      isComment(scopesAt(r.tokens, line.indexOf(" c ") + 1)));
    check("nested comment: balanced -> ends outside comment", ended(r));
    check("nested comment: text after outer close is code",
      !isComment(scopesAt(r.tokens, line.indexOf("code_after"))));
  }

  console.log(failures === 0 ? "\nALL PASS" : `\n${failures} FAILURE(S)`);
  process.exit(failures === 0 ? 0 : 1);
})();
