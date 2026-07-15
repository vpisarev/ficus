# Ficus language server (lsp-1)

`tools/FicusLsp.fx` is a minimal LSP server for Ficus, written in Ficus. On
open/save it runs `bin/ficus -no-c -diag-format=json -Wall <file>` and republishes
the compiler's diagnostics to your editor, with a **quickfix** for every
"did you mean 'x'?" suggestion.

## What works in v1

- Live diagnostics (errors **and** `-Wall` warnings) on **open** and **save**.
- Exact underlines for frontend diagnostics (lexer/parser/typecheck/K-norm);
  drifted (middle/backend) locations fall back to a whole-line range with the
  enclosing-context anchor folded into the message.
- **Code actions**: a did-you-mean suggestion becomes a "Change to 'x'" quickfix
  that replaces the exact identifier span.
- Crash-safe: the compiler runs as a child process, so a compiler crash surfaces
  as one internal-error diagnostic and can never take the editor's server down.

Not in v1: completion, hover, go-to-definition, and **unsaved-buffer** analysis
(`didChange` is accepted but ignored — you get diagnostics on save). See the
roadmap below.

## Build the server

```sh
make -j8                                   # builds bin/ficus
bin/ficus -o ficus-lsp tools/FicusLsp.fx   # builds ./ficus-lsp (a standalone binary)
```

Point your editor at the `ficus-lsp` binary and tell it where `bin/ficus` is via
`initializationOptions.ficusPath` (otherwise the server looks for `bin/ficus`
under the workspace root, then on `PATH`). Set `FICUS_LSP_LOG=1` in the
environment to get protocol logging on the server's stderr.

## Neovim (built-in LSP, `vim.lsp.start`)

Ficus has no built-in filetype, so register `*.fx` first. Put this in your
`init.lua` (adjust the two absolute paths):

```lua
-- 1. recognize .fx as filetype "ficus"
vim.filetype.add({ extension = { fx = "ficus" } })

-- 2. start the Ficus LSP for every ficus buffer
local FICUS_ROOT = "/path/to/ficus"          -- repo root (contains bin/ficus)
vim.api.nvim_create_autocmd("FileType", {
  pattern = "ficus",
  callback = function(args)
    vim.lsp.start({
      name = "ficus-lsp",
      cmd = { FICUS_ROOT .. "/ficus-lsp" },
      root_dir = vim.fs.dirname(vim.fs.find({ ".git" }, { upward = true })[1])
                 or FICUS_ROOT,
      init_options = { ficusPath = FICUS_ROOT .. "/bin/ficus" },
    })
  end,
})
```

Diagnostics appear on `:w`. Trigger a quickfix with `vim.lsp.buf.code_action()`
(map it to e.g. `<leader>ca`) while on a "did you mean" error.

## VS Code

Unlike neovim/eglot, VS Code has no built-in generic LSP client — it needs a
small extension. One is provided in `editors/vscode/` (a thin client + a TextMate
grammar for syntax highlighting). Setup:

```sh
bin/ficus -o ficus-lsp tools/FicusLsp.fx      # build the server
cd editors/vscode
npm install                                    # fetch vscode-languageclient
ln -sf ../../ficus-lsp ficus-lsp               # let the extension find the binary
```

Then either **(a)** open `editors/vscode` in VS Code and press **F5** (launches an
Extension Development Host), or **(b)** install it for real:
`ln -sfn "$PWD" ~/.vscode/extensions/ficus-lsp-0.1.0` and restart VS Code. In
Settings set **`ficus.ficusPath`** to the absolute path of `bin/ficus`.

Open a `.fx` file: syntax highlights immediately; diagnostics appear **on save**
(v1 syncs on save — turn on `files.autoSave: afterDelay` for a near-live feel),
and `Ctrl+.` on a did-you-mean applies the quickfix. See
`editors/vscode/README.md` for details. Note: diagnostics from imported modules
are routed to those modules' own files (shown in the Problems panel and in the
module when opened), not drawn in the file you have open.

## Emacs (eglot)

```elisp
(define-derived-mode ficus-mode prog-mode "Ficus"
  "Major mode for Ficus."
  (setq-local comment-start "// "))
(add-to-list 'auto-mode-alist '("\\.fx\\'" . ficus-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(ficus-mode . ("/path/to/ficus/ficus-lsp"
                               :initializationOptions
                               (:ficusPath "/path/to/ficus/bin/ficus")))))
;; M-x eglot in a .fx buffer; diagnostics on save, M-x eglot-code-actions for fixes.
```

## Known v1 limitations

- **Save-only.** Edits are analyzed on save, not per keystroke (`didChange`
  ignored). Fine at the compiler's ~0.13 s `-no-c` latency; unsaved analysis is
  the v1.5 item below.
- **Character offsets** are treated as code-point indices; LSP nominally uses
  UTF-16 code units, so a column past an astral (non-BMP) character on the same
  line can be off. BMP text (the vast majority) is exact.
- **Non-ASCII percent-encoded `file://` URIs** are decoded ASCII-correctly only;
  raw-UTF-8 and ASCII paths work. Windows drive-letter URIs are not special-cased.

## Roadmap

- **v1.5 — unsaved buffers.** Needs a compiler flag to analyze a temp file *as if*
  it were at the real path, so imports/module naming still resolve:
  `-substitute-file orig=tmp`. Then the server writes the buffer to a temp file
  and passes the substitution; `didChange` becomes live (debounced).
- **v2 — navigation.** Go-to-definition / find-references via a compiler symbol
  dump (a `-pr-symbols`-style json of definition locations), consumed like the
  diagnostics jsonl.
- **Incremental analysis** only if latency ever demands it; the process-per-analysis
  model is intentionally simple and stateless.

## Testing

`python3 tools/fxtest/fxtest.py lsp` builds the server and drives it over a pipe
(`tools/fxtest/lsp_driver.py`, Python-stdlib `unittest`): initialize handshake;
a typo → one diagnostic with the expected range + one quickfix; fix → cleared
diagnostics; a warning file → severity mapped; a missing compiler → survived
with an internal-error diagnostic.
