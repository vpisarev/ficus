# Ficus VS Code client

A thin VS Code extension that launches the Ficus language server
(`tools/FicusLsp.fx`) over stdio. All the intelligence is in the server — this
just registers the `.fx` language and forwards LSP traffic. You get live
diagnostics on open/save and a "Change to 'x'" quickfix for every did-you-mean.

## Prerequisites

- Node.js + npm (for `vscode-languageclient`).
- A built compiler and server:
  ```sh
  cd <ficus-root>
  make -j8
  bin/ficus -o ficus-lsp tools/FicusLsp.fx
  ```

## Setup (one time)

```sh
cd <ficus-root>/editors/vscode
npm install                       # fetches vscode-languageclient
# make the server binary findable by the extension (either copy or symlink):
ln -sf ../../ficus-lsp ficus-lsp  # or: cp ../../ficus-lsp .
```

Alternatively, skip the symlink and set `ficus.serverPath` in VS Code Settings to
the absolute path of the `ficus-lsp` binary.

## Run it (Extension Development Host)

1. Open the `editors/vscode` folder in VS Code (`code editors/vscode`).
2. Press **F5** ("Run Extension") — a second VS Code window opens (the Extension
   Development Host) with the extension loaded.
3. In that window, open your Ficus project and any `.fx` file.
4. Set the compiler path: **Settings → Extensions → Ficus → `ficus.ficusPath`** =
   absolute path to `<ficus-root>/bin/ficus` (only needed if the workspace root
   isn't the ficus repo).

Now:
- **Save** a `.fx` file with an error → a red squiggle appears (diagnostics sync
  on save in v1).
- On a `lenght`-style typo, click the lightbulb or press **Ctrl+.** →
  **"Change to 'length'"** applies the fix.
- Warnings (`-Wall`) show as yellow squiggles.

Set `FICUS_LSP_LOG=1` in the environment before launching VS Code to get server
protocol logging (View → Output → "Ficus Language Server").

## Install permanently (optional)

```sh
npm i -g @vscode/vsce
vsce package                                   # produces ficus-lsp-0.1.0.vsix
code --install-extension ficus-lsp-0.1.0.vsix
```

## Developing the grammar

Syntax highlighting is a TextMate grammar (`ficus.tmLanguage.json`). After editing
it, run the offline regression test (on the same engine VS Code uses):

```sh
npm install        # dev deps: vscode-textmate, vscode-oniguruma
npm test           # node test/grammar.test.js
```

It locks the tricky cases: a plain `"{"` is a literal brace (interpolation applies
only to `f"..."`, like Python), `{{`/`}}` are literal braces in f-strings, and
`/* */` comments nest.

## Notes / v1 limitations

Diagnostics update on **save**, not per keystroke (`didChange` is intentionally
ignored in v1 — see `docs/lsp.md` for the v1.5 unsaved-buffer plan). Column
offsets are code-point-based (exact for BMP text). No completion/hover/go-to-def
yet.
