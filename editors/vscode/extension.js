// Minimal VS Code client for the Ficus language server (tools/FicusLsp.fx).
// It registers no logic of its own -- it just launches the ficus-lsp binary over
// stdio and forwards LSP traffic. All the intelligence lives in the server.

const { workspace, window } = require("vscode");
const { LanguageClient, TransportKind } = require("vscode-languageclient/node");

let client;

function activate(context) {
  const cfg = workspace.getConfiguration("ficus");
  const serverPath = cfg.get("serverPath") || context.asAbsolutePath("ficus-lsp");
  const ficusPath = cfg.get("ficusPath") || "";

  const serverOptions = {
    command: serverPath,
    transport: TransportKind.stdio,
    options: { env: process.env },
  };

  const clientOptions = {
    documentSelector: [{ scheme: "file", language: "ficus" }],
    initializationOptions: ficusPath ? { ficusPath } : {},
    outputChannelName: "Ficus Language Server",
  };

  client = new LanguageClient(
    "ficus-lsp",
    "Ficus Language Server",
    serverOptions,
    clientOptions
  );

  client.start().catch((err) => {
    window.showErrorMessage(
      `Ficus LSP failed to start (${serverPath}): ${err.message}. ` +
        `Set 'ficus.serverPath' and 'ficus.ficusPath' in Settings.`
    );
  });
}

function deactivate() {
  return client ? client.stop() : undefined;
}

module.exports = { activate, deactivate };
