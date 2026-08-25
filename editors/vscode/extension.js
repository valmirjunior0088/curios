// What the extension does beyond its grammar: launch `curios wonder server` for Curios documents and let the language client paint what it publishes. The server is the `curios` binary on PATH, so there is nothing to download and no version to pin — the compiler the user runs is the one whose diagnostics they see.
const { LanguageClient } = require("vscode-languageclient/node");

let client;

exports.activate = (context) => {
  client = new LanguageClient(
    "curios",
    "Curios",
    { command: "curios", args: ["wonder", "server"] },
    { documentSelector: [{ scheme: "file", language: "curios" }] },
  );
  context.subscriptions.push({ dispose: () => client?.stop() });
  client.start();
};

exports.deactivate = () => client?.stop();
