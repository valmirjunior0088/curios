// What the extension does beyond its grammar: launch `curios wonder server` for Curios documents and let the language client paint what it publishes. The server is the `curios` binary itself, so there is nothing to download and no version to pin — the compiler the user runs is the one whose diagnostics they see.
//
// Two things it will not do. It will not start the server in an untrusted workspace: the server elaborates the workspace's own sources, which is running the workspace's code, and that is what the `limited` trust declaration in the manifest promises. It will not guess at a binary either — when the compiler is missing it says so, with the one-line installer and a button that copies it.
const vscode = require("vscode");
const { LanguageClient } = require("vscode-languageclient/node");

const INSTALL_COMMAND =
  "curl -fsSL https://github.com/valmirjunior0088/curios/releases/latest/download/install.sh | sh";

let client;

const serverPath = () =>
  vscode.workspace.getConfiguration("curios").get("serverPath") || "curios";

// The rejection carries the spawn failure, which names the path that was tried; the installer is offered beside it because a missing compiler is the overwhelmingly likely cause and is one paste to fix.
const report = async (error) => {
  const copy = "Copy install command";
  const choice = await vscode.window.showErrorMessage(
    `Could not start the Curios language server (${serverPath()}): ${error.message ?? error}`,
    copy,
  );

  if (choice === copy) {
    await vscode.env.clipboard.writeText(INSTALL_COMMAND);
  }
};

// Nothing waits for the server to come up. `report` resolves only when the user dismisses its notification, so awaiting this would leave activation — or a command — pending for as long as the notification sits there unread.
const start = () => {
  client = new LanguageClient(
    "curios",
    "Curios",
    { command: serverPath(), args: ["wonder", "server"] },
    { documentSelector: [{ scheme: "file", language: "curios" }] },
  );

  client.start().catch((error) => {
    client = undefined;
    report(error).catch(() => {});
  });
};

const stop = async () => {
  const running = client;
  client = undefined;
  await running?.dispose();
};

// Trust is checked here rather than at the call sites, because every way of arriving at a running server passes through it: the command, a changed setting, and activation itself.
const restart = async () => {
  await stop();

  if (!vscode.workspace.isTrusted) {
    vscode.window.showInformationMessage(
      "The Curios language server stays off until this workspace is trusted.",
    );
    return;
  }

  start();
};

exports.activate = (context) => {
  context.subscriptions.push(
    vscode.commands.registerCommand("curios.restartServer", restart),
    // A setting that names a different binary is only observed on a restart, so restarting is what the change does. Restarting rather than reconfiguring a live client also covers the case this exists for: the server failed to start, and pointing the setting at a working binary is the fix.
    vscode.workspace.onDidChangeConfiguration((event) => {
      if (event.affectsConfiguration("curios.serverPath")) {
        restart().catch(() => {});
      }
    }),
    // Never fires in a workspace that was trusted to begin with, and starts the server without a reload in one that becomes trusted.
    vscode.workspace.onDidGrantWorkspaceTrust(() => start()),
    { dispose: stop },
  );

  if (vscode.workspace.isTrusted) {
    start();
  }
};

exports.deactivate = stop;
