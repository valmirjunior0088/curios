# Curios for Visual Studio Code

Syntax highlighting and live diagnostics for [Curios](https://github.com/valmirjunior0088/curios), a dependently typed functional language that compiles to WebAssembly.

## Requirements

The extension does not ship a compiler. It runs the one you have:

```sh
curl -fsSL https://github.com/valmirjunior0088/curios/releases/latest/download/install.sh | sh
```

That installs `curios` into `~/.local/bin`. Any `curios` on your `PATH` will do — the language server is `curios wonder server`, so the compiler you build with is the compiler whose diagnostics you see.

## Settings

| Setting | Default | Meaning |
| --- | --- | --- |
| `curios.serverPath` | `curios` | The binary to run. A bare name is looked up on `PATH`; an absolute path is used as given. |

To see the traffic between the editor and the server when reporting a problem, run **Developer: Set Log Level…**, pick **Curios**, and choose **Trace**. The exchange then appears in the Curios output channel.

## Commands

- **Curios: Restart Language Server** — after installing or replacing the compiler.

## Where it does not run

The server is a local binary reading local paths, so the extension declares no support for virtual workspaces — in `vscode.dev` or a remote GitHub repository, highlighting works and diagnostics do not.

## Workspace trust

The language server elaborates the sources in your workspace, which means running the workspace's code. In an untrusted workspace the server stays off and highlighting continues to work; trusting the workspace starts it without a reload.

## Building it from this repository

```sh
npm ci
npm run package         # writes .artifacts/curios-<version>.vsix, bundling first
npm test                # TextMate grammar snapshots
```

Install the result from the Extensions view: `⋯` → **Install from VSIX…**. Everything under `.artifacts/` is a build product and is not committed.

## License

Apache-2.0, the same as [the rest of Curios](https://github.com/valmirjunior0088/curios/blob/main/LICENSE). The text ships beside this file so that the packaged extension carries its own licence.
