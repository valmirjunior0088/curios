# Curios for Zed

Syntax highlighting and live diagnostics for [Curios](https://github.com/valmirjunior0088/curios), a dependently typed functional language that compiles to WebAssembly.

## Requirements

The extension does not ship a compiler. It runs the one you have:

```sh
curl -fsSL https://github.com/valmirjunior0088/curios/releases/latest/download/install.sh | sh
```

That installs `curios` into `~/.local/bin`. The language server is `curios wonder server`, so the compiler you build with is the compiler whose diagnostics you see, and updating the compiler updates the server.

## Where it looks for the compiler

Zed inherits the `PATH` its desktop session was launched with rather than the one an interactive shell assembles, so a binary every terminal on the machine can see may be invisible here. The extension therefore searches three places, in order:

1. `lsp.curios.binary.path`, used exactly as written.
2. `curios` on the `PATH` Zed was launched with.
3. `~/.local/bin/curios`, where the installer puts it, asked `--version` before it is offered.

When none of them answers, the failure names which step it reached and how to fix it; the same text is in Zed's log, which is where to read it after the fact.

## Settings

Point the extension at a binary of your own in `settings.json`:

```json
{
  "lsp": {
    "curios": {
      "binary": {
        "path": "/path/to/curios"
      }
    }
  }
}
```

This is the one setting that does not depend on the environment, and the one to reach for when a project's shell environment names no `$HOME`.

## Building it from this repository

```sh
cargo xtask zed build
```

Install it with **zed: install dev extension** from the command palette, pointing at this directory. Zed builds the extension and its grammar itself, so `extension.wasm` and `grammars/` appear here as build products and are not committed.

The grammar is fetched from the `rev` named in `extension.toml`, not from your working tree: a grammar change is published by committing `editors/grammar/src/` beside the `grammar.js` that produced it, pushing, and moving that `rev`.

## Design

### The server is the compiler, and the extension fetches nothing

**Decision.** The language server is the `curios` binary itself, `curios wonder server`, found as "Where it looks for the compiler" states. The extension downloads nothing and pins no version, and where it finds no binary it hands back the one-line installer.

**Rationale.** The compiler on the user's `PATH` is then the one whose diagnostics they see, and installing Curios installs the server.

**Rejected.** A server the extension downloads and pins, a second version beside the compiler the user installed.

## License

Apache-2.0, the same as [the rest of Curios](https://github.com/valmirjunior0088/curios/blob/main/LICENSE). The text ships beside this file so that the packaged extension carries its own licence.
