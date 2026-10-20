# A browser page is closed and laid out as its packages

**Decision.** `curios compile --target browser` writes a directory that is the page whole. Three of its files are the page's own and no package supplies them: `index.html`, holding the import map, the loader's script element and one `<pre>` the program's output is written into while it stands in the document, which a gateway may remove; `index.js`, the loader, carrying the harness, what it is configured with, the program's rows, the bridge and an import of every gateway; and `index.wasm`, the module Binaryen wrote, which no Cranelift step follows. No file of any package may stand at one of the three, which `curios-package` refuses where a manifest is read.

The page's root is the program's package's root. A gateway the package carries stands at its path from `curios.toml`, a fetched one at `<row>.js`, a `[foreign.browser.static]` entry's files at its page path — a directory as every file under it, names starting with `.` skipped, symlinks refused — and everything a dependency's rows put in the page under the dependency's name. Every path is claimed once, a second claim refused naming both claimants. Only a row the program reaches puts anything in the page. Nothing written is root-absolute, so the directory works under any path a server puts it at, served by `run --target browser` or by any static server.

The loader fetches `index.wasm` against its integrity, `index.html` states the loader's, and the import map states every other JavaScript file's, each the `sha256-` form of the file's SHA-256. Before the program runs, the loader refuses an export of a gateway that is no function, naming the row. The program runs on the main thread, and its arguments are the page's name, then each `arg=` of the URL's query, percent-decoded to bytes. Curios runs no tool to build what a gateway imports: that is built before `compile` runs, into a directory a `static` entry serves.

**Rationale.**

- **One layout for development and deployment.** `run --target browser` serves the layout the written directory holds, so a path that resolves in one resolves in the other, and nothing is rewritten between the two.
- **A package's files keep their places beside each other in every page.** Only the program's package stands at the page's root, so a gateway names its own package's files relative to its own address, `new URL("../art/pixel.png", import.meta.url)`, which reaches the same file in its package's page and in every page that depends on it, as the `import`s it writes already do. A URL resolved against the document names the program's package's files, which are a dependency's only where it is the program.
- **The toolchain owns the files it writes.** A page a package could replace is a page the loader cannot rely on: its script element, its import map and its element for the output are what the loader reads. Holding the three names for itself, in every package, keeps any package from coming to depend on standing there.
- **A dependency is a namespace in the page as it is in the program.** Its name cannot collide with another package's, so nesting by it removes most collisions by construction, and the claim check names the rest.
- **What a page holds is what it runs.** Carrying only the reached rows' files keeps a dependency's assets out of a page that never calls it, and the integrity of every module the page imports makes a file changed after the build fail to load rather than run.
- **A build tool is a decision the author already made.** TypeScript, a bundler or a minifier each has its own configuration, and running one from the compiler would make the compiler the owner of a toolchain it cannot vouch for; a `static` entry is the one door, so whatever produced a file, the compiler serves it as it is.
- **Arguments in the URL** are the one input a page has that a link can carry, and bytes percent-encoded are every argument `/std/Process/args` can hand a program.

**Rejected.**

- **Every package under its own name, the program's included**, so a document-relative path would be wrong everywhere rather than right in one page: the program's own files would no longer stand where its package holds them, and a gateway still names its files from its own address.
- **A page the package supplies**, or a template of one: the loader's contract with the document would be the author's to keep.
- **Files named after the program** — `<name>.wasm`, `<name>.js` — which move a page's own URLs with a rename and make two programs' pages different shapes.
- **Build hooks** that run a command before or during the build, and **a bundler** of the page's JavaScript: a second toolchain inside the compiler, for what the author's own already does.
- **The module inlined into the loader**, which pays a third of its size again in base64 and loses the integrity a fetch checks.
- **A worker**, which leaves the document a gateway reaches behind a message channel; **JSPI**, unneeded, since a hook that answers a promise already suspends only the call that made it.
