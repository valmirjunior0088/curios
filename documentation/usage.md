# Usage

The complete command-line and package reference. The [README](../README.md) covers the happy path; this covers the rest.

- [Which manifest governs](#which-manifest-governs)
- [What a package is made of](#what-a-package-is-made-of)
- [Subjects](#subjects)
- [The surface](#the-surface)
- [`run`](#run) · [`compile`](#compile) · [`document`](#document) · [`test`](#test) · [`curate`](#curate) · [`pin`](#pin) · [`new`](#new) · [`lint`](#lint) · [`format`](#format) · [`wonder`](#wonder) · [`profile`](#profile)
- [The manifest](#the-manifest)
- [Exit codes](#exit-codes)
- [Flags](#flags)
- [Standard input](#standard-input)
- [Where things go](#where-things-go)
- [Reusing what was already built](#reusing-what-was-already-built)

## Which manifest governs

The governing package is the one whose `curios.toml` is nearest the subject — a file's own directory, else the working directory — found there or in the first directory above that holds one. A command run anywhere inside a package means that package, and names the manifest beside its subject where it is not in the working directory: `serve (../curios.toml)`. The walk stops at the first manifest, so a package nested in another's directory is its own, and an umbrella found first is refused, as it declares no package.

Above the package only an umbrella is looked for, and only one that enumerates the package governs it. Its directory is then the governing root, where `.curios/` goes, shared with its other members.

`--manifest <PATH>` names the package's manifest; which umbrella governs is still enumeration's answer ([the four laws](../curios-package/README.md#the-four-laws)).

## What a package is made of

Two files are a package's by being beside its manifest ([the four laws](../curios-package/README.md#the-four-laws)):

| File | Is |
| --- | --- |
| `lib.crs` | the package's library, mounted at its declared `name` |
| `exe.crs` | the package's own executable, run by that same name |

The smallest complete package is two lines of manifest, its name and the compiler it is written for:

```toml
name = "hello"
compiler = "0.16.9"
```

Neither stem is part of a name, and each is its unit's root, loading its modules from the manifest's directory ([Modules](language.md#modules)): `pub mod parse;` in `lib.crs` reads `parse.crs`, and `mod util;` in `exe.crs` reads `util.crs`.

A package named `std` is the standard library. Compiled first, it takes the archived `/std`'s place, compiled over it, and everything after it is compiled against it; anywhere else it collides with the archived root as any other claim on `/std` does ([`curios-pipeline`'s README](../curios-pipeline/README.md)).

Everything else is declared, in the tables [the manifest](#the-manifest) states. A declared executable wins over the found file, so `hello = { path = "…" }` in `[executables]` overrides `exe.crs`; with more than one executable and no `default`, a bare `curios run` refuses.

A file two units' `mod` chains reach is refused before anything is compiled, naming both ([`curios-module`'s README](../curios-module/README.md)). A module the library and a program both want lives in the library, which every executable of the package reaches by the package's name:

```
`parse.crs` is a module of the library and of the executable "tool": a file is one unit's, so declare it in the library alone, and reach it from the other through `/hello/parse`
```

## Subjects

Every command that takes a subject takes it one of four ways:

| Argument | Means |
| --- | --- |
| *(none)* | the governing package, narrowed to what the command needs |
| an identifier | the executable declared under that name |
| a name ending in `.crs` or `.crsc`, or holding a path separator | that file, placed by what declares it |
| `-` | the text on [standard input](#standard-input), loose |

The dispatch is lexical: an executable's name is one identifier, so `curios run scratch.crs` is the file even where an executable is named `scratch`.

A `.crsc` file is Core text, the form `wonder --stage core` and `core-elab` print, and always loose; any other is a program or a module. `wonder` and `format` alone read Core text, and every other command refuses a `.crsc`, naming them. Standard input is Core text under [`--core`](#standard-input).

A file is an executable's where it is the entry or the entry's `mod` lines reach it, the library's where `lib.crs`'s do, and loose otherwise, compiled against the standard library alone. Its project is read from where the file lies, never from the working directory.

A loose text — such a file, or standard input — is what it is written as: a program where a term closes its declarations, and otherwise a module of its own, mounted at the file's stem, or at `/stdin`, and answered under it, `/double/twice` for the `twice` that `double.crs` declares. Every command takes a loose module where it takes a library's file, and refuses it where it needs a program, as `document`, which needs a library, does. A loose module whose stem is no name a module can take, `my-file.crs`, can be mounted nowhere, and every command refuses it.

`run` and `compile` take a program's own file: a module of one is refused, naming its executable, and so is a library. Every other command takes a module as its unit.

A loose file has no project: no dependencies, not the library beside it, and no `[foreign]` row, so a `foreign` it declares is answered by nothing, which `run`, `test` and `compile` say ([An argument names one subject](design/tools/an-argument-names-one-subject-and-each-command-states-what-it-accepts.md)). An `[executables]` key makes a scratch program its package's, and a `mod` line a module.

## The surface

Eleven commands. What each takes and leaves:

| Command | Subject | Its own flags | Writes | Cabinet |
| --- | --- | --- | --- | --- |
| [`run [SUBJECT] [ARGS…]`](#run) | a program | `--target <TARGET>`, `--port <PORT>` | nothing | reads, files what it built |
| [`compile [SUBJECT]`](#compile) | a program | `--target <TARGET>`, `-o`/`--output <PATH>` | a native executable, or a page's directory | reads, files what it built |
| [`document [SUBJECT]`](#document) | a library | `--std <DIR>`, `-o`/`--output <DIR>` | pages | reads, files what it built; none under `--std` |
| [`test [SUBJECT]`](#test) | anything | `--target <TARGET>`, `--port <PORT>`, `--filter <PREFIX>` | nothing | reads, files what it built |
| [`curate`](#curate) | the governing package | — | materialized sources | none |
| [`pin foreign <NAME>`](#pin) | the governing package's manifest | `--target <TARGET>`, `--path <PATH>` or `--url <URL>`, `--refresh`, `--check` | one manifest row | files what it delivered |
| [`pin dependency <NAME>`](#pin) | the governing package's manifest | `--path <PATH>`, `--git <URL>`, `--rev <REV>`, `--refresh`, `--check` | one manifest row | files what it delivered |
| [`new <DIR>`](#new) | — | — | a package | none |
| [`lint [SUBJECT]`](#lint) | anything | — | nothing | reads, files the units it compiled from disk |
| [`format [SUBJECT…]`](#format) | files, any number | `--check`, `--line-width <COLUMNS>`, `--flat-match-width <COLUMNS>`, `--flat-match-arms <ARMS>` | the files, rewritten | none |
| [`wonder [SUBJECT]`](#wonder) | anything | — | nothing | reads, files the units it compiled from disk |
| [`wonder --tests [SUBJECT]`](#wonder) | anything | — | nothing | reads, files the units it compiled from disk |
| [`wonder --stage <STAGE> [SUBJECT]`](#wonder) | a program, or at `text`, `core` and `core-elab` a library's file | — | nothing | reads, files the units it compiled from disk |
| [`wonder --at <FILE>:<LINE>:<COLUMN>`](#wonder) | the file its place names | — | nothing | reads, files the units it compiled from disk |
| [`wonder --classes [SUBJECT]`](#wonder) | a program or a library's file | — | nothing | reads, files the units it compiled from disk |
| [`wonder --name <PATH> [SUBJECT]`](#wonder) | anything | — | nothing | reads, files the units it compiled from disk |
| [`wonder --name <PATH> --witnesses [SUBJECT]`](#wonder) | anything | — | nothing | reads, files the units it compiled from disk |
| [`wonder --name <PATH> --footprint [SUBJECT]`](#wonder) | a program | — | nothing | reads, files the units it compiled from disk |
| [`wonder --name <PATH> --stage <STAGE> [SUBJECT]`](#wonder) | anything at `text`, `core` and `core-elab`, and a program past them | — | nothing | reads, files the units it compiled from disk |
| [`wonder --false [SUBJECT]`](#wonder) | a program | — | nothing | reads, files the units it compiled from disk |
| [`wonder --server`](#wonder) | — | — | nothing | reads, files the units it compiled from disk |
| [`profile <PATH>`](#profile) | — | — | nothing | none |

## What a command reports

A command narrates on standard error, in one form: a subject alone on a line, and beneath it a node for each thing done for it, the node's label written before its work and what the work came to after it.

```text
/geometry
├─ check; 1.6s
└─ store; 0.1s
hello
├─ check; 0.1s
├─ build; 0.2s
└─ emit; 0.4s
```

A subject is a unit by the prefix it mounts, an executable by its name, a file as it was written, or `<stdin>`, and the manifest that governs it is named beside it, `hello (../curios.toml)`, where the invocation does not stand in the manifest's directory. A unit is checked, `check`, or `check with its tests` where it is compiled with the tests it declares, as the one `test`, `wonder` and `lint` read is, and filed where a cabinet keeps it, `store`. A program is checked, `check`, its own declarations elaborated, certified, erased and linked with the units it stands on; built, `build`, lowered and optimized; and emitted, `emit`, written as WebAssembly, optimized by Binaryen and, natively, compiled to machine code. What the cabinet already held is not narrated: a subject nothing was done for says nothing, save one named beside its manifest.

A node that finished closes with what it took, `; 0.3s`. One that did not closes with how it ended and what it took — `; refused in 0.3s` where the compiler refuses the program, `; 1 goal in 0.3s` where written goals alone stopped it, `; failed in 0.3s` where the machine did — and what stopped it follows the tree, a blank line between. One whose point is where its work came to stand closes with that, spelled from where the invocation stands: `write; .curios/native/hello/hello`. `├─` is written where more nodes are planned and `└─` at the last, each before its node's work, so a tree a refusal cuts short ends on `├─`.

Nothing is rewritten or erased, and the bytes are the same in a pipe, a log file and a CI transcript as on a terminal, colour apart.

What the compiler says of a program is a report, and every report has one form, a block:

```text
type mismatch
│  inferred: `Nat`
│  expected: `Str`
│
│  3 │ let label: Str = double(21);
│    │                  ^^^^^^^^^^
│
└─ [error] demo.crs:3:18
```

The first line is a tag: a few words that name the kind of thing reported, the same wherever it is met and whichever stage reports it, with nothing of the program in them. What is reported of this program follows on a trunk, a fact a line and in one order: what stands here, the terms at issue each behind a label, the rule it is held to, and the way out, said as what to write. Whatever a report shows of a program — a term, a name, a pattern, a label, a value as written — stands in backticks, behind a label or inside a sentence. Then comes the program's own text, an empty line of the trunk above and below it, with marks under what the report is about. The last line says what the report is, in brackets, and where it stands: `[error]` for what a rule refuses, `[goal]` for a [written goal](language.md#written-goals), `[lint]` for a [lint](#lint), then the file, the line and the column, the file spelled from where the invocation stands. A place in the compiler's own library, which no file on the machine holds, closes on the package and the file's path within it: `/std/List.crs:12:5`. A report about a file as a whole shows no code and closes on the file alone, and a way out that is a command stands above the last line, behind `try`. A report about a second place shows that place first, closed by what stands there and a `;`, so the report's own place is always its last line, the one line in brackets; and every pane of one report shares a gutter, so the code of each begins in one column:

```text
declaration cycle
│  `a` and `b` need one another
│  a declaration elaborates after what it reads
│  each of these reads another before it can finish
│  by a name it writes, or by the witness a goal of its resolves to
│  a definition may call itself, and a witness may resolve through its own entry
│  a cycle through two has no order to elaborate them in
│  declare definitions that call one another as one group, `let ... and ...`
│  and witnesses that resolve through one another as `satisfy C(A) { ... } and D(B) { ... }`
│
│  10 │ let b: Nat = a;
│     │     ^
│
├─ `b` is declared here; demo.crs:10:5
│
│   9 │ let a: Nat = b;
│     │     ^
│
└─ [error] demo.crs:9:5
```

A goal's first line is its answer in place of a tag: what the compiler knows is wanted where it stands, `` `Nat` expected ``, or what it worked out stands there, `` `Nat` inferred ``; beneath it are the names in scope, each with its type, and each term the compiler found that fits, `? ≈ seed`. A text that is no form of the grammar is a `syntax error`, whose facts are what was `obtained:` there and what was `expected:` in its place, or the rule the text breaks and what to write. A build writes its reports on standard error after the tree, a blank line between; `wonder` and `lint` write them on standard output, as their answer.

What a command refuses before it compiles anything is a report of the same form: a subject it cannot take, a flag that cannot stand, a line that does not parse, a manifest it cannot read, a dependency it cannot place or fetch, a `foreign` nothing answers. It closes on what it is about. That is a row of a manifest, at its line and column with the row shown, where a manifest states what is refused; a file or a directory as a whole; or a flag, a filter or a command by the word that was written. A way out that is a command stands on the line above, behind `try`:

```text
hash mismatch
│  the dependency `json` was fetched from `https://example.org/json` at `v1.2`
│  obtained: `c1:9f3c…`
│  expected: `c1:ab12…`
│  intended if this row pins a branch or a tag, and it has moved
│  pay close attention if this row pins a commit, which should not move
│  a tree that changed under one means its history was rewritten
│
│  7 │ json = { git = "https://example.org/json", rev = "v1.2", hash = "c1:ab12…" }
│    │ ^^^^
│
├─ try; curios pin dependency json --refresh
└─ [error] curios.toml:7:1
```

A refusal's tag is the same wherever it is met: `unknown …` for what was named and is not there, `missing …` for what was not written, `duplicate …` and `conflicting …` for what was written twice, `invalid …` for what was written and cannot be read as what it should be, `not a …` for a thing of the wrong kind, `surplus …` for what a command takes none of, `compiler fault` for what no program or manifest earns. What follows the tag is this refusal's own: what stands there, `expected:` and `obtained:` where two values disagree, the rule, and the way out.

A program that stops while it runs is not reported this way: the line it stops on is its own ([`run`](#run)). The rule and what it rejected are [A command narrates on one transcript and reports in blocks](design/tools/a-command-narrates-on-one-transcript-and-reports-in-blocks.md)'s.

Colour is a layer over those bytes and changes none of them: a subject bold, the tree and the times dim, and what went wrong — `refused`, `failed`, `trapped`, `exited N`, a count of failures that is not zero — red. `--color <WHEN>` decides it once: `always`, `never`, or `auto`, the default, which paints where standard error is a terminal and the environment's `NO_COLOR` is unset or empty.

## `run`

```sh
curios run                       # the governing package's sole or `default` executable
curios run serve --port 8080     # a declared executable, and its own arguments
curios run scratch.crs           # a file
curios run -- mock               # the default executable, with argument mock
curios run serve --help          # the program's help
curios run --target browser      # the default executable's page, served on loopback
```

Everything after the subject belongs to the program and reaches it through `/std/Proc/args`, including `--help`, `--budget`, `--manifest` and `--profile`. Compiler flags go before the subject. A leading `--` omits the subject and passes the words after it to the governing package's sole or `default` executable. One `--` immediately after an explicit subject is an optional separator too: `curios run serve -- --help` passes `--help`. Every later word is collected verbatim, hyphens and further separators included; `curios run -- -- mock` passes `--` and `mock`. Program arguments retain their OS bytes, including text that is not UTF-8; an explicit subject must be UTF-8. A file whose name starts with a hyphen can be named with its directory, such as `./-sample.crs`.

A program the runtime stops rather than one that exits says why on one line of stderr and exits 1: the kind of stop, a colon, and what there is to say of this one. `panic:` is followed by one sentence naming the rule that refused the program — a `Nat` or `Int` argument to a `foreign` function past what the wire carries, a value past what the runtime can hold, a read past the end of a packed value or list, a `Flt` decoded from the wrong number of bytes, a recursive value read while its own initializer was running. `stack exhausted:`, `heap exhausted:`, `overflow:`, `division by zero:` and `unreachable code:` are the engine's stops; a program's recursion with no end stops on `heap exhausted:` ([A recursion is as deep as the heap allows](design/compilation/a-recursion-is-as-deep-as-the-heap-allows.md)), and a `foreign` module's on `stack exhausted:`. A call the runtime refuses where it crosses to what answers it is no panic: `invalid answer:` where a module answers `256` to a `foreign` declared `-> Byte`, `value too large:` where a byte string cannot cross to a module, `module failure:` where a module's own export fails, and `host failure:` where a host cannot answer a call that has no failure to report. `deadlock:` is a program waiting on nothing that can wake it. The line is no report block and carries no frame of the machine, and reads the same whether `curios run` ran the program, a test did, or it runs as a built executable, which says the same line of a bundle it cannot start: `not a bundle:`, `corrupt bundle:`.

A running program's own exit code passes through untouched. A program that closes with a description that may fail, and fails, writes the failure on stderr and exits 1, and one that closes with a test that did not hold reports it and exits 1 ([`documentation/language.md`](language.md#declarations-and-modules)). A `print` or `print_err` that cannot write ends the program with 141 where its reader has gone and 1 otherwise ([`curios-prelude`](../curios-prelude-build/README.md#a-loose-printer-that-cannot-write-ends-the-program)).

### Serving a page

`--target browser` builds the program's [page](#the-page) as `compile --target browser` does, reads every file of it once, and serves it from memory on `127.0.0.1` — at port 8000 unless `--port <PORT>` names another, and at a free one under `--port 0` — until the command is stopped. It closes the program's tree with where a browser opens it — `└─ serve; http://127.0.0.1:8000/?arg=--help` — and opens no browser.

The page stands at the root: `/` and `/index.html` are its document, every other file stands at its page path, and the program's arguments are the address's query, each an `arg=` percent-encoded, so the printed address runs it with the words after the subject. Nothing reloads: a changed program is served by running the command again, and every reply says `Cache-Control: no-store`.

Only `GET` is answered, and a request naming a host other than `127.0.0.1` or `localhost` at the port is refused. A port another process holds is refused, naming the flag that picks another, and `--port` beside the native target is refused.

## `compile`

```sh
curios compile                   # under `.curios/`, beside the governing manifest
curios compile -o hello          # somewhere else
curios compile scratch.crs -o s  # a loose program, which has nowhere else to go
curios compile --target browser  # a page, under `.curios/`
```

`--target` says what the program is built for: `native`, the default, writes an executable, and `browser` a directory holding a page. The command closes the program's tree with `write` and where the result stands, a page's directory with a trailing separator, whether or not anything had to be compiled. A declared executable's build is written under `.curios/` beside the governing manifest, under the target, nested under the package and named after the executable it built; `-o`/`--output <PATH>` writes it somewhere else instead. A program no package declares — a loose file, or standard input — is filed nowhere, so `--output` is the only place it can go, and without one the command is refused naming the flag that would build it.

**The result is self-contained.** Every [`[foreign]`](#the-manifest) module that answers a declaration the program reaches is compiled into the executable beside the program, and nothing is read from the cabinet or the package at run time ([A foreign row states a delivery for each target](design/tools/a-foreign-row-states-a-delivery-for-each-target.md)). It is linked before it is written, as `run` links before it runs: a declaration the program reaches that no module and no operation of the host answers, a declaration two rows claim and an export of another type than its declaration crosses as are each refused, so no executable is written that would refuse to start.

### The page

`--target browser` writes the module Binaryen produced, with no Cranelift step after it, beside the page that runs it, all in one directory: under `.curios/` at `browser/<package>/<name>/`, emptied first so it holds nothing an earlier build left, or the directory `--output` names, which is created where it is missing and keeps whatever else it holds, a file of the page replacing the one of its name. The directory is the page whole, and copying it anywhere a static server serves deploys it.

| File | Is |
| --- | --- |
| `index.html` | the page: the import map, the loader, and one `<pre id="output">` the program's output is written into while it stands in the document — a gateway may remove it |
| `index.js` | the loader: the browser's host, what it is configured with, the program's rows, and an import of every gateway |
| `index.wasm` | the program |
| everything else | what the [`[foreign]`](#the-manifest) rows the program reaches put in the page |

The page's root is the program's package's root. A gateway the package carries stands at its `path` from `curios.toml`, a fetched one at `<row>.js`, a `[foreign.<row>.browser.static]` entry's files at its page path, and everything a dependency's rows put in the page under the dependency's name. Only a row the program reaches puts anything in the page.

**A gateway names its package's files relative to its own address**: the page's root is its package's root only where its package is the program.

An `import` the gateway writes resolves that way already; a URL it builds itself — for `fetch`, an image's `src`, a stylesheet — resolves against the document unless it is resolved against `import.meta.url`: from `web/dom.js`, `new URL("../art/pixel.png", import.meta.url)` reaches the entry `art`'s `pixel.png` in the package's own page and in every page that depends on it, where a bare `art/pixel.png` reaches it only in the first.

The path is the page's: a `static` entry's files stand at its key, whatever directory of the package the entry reads them from, and `run --target browser` serves the same layout the written directory holds. A page path two files claim is refused naming both, and nothing written is root-absolute, so the page works under any path.

The program and every JavaScript file the page loads are held to the build: the loader fetches `index.wasm` against its integrity, `index.html` states the loader's, and the import map states every other `.js` and `.mjs` file's, each the `sha256-` form of its SHA-256, so one changed after the build fails to load. A browser that predates import-map integrity loads the gateways unchecked.

The program runs on the page's main thread, its output written to the `<pre>` and, line by line, to the console, where a program the runtime stops says why in both. Its arguments are the page's name — the executable's, a loose file's stem, or `stdin` — then each `arg=` of the URL's query in order, percent-decoded to its bytes, `+` included as itself: `index.html?arg=one&arg=two%20words`. `arg` is the one key the page claims in its query; every other key is a gateway's to read. Before the program runs, a gateway's export that is no function is refused, naming the row.

A framed page — one whose window has a parent other than itself — posts to that parent, at its own origin alone: `{ type: "started" }` before the program runs, and `{ type: "outcome", outcome }` after, `outcome` being `{ stdout, stderr, exitCode, trap, answer }` with each stream whole and `answer` what a test program's entry answered, the bytes of what its test found as a `Uint8Array`, or `null` for a program, which answers nothing a host reads, an exit or a trap. It is how the runner of [`test --target browser`](#serving-the-tests) reads each test, and any embedder's to use.

Each export a row's `answers` table names is a function answering the declaration it is named for, held to the row where the page loads it: an export that is no function is refused before the program runs, naming the row. It is handed each operand as the JavaScript value its wire type names, a copy it owns, then an `AbortSignal` that fires where the call is cancelled, and it answers the row's results in the same values — nothing for a row with none, the value for a row with one, and an object keyed by exactly the results' labels for a row with several — at once, or as a promise of them, which the call waits on while the rest of the program runs. A row declared `-> Never` is answered by an export that ends the program by throwing. An answer outside the row, or a promise that rejects, stops the program naming the row; a value of another JavaScript type is refused rather than converted: a `Number` where a `Nat` is declared stops the program.

| Wire type | Handed and answered as |
| --- | --- |
| `Nat` | a `BigInt` from `0n` to 2⁶⁴ − 1 |
| `Int` | a `BigInt` from −2⁶³ to 2⁶³ − 1 |
| `Bool` | a boolean |
| `Byte` | a number from 0 to 255 |
| `Flt` | a number |
| `Bytes`, `Handle` | a `Uint8Array` |
| `List(T)` | an array of what `T` is handed as |
| `Option(T)` | what `T` is handed as, or `null` |

A row that states both tables links its `native` module wherever the program runs natively, so `curios test` and `curios run` link a `.wasm` standing in for the gateway, and [`test --target browser`](#serving-the-tests) runs the same tests against the gateway itself. A row with only a `browser` table is refused natively, naming the table a stand-in goes in and the command that runs its tests against the gateway ([A foreign row states a delivery for each target](design/tools/a-foreign-row-states-a-delivery-for-each-target.md)).

The page needs WebAssembly's garbage collection and tail calls: it runs in Chrome 119, Firefox 121 and Safari 18.2 and every later version. A server must give `.js` and `.mjs` a JavaScript type; `index.wasm`'s type does not matter. Curios runs no build of what a gateway imports: TypeScript, a bundler or a minifier runs before `compile`, into a directory a `static` entry serves.

## `document`

```sh
curios document              # .curios/documentation/<name>/
curios document -o site      # somewhere else
curios document --std site   # the standard library
```

`curios document` writes a library's interface as pages, one a path, read off the compilation that builds it: what each module exposes, each declaration's head printed as written, every word and name in it painted by what it is and every name linked to where it is drawn whole, and the `---` documentation comments attached to each. The subject names the library — none for the governing package's, or `lib.crs` or a module its `mod` lines reach; a program, a loose file or standard input is refused.

`--std <DIR>` documents the standard library the compiler was built with instead, writing its pages under `DIR`: the record is read off an image the compiler was built with, so nothing is compiled and no sources are read. It takes no subject, the flag names where the pages go, and `--output` does not apply.

The pages are the library's consumers' view: a private declaration or module is absent rather than hidden, a type whose representation is private shows no constructors and is marked `opaque`, where one that shows them is marked `transparent`, a concept is marked `sealed` or `unsealed` the same way, a test never appears, and a `pub use` shows the declaration it re-exports by its head, the paragraph its note opens with and the path it is drawn whole at — unless that declaration's own module is private and so has no page, in which case the declaration is documented on the re-exporting module's page. The constructors or methods a `pub use` exposes beside their owner are shown as that line.

A reference into a dependency or the standard library renders as written, painted by what it names but unlinked: the pages document one library and link nothing outside it.

A page answers its path. Where the module above exposes a declaration under the module's own name, as `/std/Option` is a type and the module of what works on it, the page opens on that declaration, drawn whole: its source with its body left out, its note, and on the corner of its source the facts the source does not show, each one word — how its representation stands, `intrinsic` for what the compiler provides, `proof`, and `total` or `partial` for a definition. Then the page lists what the module exposes, first in brief, each name with the paragraph its note opens with, and then in full, in the order the module writes it: a name that is a module too carries the line that declares the module, `pub mod`, with the count of what it exposes, which leads to its own page, and a module that is no declaration stands after the names. Last come the witnesses the declaration takes part in, wherever the library declares them, each with the module that declares it, and the witnesses the module declares of anything else. The library's paths that have a page stand beside every page, the branch the page is on opened, and what the page holds beside that.

The landing page is the root module's page and opens with the manifest's `description`; any other page says under its heading what the `---` block above its `mod` says, where the declaration it opens on does not say the same. In prose, a pair of backticks encloses a code span.

What is written is `index.html`, one page per other module at its source path with the suffix `.crs.html` — `/json/parse/lexer` is `parse/lexer.crs.html`, so a module named `index` never lands on the landing page — and, under `static/`, the stylesheet, the three brand fonts with their licenses, the mark, the search index as one script, and the script that runs the rail and the field. Files are overwritten by name and nothing else in the directory is touched. How the pages themselves are built, and why the search index is a script rather than a file to fetch, is [`curios-document`'s](../curios-document/README.md).

A library that does not compile is not documented: its diagnostics are reported as `run` reports them, no page is written, and the exit is non-zero. Success closes the library's tree with `write` and the landing page, `<directory>/index.html` spelled from where the invocation stands, under `--std` as well.

## `test`

```sh
curios test                      # every test the package declares
curios test serve                # only the executable `serve`'s
curios test --filter /app/Map    # only the tests at or under /app/Map
curios test --filter /app/Map --filter /app/Set    # the tests under either
curios test --target browser     # the same tests, run by whatever opens the printed address
```

`curios test` runs declared tests — the [`test name = body;`](language.md#test-declarations) declarations. With no subject it runs the governing package's: its library's, then each executable's. A subject narrows the run to what it selects: an executable runs that executable's tests, a library module runs the library's, a file no unit declares runs its own against the standard library alone. `--filter` narrows whatever was selected to the tests at or under a path, compared a whole segment at a time, so `/app/Map` never selects a test under `/app/MapBuilder`; repeated, a test any of its paths selects runs, and each one that matches no test is an error naming it.

Each unit is compiled as its own test program — the same compilation `run` performs, with the final term replaced by a synthesized scheduler over that unit's selected tests — and every selected test runs in an instantiation of its own. The unit itself is compiled with every test it declares whatever the filter, and is the one `wonder` and `lint` read; the filter narrows the program, so only the tests it selects are built past the front end, and a unit it selects none from builds no program. A unit holds its tests only here and in what `wonder` and `lint` ask of it: `run`, `compile` and a unit compiled for the units after it leave them out.

The report is on standard error, on each subject's own tree, after the nodes that built its test program ([What a command reports](#what-a-command-reports)). A unit and its test program are each a subject, so a library is named twice: checked with its tests and filed, then built into the program that runs them.

```text
/demo
├─ check with its tests; 0.3s
└─ store; 0.0s
/demo
├─ check; 0.0s
├─ build; 0.2s
├─ emit; 0.4s
├─ /demo/shapes/a_square_has_four_sides; ✓ passed in 0.0s
├─ /demo/two_is_three; ✗ failed in 0.0s; lib.crs:7:6
│    not equal
│      obtained: `2`
│      expected: `3`
├─ /demo/render/draws_a_square
│  > ■■■■
│  ✓ passed in 0.1s
└─ 2 passed, 1 failed; 0.1s
```

Each selected test is a node, opened by its path before the test runs, so a test that never ends is the one whose line stands open, and closed by how it ended, led by its mark, and what it took: `✓ passed`, or `✗` and `failed`, `trapped` or `exited N`, then where the test's name is written, as a report closes on its place, since a test that did not pass is one its reader goes to. A test that exits on its own has answered nothing, so it is `exited N` whatever `N`, `0` included. The tests run module by module, the modules in the order each declares its first test.

What a test writes is never held: it reaches the stream the test wrote it on, standard output or standard error, as it is written, each line behind a frame, the tree's trunk and a quote mark, `│  >`; the mark of a line on standard error is painted apart where colour is seen. Its first byte ends the line its node opened on, and the node closes beneath what was written, its outcome's mark in the quote's column, whichever stream that was; a stream sent elsewhere carries its lines behind the frame too. Beneath a test's outcome stands what only the runner holds: a trap's text, and what a failing test found — each check that failed on a line, the steps that reached it leading, a `Test/step`'s label and `case 2` for the third case of a `Test/all`, as in `parsing: case 2: not equal`, and beneath it each value it compared behind its label, in backticks. Two things write past the frame and stand unframed: a child process a test starts with a stream of its own inherited, and a test that takes the terminal.

A subject's tally closes its tree and the run's is the last line where more than one subject ran, one subject's tally being the run's, each with what its tests took together where any ran: `N passed, N failed` always, then `trapped`, `exited` and `skipped`, the tests the filter left out, when they happened. A subject that declares no test, or that the filter selects nothing from, closes with its tally at once.

A claim about every instance is [a `let`, not a test](language.md#test-declarations): a broken one fails the build, and `curios test` never sees it.

The test *outcomes* are never cached — every invocation runs every selected test, however much of the compiling was reused.

### Serving the tests

`--target browser` builds every subject's test program as its [page](#the-page) instead, as `compile --target browser` would build it, and serves on `127.0.0.1` — at port 8000 unless `--port <PORT>` names another, and at a free one under `--port 0` — until every selected test has an outcome. It prints the address on a line under no subject, `serve; http://127.0.0.1:8000/`, and opens no browser: whatever opens the address runs the tests — a person, a headless browser a script starts against the printed port, a device reaching the machine.

A script runs them with no window by holding a headless browser open until the command exits — `timeout 120 google-chrome --headless=new --user-data-dir=<a fresh directory> http://127.0.0.1:<port>/` — rather than by a mode that dumps the page at its load event, which ends the run before its first outcome.

Everything is built before anything is served, so a `--filter` matching nothing and a test reaching a row with no `browser` table are refused as [`compile`](#compile) refuses them, before the address is printed.

The root is the run: a runner that opens each test in an iframe of its own, one at a time in the order native runs them, each a fresh realm with fresh instances of every gateway module. `index.html` beside it is the subject in hand's page, the test's index as the program's argument, and switches to the next subject with the last outcome of the one before, so no subject's page has a name of its own.

The runner drives the run over a WebSocket it opens at `/`, which the command takes from one page at a time: it is sent the test in hand, whose node the command opens as it sends it; the page posts each write its test makes to the runner as it is made, and its outcome when its program ends ([The page](#the-page)), and the runner sends each on over the socket. The command writes what a test wrote as it arrives, each line behind its frame as a native test's is, so the test in hand is the one whose line stands open and the two streams keep the order they were written in; the outcome is answered with what the command wrote since its last reply and the next test to open, and the first reply carries every line of the run written before it.

A socket asked for while another drives the run is refused, so a second tab runs nothing and says so; one that closes, a reload's or a closed tab's, lets the run go, and the next page to open resumes it at the test in hand, which runs again. A socket is refused too where its `Origin` names another site, and a page whose loader never starts — a gateway that fails to import — is reported trapped.

Past the address each served subject is named again and its tests stand on a tree of their own, written as native writes them, each whole as its outcome arrives: its node, what its page kept of the two streams — relayed on the command's own, standard output first — and how it ended, timed from the test's being handed to a page. The runner page shows every line the command wrote from its first, a reloaded runner all of them again; a subject the filter selects nothing from is never served, and closes with its tally as it is built, on both. Once the last outcome is in, the command stops serving and exits as `test` exits; a rerun is the command run again.

## `curate`

```sh
curios curate
```

`curate` materializes what the manifests reference. It and [`pin`](#pin), whose `--url` and `--refresh` fetch through the same delivery, are the only commands that fetch — the compiler itself never does, and [`run --target browser`](#serving-a-page) only listens, on loopback. A delivered tree is accepted against its `hash` whatever transport produced it, and a delivery that fails its hash is refused whoever fetched it ([`curios-package`'s README](../curios-package/README.md#a-pin-is-derived-by-one-command-and-checked-everywhere-else)).

It reaches the network through programs already on the machine ([why](../curios-package/README.md#a-pin-is-derived-by-one-command-and-checked-everywhere-else)). A package tree comes through `git`, and a `[foreign]` module through `curl` or `wget`, whichever is on `PATH`. A project with no `[foreign]` row that names a `url` needs neither of those two; one that has one and has neither installed is refused naming the module and both programs.

Each fetch is a subject of its own — a package by the prefix it mounts, a module by its row's name — whose one node stands open while it arrives, and the last line counts what was brought in, `0 fetched` where the cabinet already held everything:

```text
/json
└─ fetch; 1.4s
sha2
└─ fetch; 0.3s
2 fetched
```

A fetch whose transport fails closes as `fetch; failed in 2.0s`, and one whose delivery is not what its row pins as `fetch; refused in 1.4s`; why follows the tree, and nothing after it is fetched.

## `pin`

```sh
curios pin foreign sha2 --target native --path foreign/libsha2.wasm   # a module this package carries
curios pin foreign sha2 --target native --url https://…/libsha2.wasm  # one fetched and filed in the cabinet
curios pin foreign dom --target browser --path web/dom.js             # a gateway a browser links
curios pin foreign sha2 --target native --refresh                     # deliver again, pin what arrives
curios pin dependency json --git https://…/json --rev abc123
curios pin dependency json --rev def456                 # the same repository, a new revision
curios pin dependency json --path ../json               # a live sibling on disk
curios pin dependency json --refresh
curios pin dependency json --rev def456 --check         # write nothing, exit nonzero if it would
```

`pin` is the only command that writes a manifest, and the only one that derives a hash from a delivery rather than checking one against it. Adding a row and re-pinning one are the same command ([why](../curios-package/README.md#a-pin-is-derived-by-one-command-and-checked-everywhere-else)).

The name is a positional and always required, on both forms, so `--rev` and `--refresh` have a row to name. What the name *is* differs, and so does what can be checked about it:

| | `pin foreign <NAME>` | `pin dependency <NAME>` |
| --- | --- | --- |
| the name is | a handle for this command; no program refers to it | the package's mount prefix, and every consumer's only way to refer to it |
| who chooses it | you | the dependency, in its own `curios.toml` |
| a disagreement | impossible; there is nothing to disagree with | refused naming both, before anything is written |

Exactly one delivery form is required. `--path`, and `--url` for a module or `--git` for a repository, say where the row points, each named for the key it writes; `--rev` alone keeps the repository and moves the revision; `--refresh` keeps everything and delivers again. `--git` requires `--rev`. A row is written stating one source, so a pin clears every other source's key the row stated: a `member = true` row pinned `--path` states `path` alone.

`member` and `catalog` rows are not written here: each is one word written in the manifest, naming a member of the governing umbrella or a row of its catalog.

| Flag | Is |
| --- | --- |
| `--path <PATH>` | a module the package carries, spelled plainly and relative as an executable's path is; or a dependency in a separate project on disk, such as `../json`. A package of the same project is a `member` of its umbrella, which this command does not write |
| `--url <URL>` | a module fetched once and filed in the cabinet |
| `--git <URL>` | a dependency's repository, pinned with `--rev` |
| `--rev <REV>` | a dependency's revision to pin. Alone on an existing row, it re-pins that row's own URL to a new revision |
| `--refresh` | deliver again whatever the row already names and pin what arrives. The only spelling permitted to overwrite a hash that was not asked to move |
| `--check` | write nothing and exit nonzero if the manifest would change. The delivery still happens, and only the write is withheld |

What was fetched is filed in the cabinet under the digest it turns out to have, so the `curate` that follows finds it there.

The row is the subject, by its name. A module is described before its pin is written: what arrived is a node of its own, `deliver`, open while a fetch arrives and closed by its size and whether it is a WebAssembly module, with **what it exports** beneath, which is what an entry of the row's `answers` table may name as the export that answers a declaration. The `pin` node closes the tree with what became of the row — `recorded` where the manifest was written, `unchanged` where the row already said so, `would change` under `--check` — and each key it set beneath, beside what the key said before where it said anything:

```text
sha2
├─ deliver; 12,484 bytes, a WebAssembly module
│    exports: alloc, memory, SHA256_Digest
│    nothing verified this download — the hash below is derived from it, not checked against it
└─ pin; recorded
     url: https://example.org/libsha2.wasm
     hash: f1:93a44bbb96c751218e4c00d479e4c14358122a389acca16205b1e4d0dc5f9476
```

The export list is read by compiling the module, so a module this toolchain cannot link reports why instead. The line about verification appears only for a fetch. A dependency has no `deliver` node, and its `pin` node stands open while a repository arrives. A node open when the pin stops closes with how — `failed` where the transport brought nothing, `refused` where a rule refused what it brought — and a pin refused before any delivery began reports why and nothing else.

`--target` is required on `pin foreign`, and names the table of the row that is written, `[foreign.<NAME>.native]` or `[foreign.<NAME>.browser]`, inline where its author wrote it inline; the other table is left as it was. A browser delivery's exports are not read here: the page holds them to its rows where it loads the gateway. The manifest is edited in place, keeping its comments and order, then read back and compared key by key with what went in, so no row the command was not asked about changes. Pinning a row to what it already says writes nothing and reports that it did.

A pin reads only the row it writes, so it repairs a row the manifest refuses: `--refresh` gives a row the `hash` it lacks, and `--path` or `--url` gives a row written by hand the table it names no file in. A value on the way to the row of another kind than the manifest states there — `dependencies = 3`, a row written as a string — is refused at it, as the manifest's reader refuses it, before anything is delivered; a table written inline is written into inline. What is held to the manifest's rules is the manifest as the pin would leave it, read before it is written: one that would still be refused, for this row or another, is refused naming why, and nothing is written.

## `new`

```sh
curios new <DIR>
```

The directory's name is the package's, checked before anything is written, so a name the manifest could not hold writes nothing.

It writes the manifest, `lib.crs`, `exe.crs` and a `.gitignore` for `.curios/`, and no flag asks for fewer ([why](../curios-package/README.md#new-starts-a-package-whole)). Deleting either file makes the package a library alone or a program alone.

The directory is the subject, each file written a node, and the last node the command that runs what was written:

```text
hello/
├─ write; curios.toml
├─ write; lib.crs
├─ write; exe.crs
├─ write; .gitignore
└─ try; cd hello && curios run
```

## `lint`

```sh
curios lint              # the governing package entire: its library, then every executable, then its dependencies
curios lint app.crs      # one file, placed in its unit
curios lint -            # the program on standard input
```

A lint is an exact finding the compilation already has and nothing stops on: `run`, `compile` and `test` never mention one, `wonder` and the language server report them beside the errors and goals, and `lint` is where they turn into an exit code. There are seven, every one always on, and there is nothing to configure — what keeps a name is spelled in the program, and each message says how.

| Lint | Reports | Kept by |
| --- | --- | --- |
| `unused-import` | a `use` selector, or a glob, that no reference resolved through; a `pub use` is a re-export and its own use | deleting it |
| `unused-binder` | a parameter, `let` binder or pattern binder nothing references, implicit or shadowed included, where it is no `unused-let`'s; a hypothesis a proof the elaborator wrote from the facts in scope reads is referenced, though no written name reaches it, and a declaration holding a written goal reports none | writing `_` in its name's place, `()` where it binds `{}` |
| `unused-let` | a `let` whose value runs no action — no `!` stands in it — and none of whose binders anything references, `_` included: it does nothing, whatever its value, an action bound and never run among them; a binder a proof the elaborator wrote reads is referenced, as for `unused-binder` | deleting it, or running it with `!` where it holds an action |
| `unit-wildcard` | a written `_` that binds `{}`, as elaboration decided what it binds: the pattern of a `let` that runs an action, a part of one, a parameter, or a match pattern's payload or component — an arm's whole `_` excepted, which takes what the arms before it left | writing `()`, which refuses a value that stops being `{}` |
| `unused-declaration` | a non-`pub` `let`, `foreign`, `induct`, `struct` or `concept` unreachable from the unit's roots — its exported surface, its tests, its witnesses, the declarations a `_` keeps unused and its program's tail; a private `mod` none of whose declarations is reached is reported once, at the `mod` | deleting it, `pub`, or naming it `_x` |
| `unused-dependency` | a `[dependencies]` row whose package no reference in the library or any executable names; decided over the package, so reported only by the package-entire form | deleting the row |
| `renaming-binder` | a final arm's name after arms that each take the scrutinee apart, or a match's one arm that binds the whole value, under a head written as a name: it names again what the head already names | reading the head, and writing `_` after arms, or nothing for the lone arm |

Output is what `wonder` prints — each diagnostic, goal and lint rendered as `run` reports it, a blank line between — and the exit is the tri-state: 1 when a lint or an error was reported, 2 when only goals were, 0 when nothing was. A program that does not lower reports its error alone; one that lowers and is then refused reports its lints beside the refusal. Each unit is linted as `curios test` compiles it, its tests included, so a declaration only a test reads is reached.

Standard error narrates what had to be compiled, as [`wonder`](#wonder) does, and its last line counts what was found, the lints always and the errors and goals beside them where there were any — `2 lints`, `0 lints`, `1 lint, 1 error`, `0 lints, 1 goal`:

```text
/demo
├─ check with its tests; 0.3s
└─ store; 0.0s
demo
└─ check; 0.1s

2 lints
```

## `format`

```sh
curios format                   # every file the governing package declares, rewritten in place
curios format serve util.crs    # an executable's files, and one file
curios format proof.crsc        # a file of Core text
curios format --check           # write nothing, exit nonzero if anything would change
curios format - < draft.crs     # standard input, written canonically to standard output
curios format --core - < p.crsc # standard input, read as Core text
curios format --line-width 64 - # standard input, within 64 columns
```

Each subject names files, and what several subjects name is rewritten once: no subject is every file the governing package declares — its library's header and the modules its `mod` lines reach, then each executable's entry and the modules the entry reaches — an executable's name is its own, a file is itself whatever declares it, and `-` is standard input, written back to standard output, read as a program or, under `--core`, as Core text. A file nothing declares is formatted only when it is named, and a `.crsc` is formatted as the Core text it holds, read and not checked, so a file its reader refuses is formatted all the same.

There is one canonical style, and three settings: `line-width`, the width a line is written within, 50 through 100 columns counted in characters, and 100 where nothing states another; and `flat-match-width` and `flat-match-arms`, the most columns, from `match` to `end`, and the most arms a match keeps on one line — the columns 0 through the line width, 70 where nothing states another or the line width where that is narrower, and the arms 1 or more, 3 where nothing states another. A file takes its settings from one place. Where a package governs it — the package whose manifest is nearest above the file, or the one `--manifest` names — that is the package's [`[format]`](#the-manifest) table, or its umbrella's, and a flag given for such a file is refused before anything is written. Standard input and a file no package governs take the flags, so `curios format --line-width 80 - < file.crs` shows a package's file at another width without rewriting it. The rules and what they rejected are [A file is formatted as one place states](design/tools/a-file-is-formatted-as-one-place-states.md)'s.

The canonical form writes no byte-order mark, and writes a character that prints as nothing or as a blank inside a literal by its escape, `\u{200D}`.

Formatting is verified before anything is written — the output must reparse to exactly the same program or Core text, with every comment preserved — so a formatter defect refuses rather than corrupts.

A file that changes is a subject, spelled from where the invocation stands, with one node saying what became of it: `rewritten`, or `would change` under `--check`, which writes nothing and exits 1 where anything would. A file already canonical says nothing, and the last line counts both:

```text
lib.crs
└─ format; rewritten
shapes.crs
└─ format; rewritten
2 rewritten, 5 unchanged
```

A file that cannot be formatted closes as `format; refused`, why following, and nothing after it is formatted. Standard input formatted onto standard output is an answer, of which nothing is said; under `--check` it is a subject as a file is, `<stdin>`.

## `wonder`

`curios wonder [QUERY] [SUBJECT]` answers a question from the compilation that would build the subject, and executes nothing: with no query, the check. The subject is the one argument, read as every command reads its own, and a query is an option, one at most; `--witnesses`, `--footprint` and `--stage` are views beside `--name`, one at most, refused beside any other query, `--stage` alone being a query of its own. No word is both: `curios wonder tests` checks the executable named `tests`. The answer goes to stdout and nothing else does; standard error narrates the units the question had to compile, each checked and stored as a build's are ([What a command reports](#what-a-command-reports)), and, where the question is the check or `--tests`, the check of a program the cabinet did not hold, with a blank line then parting the trees from the answer. A question that reprints or reads a program — a rung, `--footprint`, `--at`, `--classes` — narrates what the program stands on and never the program, and one the cabinet answers whole narrates nothing. Exit 0 means the question was answered, including when the answer is a list of errors, and non-zero that it could not be — no such subject, no such stage, a scope that cannot be assembled — or that the program stopped before the answer, which exits as a build would: 2 when written goals alone stopped it, 1 otherwise.

| Query | Answers |
| --- | --- |
| none | every diagnostic, goal and lint — the errors and goals rendered as `run` reports them, each lint after them as [`lint`](#lint) describes — a blank line between; nothing when the subject compiles clean. A unit is asked under its written tail, never the test program `test` synthesizes |
| `--tests` | every test the subject declares, one path per line, in declaration order — the library's, then each executable's, for the package entire; nothing when it declares none, and nothing executes |
| `--stage <STAGE>` | the program's representation at one rung of the pipeline, reprinted. A rung the compilation reached is answered even when a later stage refuses: the rendering goes to stdout, what stopped the program goes to stderr, and the exit is 0. Only a program that stops *before* the rung has not answered, and exits 2 when written goals alone stopped it and 1 otherwise. Beside `--name`, the declaration its path names at a rung a unit has: `--name`'s block without its source, then, a blank line before, the declaration's part of the rung, printed as the rung prints its whole — at `text` its text parsed and reprinted, at `core` the items it lowered to in the subject's own lowering, and at `core-elab` its items as the unit that declares it was elaborated, a type's constructors and methods with it. A unit in scope keeps no lowering, so `core` of a declaration of one is refused, with the command that asks it of the file that holds it. Past `core-elab` the subject is a program, and from `anf` to `cps-optm` the part is what the rung holds of the declaration's functions, a loop the optimizer made of one in its caller among them: each at its binding site, under the headers of the functions it stands in, `…` for what their bodies hold besides, and the rows it reaches declared above it; at `wasm`, the Wasm functions the compiler made of them |
| `--at <FILE>:<LINE>:<COLUMN>` | what stands at a place. A name answers the declaration it names, as `--name`'s block: the path it is exposed at, which `--name` takes, or a witness's head as written; what its source does not show, and its source as the block's first entry, as `--name` writes them, its totality where the compilation established it; where it is a bare name a `use` line brought into scope, that line as it is written, an entry closed by where the line writes what brought the name, `use /std/{Nat, Eq}; app.crs:1:11`; and where its name is written. A local, where it is bound or read — a parameter of a function type or of a declaration's head, a payload's binder and an index among them — answers its name, the type it was bound at or settled at, and where it is bound, a block closed on `[local]` at its binding. A field's label answers the field, where a structure or a tuple type writes it as where it is read: its label, the structure that declares it, none for a tuple type's, and its type as the structure declares it, a block closed on `[field]` where the structure writes it, or at the label where no file holds the structure. A method a concept's later field reads answers the method. A segment of a path answers what it reaches: the family or the concept whose namespace it is, as a name of it, and a module a `mod` line declares as a block of its own closed on `[module]` where that line names it, or on the module's path where no file holds the line, which names the command that answers the module. A `use` line's names answer alike, a selector the declaration or the module it imports, and a `mod` line's name the module it declares. Where neither a name, a field nor a local stands, the narrowest term written there answers with the type it settled at, a block of its own closed on `[term]`, quoting it at its place. A place where no term stands either is refused, as a path that names nothing is. A place is written as a report closes on one: its file, spelled from where the reader stands, or `<stdin>` for the text on standard input, then a line and a column each counted from one, read from the right so a path holding a colon reads too. The file is the subject, so `--at` takes no other |
| `--classes` | every span of the file the subject is asked through that the compiler classes, a row a line in the order they are written, each four fields joined by tabs, `<FILE>:<LINE>:<COLUMN>`, `<LENGTH>`, `<CLASS>` and its modifiers — `declaration` where what the span names is declared there and `default-library` where it lies under `/std` or `/sys`, a space between the two, or `-` where neither holds, so every row splits alike: its place as a report closes on one, its length in scalar values as the column counts them, and its class by the name [Curios is coloured from one class set](design/tools/curios-is-coloured-from-one-class-set.md) gives it. A name is classed by what its declaration is declared as, over its path's last segment, each segment before it reaching a module a `module` of its own; a binder by the form that binds it — a `parameter`, a `type-parameter` where its stated type ends in a sort, a `local` for a `let`'s or a match arm's — where it is bound and read; a field's label and a method's name as the member they declare; and a contextual word, `and`, `test`, `concept` or `satisfy`, as a `keyword` where it opened a member. Each other span is classed as `--at` answers what stands there, and what it answers nothing of is no row, left to an editor's grammar. Answered off the check of the subject, as `--at` is: a file that does not elaborate classes what its lowering answers, and one that does not lower stops the question |
| `--name <PATH>` | the declaration an absolute path names in the subject's scope — the subject's own, a dependency's or the standard library's — as a block: the path asked; one line of what its source does not show, each a word its documentation page says the same fact in — `transparent` or `opaque` of a type and `unsealed` or `sealed` of a concept, as its representation is public or kept, `intrinsic` where the compiler provides it, `derived` of a witness whose body the compiler writes, `proof`, and of a definition `total`, or `partial` where it is not known to terminate on every input, as the certifier recorded it; the path it is declared at, where a module exports it at the one asked and the block closes on a place; for a type or a concept, how many witnesses in scope it takes part in, where any does, with the `--name <PATH> --witnesses` command that lists them; its source as one entry behind a `├─`, the rest of it on the trunk beneath — its doc comment and its head as its author wrote them, a family and a structure with their members where their representation is public and by their head alone where it is kept, a concept with its methods, every body left out, and a constructor or a method within its owner's head and closing word, its siblings left out; then where its name is written, or the path it is declared at where no file holds it, which is the path asked where the compiler provides it. A declaration of Core text shows no source in its block, which states first what it is declared as, its type and its members, and its whole text follows, a blank line before. A path that names a module is answered with the module, as a block: the module's path; `public module` or `private module` as its `mod` line is written — a root, which no line declares, is a `module` — with how many names and modules it exposes, a count of none unsaid; an entry behind a `├─` for each thing it exposes, the rest of an entry on the trunk beneath; and `[module]` where its `mod` line names it, or its path where no file holds the line. An entry is the declaration as its source writes it with what a body elides left out: a definition and a witness their head; a family, a structure and a concept whole, a family whose representation is kept its head alone; the members a module exposes beside their owner the line that does it, `pub use Option/{some, none}`; and a public child the line that declares it with what it exposes in turn, `pub mod read, exposing 12 names`. Its names come first, in the order the module's documentation page lists them, then its witnesses, then its children; a module no page shows — one beneath a `mod` line written without `pub`, a program's own, a loose module's — is read off the lowering, its names in the order its body writes them, and is answered whether or not its unit checks. A facade's path, which names a declaration and a module, is answered with the declaration and then the module, a blank line between, the module's block listing what it exposes beside that declaration, `exposing 17 more names`. A path that names nothing in scope is refused as a program that wrote it is told, with the step that failed and the nearest name a reader may write there, `did you mean …`; a path a subject of Core text names nothing at is refused as `unknown declaration` |
| `--witnesses` | beside `--name`, the witnesses in scope that the declaration its path names takes part in: `--name`'s block with an entry for each witness in place of the declaration's source — its head as its author wrote it, then `;` and where it is written, or the module that declares it where no file holds it, as a block closes a further place. Each unit's witnesses stand together, in the order of the scope and by place within it. A declaration that is neither a type nor a concept has none |
| `--footprint` | beside `--name`, what the compiled program holds of each function of the text of the declaration its path names, and what each weighs there: `--name`'s block without its source, then, a blank line before, a row for each function in the order the declaration writes them, four fields tab-separated — its name as the compiler printed it, `-` where it has none; what became of it: `not linked` where the program holds nothing of the declaration, as its one row, `pruned before CPS` where the erased program's optimizer left nothing of it, `kept`, `copied <n>` where a pass copied it into `n` functions, `inlined into <names>` where its code stands in the bodies of the functions named, `absorbed` where nothing of it stands, and `heap twin` for the copy that runs a function on a recursion on the heap, under that function's row; the bytes its bodies weigh in the emitted module, before Binaryen; and the values those bodies rebuild in place — both `-` for a function with no body of its own. It takes a program. Nothing is judged: a row states the fate and stops |
| `--false` | whether the checkers admit the program's closing term at `False`, the question the [soundness board](../xboard/README.md) asks of every witness: a block, as every answer is, headed by the verdict, refused or admitted at `False`; a fact for each checker the subject meets and what it made of the term, `elaborator: refused`, `admitted`, or `not asked` where the elaborator built the certifier no module to judge; and closed on `[refused]` and who refused it — each checker that refused, with each error it raised by the name its variant is declared under and where it stood, in the declaration it names or at the closing term — or on `[admitted]` and the type; then each refusal as its checker reports it, a blank line before each. A program is put whole, its declarations and its closing term one module: to the elaborator, its erasure obligations reported rather than raised so a program the elaborator alone refuses still reaches the certifier, then to the certifier over what it built; Core text to the certifier alone. The answer exits 0 whichever way it went, and a subject that does not lower or read was put to no checker and stops as a build does. A term both checkers admit is a proof of `False`: a flaw in the checkers, for the board to file |
| `--server` | every question the others answer, asked by an editor over the language server protocol on standard input and output: what an editor integration launches. It publishes a document's diagnostics as the document is edited, telling each check's progress as it goes where the editor takes progress, a report for each unit the check compiles, and every open document's when one is saved or, where the editor watches files at the server's request, when a module or a manifest changes on disk, read from the disk as it then stands; it formats a document, and answers where the name under a cursor is declared, what stands under the cursor as `--at` answers it — a declaration's head, a member's within its owner's head and closing word, or its path where it states none, the facts `--at`'s block states and its prose, and the `use` line a bare name came in through, a local or a field with its type, a module, or the term written there with its type, over the range it covers, as a hover — where a local or a field under the cursor is bound, what the document declares, the selections a cursor grows through, each term around it out to its declaration, a fold for each declaration over several lines, a link from each segment of a path that reaches a module to the line of its `mod` line, where each witness of the concept or the type under the cursor is written, and the document's semantic tokens, the rows `--classes` lists, asking the editor to request them again once a check is in where it takes the request: each from the document's last check, so none waits on a check in hand, and a declaration edited since answers once it is checked again. It takes no subject, and one written beside it is refused where the line is parsed |

No subject at all is the governing package entire for the check and `--tests`, and the sole or `default` executable for `--classes`, `--stage`, `--footprint` and `--false`; `--footprint` and `--false` need a program, and `--stage` one past the three rungs a unit has, so each refuses a text written as a module where it needs one. For `--name` and its other views it is the widest scope where the command stands: that executable's, else the package's library's where it declares no executable or several and no `default`, and outside every package the prelude's alone, the scope a loose program has, so `curios wonder --name /std/List/map` answers wherever it is asked.

`--at` is asked about the file its place names, read as every command reads a file it is named. It is answered off the check of the subject, as the editor's hover is: the lowering answers every place whatever the check concludes, so a file that does not elaborate answers as one that does, and only one that does not lower stops the question as it stops a build; a declaration elaboration established answers its type, and its totality where the certifier judged it, a declaration of a unit the subject stands on always and one of the subject's own where it elaborated.

`--name` takes a path as a program outside every module writes it: a declaration's own path, or one a module exports it at, so `/std/Bool/and` answers the `/sys` declaration `/std` exports there. It is answered off the unit that declares the name, and elaborates the subject only where the subject declares it, so a name of a unit in scope is answered whatever state the subject is in, and one of the subject's own is answered once the subject checks.

A declaration of the library the compiler was built with holds its source's name and no file, whichever checkout the compiler was built in, so the block `--at` and `--name` answer with closes on its path; its head, prose and whole text are read off the text the compiler holds of it. A package named `std` stands in for that library, its declarations placed in the package's files.

`--stage` reprints a unit too — a library, selected through one of its files, or a loose module — at the three rungs a unit has: `text` is the parse of the file asked through, or of what standard input held, `core` the unit's lowered declarations, and `core-elab` its elaborated ones. From `anf` on a program's entry roots what is printed, and a unit is refused as it is by any command that takes a program.

A file a package holds that no `mod` reaches is in no unit, so it is checked on its own — as a program when a final term follows its items, and as a module of its own otherwise, as a file no manifest above it claims is. A module of the standard library is [the package named `std`](#what-a-package-is-made-of)'s: a question about `curios-prelude-build/std/List.crs` is answered against the prelude it is part of, and costs the closure of the edit.

A `.crsc`, or standard input under `--core`, is Core text, the form `--stage core` and `core-elab` print a program in, and the check reads it as the program or module it writes, over the standard library alone, as a loose file is compiled: a text closing with a term is a program, its declarations a unit and that term judged at `Io({})` and erased over it, as it is written — its tails are not run through `/std/Program`, and a type it states for the term is one the check puts it at or is refused — and a text closing with none is a module, a unit mounted at its stem, or at `/stdin`. The check reports the reader's refusal where the text stands, and the certifier's and erasure's at the name of the declaration they refuse, or at the closing term. A module `--stage core-elab` printed reads back so and checks clean. The other queries ask what the text's unit holds: `--tests` lists the tests a module of Core text writes; `--stage` reprints a program's rungs from `core-elab` on, as the program it was dumped from prints them, and a module's at `core-elab`, its one rung, as the module it was dumped from prints it, and refuses `text` and `core`, a lowering's rungs; `--name` answers a declaration the text writes off the unit it reads to — its path, which is the one asked, Core text exporting nothing at another, what it is declared as, its type, its totality, its members, where the text writes it and that text — and any other name as the prelude's scope answers it, which `--witnesses` and `--stage` beside it follow; and `--footprint` reads a program's rows as a program's. `--false` puts the term the text closes with at `False`, refusing a type it states for the term that is not `False`. `--at` and `--classes`, answered off a lowering, refuse a `.crsc`.

`--footprint` says whether an optimization fired on the code a declaration holds. A function is `absorbed` when nothing in the compiled program was made of it — inlined whole into its callers as straight-line code, or pruned once something else was — and `inlined into` names the functions a pass put it in where some of it still stands as code of its own there, as a loop does. What a function was made of is kept on every copy, never read off a name, so a function named after nothing is found and two declarations' locals of one name are told apart. The rows are read off the program that ships.

The stages, in the order the compiler passes them:

| Stage | Is |
| --- | --- |
| `text` | the surface syntax tree, reprinted |
| `core` | the lowered core term, which nothing has checked yet |
| `core-elab` | the same after elaboration, every inferred term filled in — the module every later stage reads |
| `anf` | the erased term, in A-normal form |
| `anf-optm` | the erased term in A-normal form, after optimization |
| `cps` | the program in continuation-passing style |
| `cps-optm` | the program in continuation-passing style, after optimization |
| `wasm` | the emitted WebAssembly module |

The rungs end at the module the compiler emits. What Binaryen makes of it is the module [`compile --target browser`](#compile) writes beside its page, to be read with a WebAssembly tool of one's own.

## `profile`

```sh
curios profile .curios/profile.tsv
```

Reads back a stream [`--profile`](#flags) filed and writes its summary to stdout: the spans by total time, the magnitude sites beneath them, and — for a run that never returned — the stack it was inside when it stopped.

`PATH` is spelled as it was to `--profile`. Reading is a pass of its own, not the profiled run's last step ([`curios-profile`](../curios-profile/README.md#the-library-emits-records-and-aggregation-is-one-of-its-consumers)).

## The manifest

A `curios.toml` declares a package or an umbrella, never both, and one that declares neither is refused naming what each would have had to state. Unknown keys are refused too ([`curios-package`'s README](../curios-package/README.md#a-manifest-is-read-strictly)). What a manifest states is refused where it states it: the report stands at the key, the value or the row it is about, with that line of the manifest shown ([What a command reports](#what-a-command-reports)), and at the file where it is about the manifest as a whole.

| Package key | Is |
| --- | --- |
| `name` | required: the package's canonical name, its mount prefix and every consumer's only way to refer to it |
| `compiler` | required for a standalone package: the Curios version it is written for, a bare SemVer version, or the oldest one, after `>=`; an umbrella member inherits this from its umbrella instead |
| `description` | one line, which opens the landing page [`document`](#document) writes |
| `default` | which executable a bare `run`, `compile` or `test` means; it may name a key of `[executables]` or the package's own `exe.crs` program |
| `[format]` | how [`format`](#format) writes the package's files: `line-width`, the columns a line is written within, 50 through 100; `flat-match-width` and `flat-match-arms`, the most columns and arms a match keeps on one line; an umbrella member inherits this from its umbrella instead |
| `[dependencies]` | one row per package this one reaches, keyed by that package's name |
| `[executables]` | one entry per program beyond `exe.crs`, keyed by the name `run` takes: `path` when the file is not `<name>.crs` |
| `[foreign]` | one row per provider of this package's [`foreign` declarations](language.md#foreign-declarations), keyed by the handle `pin foreign` takes: what each target links, in `[foreign.<handle>.native]` and `[foreign.<handle>.browser]`, and what its exports answer |

```toml
name = "hello"
compiler = "0.16.9"
default = "bench"

[executables]
bench = {}                            # compiled from bench.crs
serve = { path = "tools/serve.crs" }  # unless the entry says otherwise
```

An executable is named by one identifier, its key; a name written twice is refused where it is written again, as TOML refuses any key written twice. An executable's `path` is spelled plainly, relative to the manifest: no `.`, `..` or leading `/`. Its own modules load from the directory it sits in, so a program with modules of its own and a directory to itself is `path = "tools/serve/serve.crs"` beside `tools/serve/*.crs`.

`compiler` states the one version a package is written for, or the oldest: `compiler = "0.16.9"` accepts that version and no other, and `compiler = ">= 0.16.9"` that version and every later one, each by SemVer precedence, so build metadata is ignored and a pre-release is a version of its own. No other comparison is read. A minimum is its author's word that the package is meant to outlive the compiler that built it ([`curios-package`'s README](../curios-package/README.md#a-compiler-version-belongs-to-the-package-or-its-enumerating-umbrella)). A compiler the field does not accept is refused before compilation or cached execution, naming the manifest, the version it states and the running one. Every dependency is checked against its own effective version too, so a pinned dependency is one that accepts the compiler that builds it. A standalone package must state one: there is no implicit version, so a manifest without `compiler` is refused, naming the line that would satisfy the running compiler, and `curios new` writes that line when it starts a package. Loose programs and standard input have no manifest requirement.

An umbrella must declare `compiler`, and every package its `members` enumerates inherits that version. A member cannot declare a local override; the refusal points to its umbrella. A package merely beneath an umbrella directory is still standalone unless enumerated. Inheritance follows each dependency's own membership, never its consumer's umbrella. A dependency delivered without the umbrella it needs cannot inherit and must declare a version of its own in its manifest.

`[format]` is inherited as `compiler` is: an umbrella's table is every member's, a member that writes one of its own is refused naming its umbrella, and a package no umbrella enumerates states its own or takes every setting's default. A line width outside 50 through 100, a flat match wider than the line width or of no arms, and a key the table does not take are refused where the manifest is read.

```toml
[format]
line-width = 72
```

A `[foreign]` row, keyed by a handle for `curios pin` that no program refers to, names a module in the table of the target that links it, and says which of its exports answers each of the package's `foreign` declarations it is named for:

```toml
[foreign.sha2.native]            # the WebAssembly module the native target links
path = "foreign/libsha2.wasm"    # or `url`, never both
hash = "f1:9c3a…"                # either way

[foreign.sha2.answers]
"digest/sha256" = "SHA256_Digest"  # in the package `crypto`: `/crypto/digest/sha256`
```

A row's `browser` table names the JavaScript gateway a browser links instead, its exports answering the same declarations, and its `static` table what the page serves beside it:

```toml
[foreign.dom.browser]            # the JavaScript gateway a browser links
path = "web/dom.js"
hash = "f1:…"

[foreign.dom.browser.static]     # page path = package path
"app" = "build/app"
"logo.png" = "art/logo.png"
```

A row states either table or both, and either may be written inline under the row's own header, `native = { path = "…", hash = "…" }` beneath `[foreign.sha2]`; a row with neither is refused where the manifest is read, saying where a module goes. A target that reaches a declaration whose row lacks its table is refused where the program is linked, naming the row and the table ([A foreign row states a delivery for each target](design/tools/a-foreign-row-states-a-delivery-for-each-target.md)). A handle is one identifier, and a handle written twice is refused where it is written again, as TOML refuses any key written twice.

Both sides of a `static` entry are plain relative paths, as an executable's `path` is, and none reads from `.curios/`. No file of any package may stand at `index.html`, `index.js` or `index.wasm`, the names a page holds for itself: a `static` key, a carried gateway's `path` and a fetched gateway in a row named `index` that would stand on one are refused where the manifest is read, the rule holding for a library as for a program.

`path` names a module the package carries, spelled plainly as an executable's is; `url` names one fetched once and filed in the cabinet. `hash` is stated for both ([why](../curios-package/README.md#a-pin-is-derived-by-one-command-and-checked-everywhere-else)).

`f1:` is plain SHA-256 over the file's bytes, so `sha256sum` computes it and a release's checksum file already states it. A delivery that fails its hash is refused naming both digests, whichever way it arrived.

A row's `answers` table maps each declaration to the export of that module that answers it. A module answers its own package's declarations: each is a `foreign` of the package's library, named from the package's root and spelled plainly, as an executable's `path` is, so a row of another package or of the standard library has no spelling here. A path written in full, `/crypto/digest/sha256`, is refused with the spelling it has; an entry that names no `foreign` the library declares is refused where the library is compiled, listing the ones it does; and a package with no `lib.crs` has no declaration for a module to answer. A declaration is answered once: the table is keyed by what is answered, so a row names one export for it, and a second row naming one for a declaration another row answers is refused where the manifest is read, naming both rows. Export names are local to their module, so two rows may name the same export.

A `foreign` no entry names is the host's to answer. A program asks only for what it reaches: a declaration nothing calls needs no implementation, and one a program reaches that neither a module nor the host answers is refused before the program starts, naming it. An export is held to its declaration when the two are linked, so a module that answers another type is refused there as well.

| Umbrella key | Is |
| --- | --- |
| `compiler` | required: the Curios version all enumerated members are written for, which each inherits |
| `[format]` | how [`format`](#format) writes every enumerated member's files, which they inherit |
| `members` | the packages this umbrella governs, as paths; an umbrella governs a package **only if it enumerates it**, so a directory nothing enumerates is governed by nothing above it, however deep it sits |
| `[catalog]` | pins the members may draw on, keyed as a dependency table is |

A dependency is pinned exactly, and its name is how every consumer refers to it. A package named `json` mounts at `/json` and no consumer may rename it ([the four laws](../curios-package/README.md#the-four-laws)). Two dependents that pin one name two different ways is a refusal naming both, and it fires before anything is compiled.

```toml
name = "app"
compiler = "0.16.9"

[dependencies]
json = { git = "https://github.com/you/json", rev = "…", hash = "c1:…" }
shape = { path = "../shape" }
base = { member = true }
```

A row's source is the key it states, one of these and no other: a second is refused where it is written, and so is a marker written `false`.

| Source | Beside it | Is |
| --- | --- | --- |
| `git = "<URL>"` | `rev`, `hash` | a repository at an exact revision, accepted by hash |
| `path = "<PATH>"` | — | a live sibling on disk, for a project with no umbrella over it |
| `member = true` | — | a live member of the governing umbrella, unpinned |
| `catalog = true` | — | the governing umbrella's `[catalog]` row of this name |

No `git` or `rev` may begin with `-` ([`curios-package`'s README](../curios-package/README.md#a-manifest-is-read-strictly)). A `[catalog]` row may name neither `member` nor `catalog`: umbrellas do not nest. A catalog row on its own fetches nothing: activation lives in the package that names it.

```toml
compiler = "0.16.9"
members = ["app", "base"]

[catalog]
json = { git = "https://github.com/you/json", rev = "…", hash = "c1:…" }
```

## Exit codes

Exit status is a tri-state:

| Code | Meaning |
| --- | --- |
| `0` | compiled, and for `run`, the program itself exited 0; for [`wonder`](#wonder), the question was answered, an answer that lists errors or goals included |
| `1` | a hard error, alone or beside written goals; a command line that does not parse; for `lint`, a lint; for `test`, any test that failed, trapped, exited or could not be built, and a `--filter` that matched nothing |
| `2` | the program contains [written goals](language.md#written-goals) (`?`) and nothing else stopped it, and their report went to stderr — for `wonder --stage`, `--footprint` and `--tests` too, when goals alone kept them from answering |

A running program's own exit code passes through untouched: once the program has started, the code is the program's, which the tri-state cannot tell from the compiler's, since a program may exit 1 or 2 itself. Tooling that needs to know whether a program was ever started checks it first, with [`lint`](#lint) or [`wonder`](#wonder), or runs the executable [`compile`](#compile) wrote.

`run --target browser` serves until it is stopped, so it ends by the signal that stops it, or with `1` before it serves: a page that cannot be built, a port that is taken. `test --target browser` serves until every selected test has an outcome, then exits as `test` exits, and ends with `1` before it serves for the same reasons.

A command whose answer goes to standard output — a `wonder` query, `lint`, `format -`, `profile` — ends with `141`, and says nothing, once whatever reads the answer has gone, as `curios wonder --stage wasm | head` leaves early, the code a Curios program's own `print` ends with.

The tri-state describes a command line that parsed. One that did not — an unknown command, an unknown flag, no command at all — exits 1, with the usage message on stderr rather than a compiler report; `--help` and `--version` exit 0.

## Flags

`--budget` and `--manifest` belong to the commands that read them, so they follow the command: `curios run --budget 200000000 serve`, `curios wonder --manifest ../app/curios.toml`.

| Flag | Taken by | Effect |
| --- | --- | --- |
| `--manifest <PATH>` | every command but `new` and `profile` | use this `curios.toml` as the governing package's, instead of the nearest one |
| `--budget <UNITS>` | every command that elaborates, which is every one but `new`, `curate`, `pin`, `format` and `profile` | units of reduction work each declaration may spend while type checking — a transition costs one, a construction costs what it builds |
| `--core` | `wonder`, `format` | read standard input as Core text, as a `.crsc` file is read; refused beside any subject but `-` |
| `--color <WHEN>` | every command, on either side of it | paint what the command reports on standard error: `auto`, `always` or `never` ([What a command reports](#what-a-command-reports)) |
| `-h`/`--help` | `curios` and every command | what that command takes, with the default each flag was built with |
| `-V`/`--version` | `curios` itself | the build's version |
| `--profile <PATH>` | every command, on either side of it | write one record per span and event to `PATH`, and, for a program `run` hosts, `/tmp/perf-<pid>.map`, the map a sampling profiler names its functions by; nothing is recorded without it. [`profile`](#profile) reads the stream back |

The budget is restored at every declaration boundary, so it bounds the heaviest declaration rather than the compilation; `--help` prints the default this build carries.

## Standard input

`-` runs whatever arrives on standard input, so a heredoc is a program:

```sh
curios run - <<'EOF'
/std/print("Hello, Curios!\n")
EOF
```

It is loose in the same sense a file no unit declares is, and answers before anything looks for a manifest — so `-` means the same thing inside a package as outside one, never the package's default executable, and no `--manifest` can govern it. `compile -` builds it where `--output` says.

Standard input is asked for with `-`, and never read because a pipe is attached ([why](design/tools/an-argument-names-one-subject-and-each-command-states-what-it-accepts.md)).

It is written as a program is. `--core` says it is Core text, as a `.crsc` file's name says it of a file, and is refused beside any subject but `-`; `wonder` and `format` take it. What a text is written in is never guessed from the text. It is drained where the command admits its subject, once, so what it held is what every step after admission reads.

**Standard input resolves no file-backed modules.** `mod util;` looks in the directory its root sits in, and standard input sits in none, so it fails as an unfound module; an inline module, `mod util … end`, works.

Text written as a module — declarations with no final term — is a module of its own mounted at `/stdin`, as a file written as a module is at its stem: every command answers it where it answers a library's file — `wonder` and its queries, `--stage` at the rungs a unit has among them, `lint` and `test` — and `run`, `compile`, `wonder --stage` past a unit's rungs, `wonder --name <PATH> --footprint` and `wonder --false`, which need a program, refuse it as written as a module, as `document`, which needs a library, refuses it as a module no package declares.

Diagnostics name it `<stdin>` where they would name a file, keeping line and column:

```text
└─ [error] <stdin>:1:19
```

The program's own standard input is spent on reading the source, so a read of it reports the end of the input. A program that reads its input wants a file, or a path the shell mints for it — `curios run <(your_generator)` is an ordinary file argument, and leaves standard input alone.

## Where things go

Everything generated lands under one `.curios/`, beside the governing manifest — the umbrella's when one governs, so its members share it: what a command emits, named for the target it is built for or for documentation, beside the cabinet, `cabinet/`, which holds whole what is kept for reuse, each of its families named for what it holds:

| Entry | Holds |
| --- | --- |
| `native/<package>/<name>` | what `compile` emits for the native target |
| `browser/<package>/<name>/` | what `compile` emits for a browser: the page's directory |
| `documentation/<package>/` | what `document` emits |
| `cabinet/sources/<scheme>/<digest>` | materialized dependency sources, keyed by the hash they were accepted against |
| `cabinet/foreign/<scheme>/<digest>` | fetched foreign modules, keyed the same way |
| `cabinet/objects/<address>/` | judged units, an object per unit, a library or an executable's own declarations: its `unit` file and the one it replaced, and the packs of the blobs they name, each named by what it holds |
| `cabinet/payloads/<target>/<address>` | precompiled payloads, one file per executable and target, each target's in a directory of its own |
| `cabinet/compiler` | this machine's memo of the digest of the compiler running now, against a stamp of its binary |

One transient entry passes through the cabinet beside its families: [`pin`](#pin) stages a delivery at `cabinet/pinning.<pid>` while the digest that will name it is still being computed, then renames it into `sources/` or `foreign/`. It is removed however the command ends, so seeing one means a `pin` was interrupted, and nothing ever reads it; a command that files takes one away once it is a day old.

What a command emits is nested under the name of the package that declared it, and always stays in the project's `.curios/`. What the cabinet holds is keyed by what it is, never by which project asked, and a cleared cabinet costs the next command the work it held and nothing else. A source's hash scheme is a directory of its own.

A command that files — `run`, `compile`, `test`, `lint` and `document` — takes away from its project's own cabinet, as it ends, what the project no longer reaches: the object of a unit or an executable no manifest of the project declares, the payload of an executable or a test run it no longer has, a source tree or a foreign module no manifest pins, and any entry of `.curios/` or of the cabinet that none of the families above holds. Nothing is taken while a manifest is refused or a dependency is not yet curated. [`wonder --server`](#wonder) does the same after a check that filed ([why](../curios-verdict/README.md#a-projects-own-cabinet-keeps-what-the-project-reaches)).

Set `CURIOS_CABINET` to a directory to make it the cabinet of every project a command sees, in place of each one's own `cabinet/`, so projects share what they keep for reuse; unset, each project keeps its own. A named cabinet keeps whatever is filed into it until you clear it, since no run knows every project that files there. `.curios/` and the cabinet it names are the only directories the toolchain writes into of its own accord ([why](../curios-package/README.md#curios-goes-where-a-manifest-governs-and-its-cabinet-nowhere-it-was-not-pointed)); `--profile` writes where it is told, and the guest's map where a sampler looks for it.

## Reusing what was already built

Neither `run` nor `compile` recompiles a declared executable nothing has changed. The precompiled payload is filed in the cabinet beside the units, and an invocation whose entry file, whose entry's own modules and whose dependencies all still hold what they held is served from it and narrates nothing, where a compile shows a tree for each unit it compiled and the program's own ([What a command reports](#what-a-command-reports)). One payload serves both subcommands, so `compile` after `run` only writes the executable, and `run` after `compile` compiles nothing. `test` and `document` file what they compile the same way, so an invocation of either whose sources are unchanged recompiles nothing.

An edit anywhere the program was built from is a miss, and so is a damaged or half-written cabinet entry; the invocation that misses recompiles and refiles, and the one after it is fast again.

A question — [`wonder`](#wonder), and [`lint`](#lint) with it — reads the cabinet and files the units it compiled from the text on disk, the units a build would have filed and no payload, so a build after a question compiles only what the question did not, and the question after it compiles nothing. A unit it finds filed from an earlier text of the same sources it compiles over that unit as a baseline, as a build does, keeping every declaration that read nothing the edit changed; what it compiles that way it files as a build would have, so the question after a saved edit is the last to compile it. It files no unit compiled from text an editor holds unsaved, nor any unit after one, so a server asking on every keystroke files nothing.

A cabinet that cannot be written costs the reuse and never the answer. The command goes on: each unit's `store` node closes `refused`, and why hangs beneath the first of them, once however many units went past it. [`wonder --server`](#wonder) says nothing of it.

A loose `.crs` file consults and writes nothing: it has no project, hence no cabinet. Standard input has none either, so its test program is compiled every time.

Payloads are native code for the machine that built them, so an entry is found only by an engine that can run it; two machines share one only when their engines agree. Nothing has to be cleaned up by hand as sources change: each executable occupies one payload, overwritten in place.
