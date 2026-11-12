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

The governing package is the one whose `curios.toml` is nearest to the subject — a file's own directory, the working directory for a name or no argument — found there or in the first directory above it that holds one. A command run anywhere inside a package — in the directory of one of its modules, say — means that package, and a build names the manifest on its `Processing` line whenever it is not in the working directory: `Processing serve (../curios.toml)`. The walk stops at the first manifest it finds, so a package nested in another's directory is its own, and an umbrella found first is refused, since it declares no package to compile.

Above the package, only the umbrella is looked for, and only one that enumerates you governs you. When it does, the umbrella's directory is the governing root: that is where the store goes, and it is shared with your sibling members.

`--manifest <PATH>` overrides exactly which manifest is the package's. Which umbrella governs is still enumeration's answer, because a manifest cannot declare itself governed.

## What a package is made of

Two files are found by presence rather than declared, since they are what a package is ([`curios-package`'s first law](../curios-package/README.md#the-four-laws)):

| File | Is |
| --- | --- |
| `lib.crs` | the package's library, mounted at its declared `name` |
| `exe.crs` | the package's own executable, run by that same name |

So the smallest complete package is two lines of manifest, its name and the oldest compiler it accepts:

```toml
name = "hello"
compiler = "0.16.9"
```

Neither stem is part of a name ([Modules](language.md#modules)), so a package's own name means one thing: where its library is mounted. Each is its unit's root, so each loads its modules from the manifest's directory, as every root loads from the directory it sits in ([Modules](language.md#modules)): `pub mod parse;` in `lib.crs` reads `parse.crs`, and `mod util;` in `exe.crs` reads `util.crs`.

One name is reserved: a package named `std` is the standard library. Compiled first, it takes the archived `/std`'s place — compiled over it, with everything the edit did not reach reused — and everything after it is compiled against it; anywhere else in a compilation it collides with the archived root as any other claim on `/std` does. It is how the standard library is worked on, not a way to swap one under a dependency.

Everything else is declared, in the rows [the manifest](#the-manifest) states. A declared row always wins over the found file, so `[[executables]] name = "hello"` with a `path` overrides the `exe.crs` convention for the package's own program; and with more than one executable and no `default`, a bare `curios run` refuses rather than guessing.

**A file is one unit's.** The library and every executable beside the manifest load their modules from one directory, and a row may point anywhere below it, so a file two units' `mod` chains reach is refused before anything is compiled, naming both: a module both declare would be compiled twice as two unrelated modules, and a program's file declared as a module would be both. A module the library and a program both want lives in the library, which every executable of the package reaches by the package's name:

```
`parse.crs` is a module of the library and of the executable "tool": a file is one unit's, so declare it in the library alone, and reach it from the other through `/hello/parse`
```

## Subjects

Every command that takes a subject takes it one of four ways, so what a bare invocation means never depends on which one you asked.

| Argument | Means |
| --- | --- |
| *(none)* | the governing package, narrowed to what the command needs |
| an identifier | the executable declared under that name |
| anything ending in `.crs`, or holding a path separator | that file, placed by what declares it |
| `-` | the program on [standard input](#standard-input), loose |

The dispatch is lexical and never probes the disk: an executable's name is a single identifier, so it can hold neither `.crs` nor a path separator nor be `-`, and the spaces cannot overlap. `curios run scratch.crs` therefore means the file even when the package declares an executable called `scratch`.

**A file is placed the same way by every command:** it is an executable's when it is that executable's entry or a module the entry's `mod` lines reach, a library's when the `mod` lines of `lib.crs` reach it, and loose otherwise — compiled against the standard library alone. A file written as [a module](language.md#declarations-and-modules), items with no final term, has no program in it. The project is decided from the file's own location, not the working directory.

`run` and `compile` are the only commands that need a program's **own** file: a module of one is refused, naming the executable to run instead, and a library is refused as the non-program it is. Everywhere else a module reaches its unit as readily as the entry does, which is what lets a question or a lint be asked about the file in front of you.

A loose file brings no project with it — no dependencies, not even the library of the package it sits in ([An argument names one subject](design/tools/an-argument-names-one-subject-and-each-command-states-what-it-accepts.md)). When a scratch program does want the library, one `[[executables]]` line gives it one. For the same reason nothing answers a `foreign` a loose program declares for itself: a module is named by a `[[foreign]]` row, and a loose program has no manifest to write one in, which `run`, `test` and `compile` say where the program reaches such a declaration. A file a package's directory holds and no unit declares is taken the same way: a `mod` line declares one written as a module, and an `[[executables]]` row one written as a program.

## The surface

Ten commands, and `wonder`'s six queries — eleven commands where the compiler was built with the `profile` feature, which carries [`profile`](#profile) as it carries `--profile`. What each takes and leaves:

| Command | Subject | Its own arguments | Writes | Store |
| --- | --- | --- | --- | --- |
| [`run`](#run) | a program | `--target <TARGET>`, `--port <PORT>`, trailing `ARGS…` for the program | nothing | reads, files what it built |
| [`compile`](#compile) | a program | `--target <TARGET>`, `-o`/`--output <PATH>` | a native executable, or a page's directory | reads, files what it built |
| [`document`](#document) | a library | `--std <DIR>`, `-o`/`--output <DIR>` | pages | reads, files what it built; none under `--std` |
| [`test`](#test) | anything | `--target <TARGET>`, `--port <PORT>`, `--filter <PREFIX>` | nothing | reads, files what it built |
| [`curate`](#curate) | the governing package | — | materialized sources | none |
| [`pin`](#pin) | the governing package's manifest | `foreign`/`dependency <NAME>`, `--target <TARGET>` for `foreign`, `--path`, `--url`, `--rev` for `dependency`, `--refresh`, `--check` | one manifest row | files what it delivered |
| [`new`](#new) | — | `<DIR>` | a package | none |
| [`lint`](#lint) | anything | — | nothing | reads, files the units it compiled from disk |
| [`format`](#format) | files, any number | `--check`, `--width <COLUMNS>` | the files, rewritten | none |
| [`wonder`](#wonder) | anything | — | nothing | reads, files the units it compiled from disk |
| [`wonder --tests`](#wonder) | anything | — | nothing | reads, files the units it compiled from disk |
| [`wonder --cost`](#wonder) | a program | — | nothing | reads, files the units it compiled from disk |
| [`wonder --stage`](#wonder) | a program, or at `text`, `core` and `core-elab` a library's file | `<STAGE>`, the option's value | nothing | reads, files the units it compiled from disk |
| [`wonder --at`](#wonder) | a file of a program or of a library | `<LINE>:<COLUMN>`, the option's value | nothing | reads, files the units it compiled from disk |
| [`wonder --declarations`](#wonder) | a file of a program or of a library | — | nothing | reads, files the units it compiled from disk |
| [`wonder --server`](#wonder) | — | — | nothing | reads, files the units it compiled from disk |
| [`profile`](#profile) | — | `<PATH>` | nothing | none |

`--manifest` is taken by every command but `new` and `profile`. `--budget` is taken by every command that elaborates, which is every one but `new`, `curate`, `pin`, `format` and `profile`. Both are stated under [Flags](#flags).

## What a command reports

A command narrates on standard error, in one form: a subject alone on a line, and beneath it a node for each thing done for it, the node's label written before its work and what the work came to after it.

```text
/geometry
├─ check; 1.6s
╰─ store; 0.1s
hello
├─ check; 0.1s
├─ build; 0.2s
╰─ emit; 0.4s
```

A subject is a unit by the prefix it mounts, an executable by its name, a file as it was written, or `<stdin>`, and the manifest that governs it is named beside it, `hello (../curios.toml)`, where the invocation does not stand in the manifest's directory. A unit is checked, `check`, and filed where a store keeps it, `store`. A program is checked, `check`, its own declarations elaborated, certified, erased and linked with the units it stands on; built, `build`, lowered and optimized; and emitted, `emit`, written as WebAssembly, optimized by Binaryen and, natively, compiled to machine code. What the store already held is not narrated: a subject nothing was done for says nothing, save one named beside its manifest, whose line says which package the invocation reached.

A node that finished closes with what it took, `; 0.3s`. One that did not closes with how it ended and what it took — `; refused in 0.3s` where the compiler refuses the program, `; 1 goal in 0.3s` where written goals alone stopped it, `; failed in 0.3s` where the machine did — and what stopped it follows the tree, a blank line between. One whose point is where its work came to stand closes with that, spelled from where the invocation stands: `write; .curios/native/hello/hello`. `├─` is written where more nodes are planned and `╰─` at the last, each before its node's work, so a tree a refusal cuts short ends on `├─`.

Nothing is rewritten or erased, and the bytes are the same in a pipe, a log file and a CI transcript as on a terminal, colour apart.

What the compiler says of a program is a report, and every report has one form, a block:

```text
type mismatch
│  inferred: Nat
│  expected: Str
│
│  3 │ let label: Str = double(21);
│    │                  ^^^^^^^^^^
│
╰─ error; demo.crs:3:18
```

The first line is a tag: a few words that name the kind of thing reported, the same wherever it is met and whichever stage reports it, with nothing of the program in them. What is reported of this program follows on a trunk, a fact a line and in one order: what stands here, the terms at issue each behind a label, the rule it is held to, and the way out, said as what to write. Then comes the program's own text, an empty line of the trunk above and below it, with marks under what the report is about. The last line says what the report is and where it stands: `error` for what a rule refuses, `goal` for a [written goal](language.md#written-goals), `lint` for a [lint](#lint), then the file, the line and the column, the file spelled from where the invocation stands, as one word a terminal opens. A report about a second place shows that place first, closed by what stands there, so the report's own place is always its last line; and every pane of one report shares a gutter, so the code of each begins in one column:

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
╰─ error; demo.crs:9:5
```

A goal's first line is its answer in place of a tag: what the compiler knows is wanted where it stands, `` `Nat` expected ``, or what it worked out stands there, `` `Nat` inferred ``; beneath it are the names in scope, each with its type, and each term the compiler found that fits, `? ≈ seed`. A text that is no form of the grammar is a `syntax error`, whose facts are what was `expected:` there and what was `obtained:` in its place, or the rule the text breaks and what to write. A build writes its reports on standard error after the tree, a blank line between; `wonder` and `lint` write them on standard output, as their answer.

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

Everything after the subject belongs to the program and reaches it through `/std/Proc/args`, including `--help`, `--budget`, `--manifest` and, in a profiling build, `--profile`. Compiler flags go before the subject. A leading `--` omits the subject and passes the words after it to the governing package's sole or `default` executable. One `--` immediately after an explicit subject is an optional separator too: `curios run serve -- --help` passes `--help`. Every later word is collected verbatim, hyphens and further separators included; `curios run -- -- mock` passes `--` and `mock`. Program arguments retain their OS bytes, including text that is not UTF-8; an explicit subject must be UTF-8. A file whose name starts with a hyphen can be named with its directory, such as `./-sample.crs`.

A program the runtime stops rather than one that exits prints why on stderr and exits 1: `panicked:` and one sentence naming the rule that refused it — a `Nat` or `Int` argument to a `foreign` function past what the wire carries, a value past what the runtime can hold, a read past the end of a packed value or list, a `Flt` decoded from the wrong number of bytes, a recursive value read while its own initializer was running — followed by the wasm frames where the build kept their names. A call the runtime refuses where it crosses to what answers it is no panic: `execution failed:` and a sentence naming the declaration and what was wrong, as when a module answers `256` to a `foreign` declared `-> Byte`, or a host cannot answer a call that has no failure to report.

A running program's own exit code passes through untouched. A program that closes with a description that may fail, and fails, writes the failure on stderr and exits 1, and one that closes with a test that did not hold reports it and exits 1 ([`documentation/language.md`](language.md#declarations-and-modules)). A `print` or `print_err` that cannot write ends the program with 141 where its reader has gone and 1 otherwise ([`curios-prelude`](../curios-prelude/README.md#a-loose-printer-that-cannot-write-ends-the-program)).

### Serving a page

`--target browser` builds the program's [page](#the-page) as `compile --target browser` does, reads every file of it once, and serves it from memory on `127.0.0.1` — at port 8000 unless `--port <PORT>` names another, and at a free one under `--port 0` — until the command is stopped. It closes the program's tree with where a browser opens it — `╰─ serve; http://127.0.0.1:8000/?arg=--help` — and opens no browser.

The page stands at the root: `/` and `/index.html` are its document, every other file stands at its page path, and the program's arguments are the address's query, each an `arg=` percent-encoded, so the printed address runs it with the words after the subject. Nothing reloads: a changed program is served by running the command again, and every reply says `Cache-Control: no-store`, so the browser asks for the new page rather than keeping the old.

Only `GET` is answered, and a request naming a host other than `127.0.0.1` or `localhost` at the port is refused, so a page another site loads cannot reach this one through a name that resolves to the machine. A port another process holds is refused, naming the flag that picks another, and `--port` beside the native target is refused.

## `compile`

```sh
curios compile                   # under the store, beside the governing manifest
curios compile -o hello          # somewhere else
curios compile scratch.crs -o s  # a loose program, which has nowhere else to go
curios compile --target browser  # a page, under the store
```

`--target` says what the program is built for: `native`, the default, writes an executable, and `browser` a directory holding a page. The command closes the program's tree with `write` and where the result stands, a page's directory with a trailing separator, whether or not anything had to be compiled. A declared executable's build is written under the store beside the governing manifest, under the target, nested under the package and named after the executable it built; `-o`/`--output <PATH>` writes it somewhere else instead. A program no package declares — a loose file, or standard input — is filed nowhere, so `--output` is the only place it can go, and without one the command is refused naming the flag that would build it.

**The result is self-contained, including its `foreign` implementations.** Every [`[[foreign]]`](#the-manifest) module that answers a declaration the program reaches is compiled into the executable beside the program itself, so it runs where there is no manifest to read a row from, no `.wasm` beside it to hash and no compiler to turn one into a module. Nothing is read from the store or the package at run time, and the executable is the only file that has to travel. It is linked before it is written, as `run` links before it runs: a declaration the program reaches that no module and no operation of the host answers, a declaration two rows claim and an export of another type than its declaration crosses as are each refused, so no executable is written that would refuse to start.

### The page

`--target browser` writes the module Binaryen produced, with no Cranelift step after it, beside the page that runs it, all in one directory: under the store at `browser/<package>/<name>/`, emptied first so it holds nothing an earlier build left, or the directory `--output` names, which is created where it is missing and keeps whatever else it holds, a file of the page replacing the one of its name. The directory is the page whole, and copying it anywhere a static server serves deploys it.

| File | Is |
| --- | --- |
| `index.html` | the page: the import map, the loader, and one `<pre id="output">` the program's output is written into while it stands in the document — a gateway may remove it |
| `index.js` | the loader: the browser's host, what it is configured with, the program's rows, and an import of every gateway |
| `index.wasm` | the program |
| everything else | what the [`[[foreign]]`](#the-manifest) rows the program reaches put in the page |

The page's root is the program's package's root. A gateway the package carries stands at its `path` from `curios.toml`, a fetched one at `<row>.js`, a `[foreign.browser.static]` entry's files at its page path, and everything a dependency's rows put in the page under the dependency's name. Only a row the program reaches puts anything in the page.

**A gateway names its package's files relative to its own address**, since its package's files keep their places beside each other in every page that holds them, and the page's root is its package's root only where its package is the program.

An `import` the gateway writes resolves that way already; a URL it builds itself — for `fetch`, an image's `src`, a stylesheet — resolves against the document unless it is resolved against `import.meta.url`: from `web/dom.js`, `new URL("../art/pixel.png", import.meta.url)` reaches the entry `art`'s `pixel.png` in the package's own page and in every page that depends on it, where a bare `art/pixel.png` reaches it only in the first.

The path is the page's: a `static` entry's files stand at its key, whatever directory of the package the entry reads them from, and `run --target browser` serves the same layout the written directory holds. A page path two files claim is refused naming both, and nothing written is root-absolute, so the page works under any path.

The program and every JavaScript file the page loads are held to the build: the loader fetches `index.wasm` against its integrity, `index.html` states the loader's, and the import map states every other `.js` and `.mjs` file's, each the `sha256-` form of its SHA-256, so one changed after the build fails to load. A browser that predates import-map integrity loads the gateways unchecked.

The program runs on the page's main thread, its output written to the `<pre>` and, line by line, to the console, where a program the runtime stops says why in both. Its arguments are the page's name — the executable's, a loose file's stem, or `stdin` — then each `arg=` of the URL's query in order, percent-decoded to its bytes, `+` included as itself: `index.html?arg=one&arg=two%20words`. `arg` is the one key the page claims in its query; every other key is a gateway's to read. Before the program runs, a gateway's export that is no function is refused, naming the row.

A framed page — one whose window has a parent other than itself — posts to that parent, at its own origin alone: `{ type: "started" }` before the program runs, and `{ type: "outcome", outcome }` after, `outcome` being `{ stdout, stderr, exitCode, trap, held }` with each stream whole and `held` the `Bool` a test program's entry answered, or `null` for a program's unit, an exit or a trap. It is how the runner of [`test --target browser`](#serving-the-tests) reads each test, and any embedder's to use.

Each export `[foreign.answers]` names is a function answering the declaration it is named for, held to the row where the page loads it: an export that is no function is refused before the program runs, naming the row. It is handed each operand as the JavaScript value its wire type names, a copy it owns, then an `AbortSignal` that fires where the call is cancelled, and it answers the row's results in the same values — nothing for a row with none, the value for a row with one, and an object keyed by exactly the results' labels for a row with several — at once, or as a promise of them, which the call waits on while the rest of the program runs. A row declared `-> Never` is answered by an export that ends the program by throwing. An answer outside the row, or a promise that rejects, stops the program naming the row; a value of another JavaScript type is refused rather than converted, so a `Number` where a `Nat` is declared stops the program instead of crossing wrong past 2⁵³.

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

**Each side is tested with its own tools, and the seam by [`test --target browser`](#serving-the-tests).** A row that states both tables links its `[foreign.native]` module wherever the program runs natively, so a `.wasm` standing in for a gateway is what `curios test` and `curios run` link for code that reaches the row, and the Curios side is tested with the JavaScript mocked. A gateway's exports are plain functions held to the values above, so a JavaScript test calls them with what a page would hand them and checks what they answer, and the JavaScript side is tested with the program mocked. The gateway answering the program's rows is tested by `test --target browser`, which serves the same tests to a browser. A row with only `[foreign.browser]` is refused natively, naming the table a stand-in goes in and the command that runs its tests against the gateway.

The page runs in Chrome 119, Firefox 121 and Safari 18.2 and every later version, the first to ship WebAssembly's garbage collection and tail calls together. A server must give `.js` and `.mjs` a JavaScript type, which a module script requires; `index.wasm` is compiled from its bytes, so its type does not matter. Curios runs no build of what a gateway imports: TypeScript, a bundler or a minifier runs before `compile`, into a directory a `static` entry serves.

## `document`

```sh
curios document              # .curios/documentation/<name>/
curios document -o site      # somewhere else
curios document --std site   # the standard library
```

`curios document` writes a library's interface as pages, read off the compilation that builds it: what each module exports, each declaration's head printed as written with every name in it linked to where it was declared, and the `---` documentation comments attached to each. The subject names the library — none for the governing package's, or `lib.crs` or a module its `mod` lines reach — since a library is the one thing with an interface; a program, a loose file or standard input is refused.

`--std <DIR>` documents the standard library the compiler was built with instead, writing its pages under `DIR`: the record is read off an image the compiler was built with, so nothing is compiled and no sources are read. It takes no subject, and since the standard library has no store to file pages under, the flag itself names where they go and `--output` does not apply.

The pages are the library's consumers' view: a private declaration or module is absent rather than hidden, a type whose representation is private shows no constructors and is marked opaque, a concept whose representation is private is marked sealed, a test never appears, and a `pub use` is a link to the declaration it re-exports — unless that declaration's own module is private and so has no page, in which case the declaration is documented on the re-exporting module's page, the facade being the only way it reaches a consumer.

A reference into a dependency or the standard library renders as its qualified name in plain text: the pages document one library and link nothing outside it.

The landing page is the root module's page: it opens with the manifest's `description`, lists the modules, then the root's own declarations; every other module's page opens with the `---` block above the `mod` that declares it, whose first paragraph is also the module's gloss on its parent's page. In prose, a pair of backticks encloses a code span.

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

Each unit is compiled as its own test program — the same compilation `run` performs, with the final term replaced by a synthesized scheduler over that unit's selected tests — and every selected test runs in an instantiation of its own, so one test's effects, traps and exits never reach another. The unit itself is compiled with every test it declares whatever the filter, and is the one `wonder` and `lint` read; the filter narrows the program, so only the tests it selects are built past the front end, and a unit it selects none from builds no program. A unit holds its tests only here and in what `wonder` and `lint` ask of it: `run`, `compile` and a unit compiled for the units after it leave them out, so a unit's tests cost its builds nothing.

The report is on standard error, on each subject's own tree, after the nodes that built its test program ([What a command reports](#what-a-command-reports)). A unit and its test program are each a subject, so a library is named twice: checked and filed, then built into the program that runs its tests.

```text
/demo
├─ check; 0.3s
╰─ store; 0.0s
/demo
├─ check; 0.0s
├─ build; 0.2s
├─ emit; 0.4s
├─ /demo/shapes/a_square_has_four_sides; passed in 0.0s
├─ /demo/two_is_three
the condition was false
│  ╰─ failed in 0.0s
│       Test/assert(2 == 3)
├─ /demo/render/draws_a_square
■■■■
│  ╰─ passed in 0.1s
╰─ 2 passed, 1 failed; 0.1s
2 passed, 1 failed; 0.1s
```

Each selected test is a node, opened by its path before the test runs, so a test that never ends is the one whose line stands open, and closed by how it ended and what it took: `passed`, `failed`, `trapped`, `exited N`. A test that exits on its own has answered nothing, so it is `exited N` whatever `N`, `0` included. The tests run module by module, the modules in the order each declares its first test.

What a test writes is never held: it reaches the stream the test wrote it on, standard output or standard error, as it is written. Its first byte ends the line its node opened on, and the node closes beneath what was written, on a curve off the trunk, whichever stream that was and wherever either stream goes. A failing test says what went wrong itself, on its standard error, so its verdict always stands beneath; beneath the verdict the runner adds what only it knows, the test's body as written and, for a trap, the trap's text.

A subject's tally closes its tree and the run's is the last line, each with what its tests took together where any ran: `N passed, N failed` always, since that pair is what the exit code turns on, then `trapped`, `exited` and `skipped`, the tests the filter left out, when they happened. A subject that declares no test, or that the filter selects nothing from, closes with its tally at once.

A claim about every instance is [a `let`, not a test](language.md#test-declarations): a broken one fails the build, and `curios test` never sees it.

The test *verdicts* are never cached — every invocation runs every selected test, however much of the compiling was reused.

### Serving the tests

`--target browser` builds every subject's test program as its [page](#the-page) instead, as `compile --target browser` would build it, and serves on `127.0.0.1` — at port 8000 unless `--port <PORT>` names another, and at a free one under `--port 0` — until every selected test has a verdict. It prints the address on a line under no subject, `serve; http://127.0.0.1:8000/`, since what is served is every subject's, and opens no browser: whatever opens the address runs the tests — a person, a headless browser a script starts against the printed port, a device reaching the machine.

A script runs them with no window by holding a headless browser open until the command exits — `timeout 120 google-chrome --headless=new --user-data-dir=<a fresh directory> http://127.0.0.1:<port>/` — rather than by a mode that dumps the page at its load event, which ends the run before its first outcome.

Everything is built before anything is served, so a `--filter` matching nothing and a test reaching a row with no `[foreign.browser]` table are refused as [`compile`](#compile) refuses them, before the address is printed.

The root is the run: a runner that opens each test in an iframe of its own, one at a time in the order native runs them, each a fresh realm with fresh instances of every gateway module — the browser's form of native's one instantiation per test. `index.html` beside it is the subject in hand's page, the test's index as the program's argument, and the page switches to the next subject with the last outcome of the one before, which is why no subject's page has a name of its own.

The runner drives the run over a WebSocket it opens at `/`, which the command takes from one page at a time: it is sent the test in hand, the page posts its outcome to the runner when its program ends ([The page](#the-page)), and the runner sends it back over the socket, answered with what the command wrote since its last reply and the next test to open; the first reply carries every line of the run written before it.

A socket asked for while another drives the run is refused, so a second tab runs nothing and says so; one that closes, a reload's or a closed tab's, lets the run go, and the next page to open resumes it at the test in hand, which runs again. A socket is refused too where its `Origin` names another site, so only the runner a run serves drives it, and a page whose loader never starts — a gateway that fails to import — is reported trapped.

Past the address each served subject is named again and its tests stand on a tree of their own, written as native writes them, each whole as its outcome arrives: its node, what its page kept of the two streams — relayed on the command's own, standard output first — and how it ended, timed from the test's being handed to a page. The runner page shows every line the command wrote from its first, a reloaded runner all of them again; a subject the filter selects nothing from is never served, and closes with its tally as it is built, on both. Once the last verdict is in, the command stops serving and exits as `test` exits; a rerun is the command run again.

## `curate`

```sh
curios curate
```

`curate` materializes what the manifests reference. It and [`pin`](#pin), whose `--url` and `--refresh` fetch through the same delivery, are the only commands that fetch — the compiler itself never does, and [`run --target browser`](#serving-a-page) only listens, on loopback. A delivered tree is accepted against its `hash` whatever transport produced it, so a mirror is no weaker than the origin, and a delivery that fails its hash is refused whoever fetched it.

It reaches the network through programs already on the machine ([why](../curios-package/README.md#a-pin-is-derived-by-one-command-and-checked-everywhere-else)). A package tree comes through `git`, and a `[[foreign]]` module through `curl` or `wget`, whichever is on `PATH`. A project with no `[[foreign]]` row that names a `url` needs neither of those two; one that has one and has neither installed is refused naming the module and both programs.

Each fetch is a subject of its own — a package by the prefix it mounts, a module by its row's name — whose one node stands open while it arrives, and the last line counts what was brought in, `0 fetched` where the store already held everything:

```text
/json
╰─ fetch; 1.4s
sha2
╰─ fetch; 0.3s
2 fetched
```

A fetch whose transport fails closes as `fetch; failed in 2.0s`, and one whose delivery is not what its row pins as `fetch; refused in 1.4s`; why follows the tree, and nothing after it is fetched.

## `pin`

```sh
curios pin foreign sha2 --target native --path foreign/libsha2.wasm   # a module this package carries
curios pin foreign sha2 --target native --url https://…/libsha2.wasm  # one fetched and filed in the store
curios pin foreign dom --target browser --path web/dom.js             # a gateway a browser links
curios pin foreign sha2 --target native --refresh                     # deliver again, pin what arrives
curios pin dependency json --url https://…/json --rev abc123
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

Exactly one delivery form is required. `--path` and `--url` say where the row points; `--rev` alone keeps the repository and moves the revision; `--refresh` keeps everything and delivers again. `--url` on a dependency requires `--rev`, since a repository is pinned to a revision and nothing else.

`member` and `catalog` rows are not written here: one names a member of the governing umbrella and the other a row of its catalog, so neither has a location to deliver from, and each is one word written in the manifest.

| Flag | Is |
| --- | --- |
| `--path <PATH>` | a module the package carries, spelled plainly and relative as an executable's path is; or a dependency in a separate project on disk, such as `../json`. A package of the same project is a `member` of its umbrella, which this command does not write |
| `--url <URL>` | a module fetched once and filed in the shared store, or — with `--rev` — a repository |
| `--rev <REV>` | a dependency's revision to pin. Alone on an existing row, it re-pins that row's own URL to a new revision, which is how a dependency is moved forward without restating where it comes from |
| `--refresh` | deliver again whatever the row already names and pin what arrives. The only spelling permitted to overwrite a hash that was not asked to move |
| `--check` | write nothing and exit nonzero if the manifest would change. The delivery still happens — what a row *should* say cannot be known without it — and only the write is withheld |

What was fetched is filed in the store under the digest it turns out to have, so the `curate` that follows finds it there.

The row is the subject, by its name. A module is described before its pin is written: what arrived is a node of its own, `deliver`, open while a fetch arrives and closed by its size and whether it is a WebAssembly module, with **what it exports** beneath, which is what a `[foreign.answers]` entry may name as the export that answers a declaration. The `pin` node closes the tree with what became of the row — `recorded` where the manifest was written, `unchanged` where the row already said so, `would change` under `--check` — and each key it set beneath, beside what the key said before where it said anything:

```text
sha2
├─ deliver; 12,484 bytes, a WebAssembly module
│    exports: alloc, memory, SHA256_Digest
│    nothing verified this download — the hash below is derived from it, not checked against it
╰─ pin; recorded
     url: https://example.org/libsha2.wasm
     hash: f1:93a44bbb96c751218e4c00d479e4c14358122a389acca16205b1e4d0dc5f9476
```

The export list is read by compiling the module, so a module this toolchain cannot link reports why instead. The line about verification appears only for a fetch, since a carried module arrived inside the tree its consumer pinned. A dependency has no `deliver` node, since nothing describes a tree, and its `pin` node stands open while a repository arrives. A node open when the pin stops closes with how — `failed` where the transport brought nothing, `refused` where a rule refused what it brought — and a pin refused before any delivery began reports why and nothing else.

`--target` is required on `pin foreign`, and names the table of the row that is written, `[foreign.native]` or `[foreign.browser]`, inline where its author wrote it inline; the other table is left as it was. A browser delivery is described as the JavaScript gateway it is, whose exports are held to its rows where the page loads it, so nothing is read of them here. The manifest is edited in place, keeping its comments and order, then read back and compared key by key with what went in, so no row the command was not asked about changes. Pinning a row to what it already says writes nothing and reports that it did.

A pin reads only the row it writes, so it repairs a row the manifest refuses: `--refresh` gives a row the `hash` it lacks, and `--path` or `--url` gives a row written by hand the table it names no file in. What is held to the manifest's rules is the manifest as the pin would leave it, read before it is written: one that would still be refused, for this row or another, is refused naming why, and nothing is written.

## `new`

```sh
curios new <DIR>
```

The directory's name is the package's, and it is checked before anything is written — so a name the manifest could not hold fails with an empty disk rather than half a package.

It writes the manifest, `lib.crs`, `exe.crs` and a `.gitignore` for `.curios/`, and no flag asks for fewer ([why](../curios-package/README.md#new-starts-a-package-whole)). Deleting either file makes the package a library alone or a program alone.

The directory is the subject, each file written a node, and the last node the command that runs what was written:

```text
hello/
├─ write; curios.toml
├─ write; lib.crs
├─ write; exe.crs
├─ write; .gitignore
╰─ try; cd hello && curios run
```

## `lint`

```sh
curios lint              # the governing package entire: its library, then every executable, then its dependencies
curios lint app.crs      # one file, placed in its unit
curios lint -            # the program on standard input
```

A lint is an exact finding the compilation already has and nothing stops on: `run`, `compile` and `test` never mention one, `wonder` and the language server report them beside the errors and goals, and `lint` is where they turn into an exit code. There are four, every one always on, and there is nothing to configure — what keeps a name is spelled in the program, and each message says how.

| Lint | Reports | Kept by |
| --- | --- | --- |
| `unused-import` | a `use` selector, or a glob, that no reference resolved through; a `pub use` is a re-export and its own use | deleting it |
| `unused-binder` | a parameter, `let` binder, pattern binder or motive label nothing references, implicit or shadowed included; a hypothesis a proof the elaborator wrote from the facts in scope reads is referenced, though no written name reaches it, and a declaration holding a written goal reports none, since its binders are the goal's scope | writing `_` in its name's place |
| `unused-declaration` | a non-`pub` `let`, `foreign`, `induct`, `struct` or `concept` unreachable from the unit's roots — its exported surface, its tests, its witnesses, the declarations a `_` keeps unused and its program's tail; a private `mod` none of whose declarations is reached is reported once, at the `mod` | deleting it, `pub`, or naming it `_x` |
| `unused-dependency` | a `[dependencies]` row whose package no reference in the library or any executable resolved into; decided over the package, so reported only by the package-entire form | deleting the row |

Output is what `wonder` prints — each diagnostic, goal and lint rendered as `run` reports it, a blank line between — and the exit is the tri-state: 1 when a lint or an error was reported, 2 when only goals were, 0 when nothing was. A program that does not lower reports its error alone, since the lints are read off the lowering; one that lowers and is then refused reports its lints beside the refusal. Each unit is linted as `curios test` compiles it, its tests included, so a declaration only a test reads is reached.

Standard error narrates what had to be compiled, as [`wonder`](#wonder) does, and its last line counts what was found, the lints always and the errors and goals beside them where there were any — `2 lints`, `0 lints`, `1 lint, 1 error`, `0 lints, 1 goal`:

```text
/demo
├─ check; 0.3s
╰─ store; 0.0s
demo
╰─ check; 0.1s

2 lints
```

## `format`

```sh
curios format                   # every file the governing package declares, rewritten in place
curios format serve util.crs    # an executable's files, and one file
curios format proof.crsc        # a file of Core text
curios format --check           # write nothing, exit nonzero if anything would change
curios format - < draft.crs     # standard input, written canonically to standard output
curios format --width 64 -      # standard input, within 64 columns
```

Each subject names files, and what several subjects name is rewritten once: no subject is every file the governing package declares — its library's header and the modules its `mod` lines reach, then each executable's entry and the modules the entry reaches — an executable's name is its own, a file is itself whatever declares it, and `-` is standard input, written back to standard output. A file nothing declares is formatted only when it is named, and a `.crsc` is formatted as the Core text it holds, read and not checked, so a file its reader refuses is formatted all the same.

There is one canonical style, and its one setting is the width a line is written within: 50 through 100 columns, counted in characters, and 100 where nothing states another. A file takes its width from one place. Where a package governs it — the package whose manifest is nearest above the file, or the one `--manifest` names — that is the package's [`[format]`](#the-manifest) table, or its umbrella's, and a `--width` given for such a file is refused before anything is written. Standard input and a file no package governs take `--width`. So the command line, `--check` and an editor's server format a file one way, and `curios format --width 80 - < file.crs` shows a package's file at another width without rewriting it. The rule and what it rejected are [A file is formatted at the width one place states](design/tools/a-file-is-formatted-at-the-width-one-place-states.md)'s.

The canonical form writes no byte-order mark, and writes a character that prints as nothing or as a blank inside a literal by its escape, `\u{200D}`, so that a reader sees it.

Formatting is verified before anything is written — the output must reparse to exactly the same program or Core text, with every comment preserved — so a formatter defect refuses rather than corrupts.

A file that changes is a subject, spelled from where the invocation stands, with one node saying what became of it: `rewritten`, or `would change` under `--check`, which writes nothing and exits 1 where anything would. A file already canonical says nothing, and the last line counts both:

```text
lib.crs
╰─ format; rewritten
shapes.crs
╰─ format; rewritten
2 rewritten, 5 unchanged
```

A file that cannot be formatted closes as `format; refused`, why following, and nothing after it is formatted. Standard input formatted onto standard output is an answer, of which nothing is said; under `--check` it is a subject as a file is, `<stdin>`.

## `wonder`

`curios wonder [QUERY] [SUBJECT]` answers a question from the compilation that would build the subject, and executes nothing: with no query, the check. The subject is the one argument, read as every command reads its own, and a query is an option, one at most, so no word is ever both: `curios wonder tests` checks the executable named `tests`. The answer goes to stdout and nothing else does, so it can be redirected; standard error narrates the units the question had to compile, each checked and stored as a build's are ([What a command reports](#what-a-command-reports)), and, where the question is the check or `--tests`, the check of a program the store did not hold, with a blank line then parting the trees from the answer. A question that reprints or reads a program — a rung, `--cost`, `--at`, `--declarations` — narrates what the program stands on and never the program, and one the store answers whole narrates nothing. Exit 0 means the question was answered, including when the answer is a list of errors, and non-zero that it could not be — no such subject, no such stage, a scope that cannot be assembled — or that the program stopped before the answer, which exits as a build would: 2 when written goals alone stopped it, 1 otherwise.

| Query | Answers |
| --- | --- |
| none | every diagnostic, goal and lint — the errors and goals rendered as `run` reports them, each lint after them as [`lint`](#lint) describes — a blank line between; nothing when the subject compiles clean. A unit is asked under its written tail, never the test program `test` synthesizes, since that tail raises nothing a test's own check did not |
| `--tests` | every test the subject declares, one path per line, in declaration order — the library's, then each executable's, for the package entire; nothing when it declares none, and nothing executes |
| `--cost` | what became of each declaration by the time the optimizer settled — `<name>`, a tab, then `survived`, `specialized <n>` or `absorbed` — one row per line, ordered by name. Nothing is judged: a row states the fate and stops |
| `--stage <STAGE>` | the program's representation at one rung of the pipeline, reprinted. A rung the compilation reached is answered even when a later stage refuses, and is written where it is reached, so nothing a later stage does unmakes it: the rendering goes to stdout, what stopped the program goes to stderr, and the exit is 0. Only a program that stops *before* the rung has not answered, and exits 2 when written goals alone stopped it and 1 otherwise |
| `--at <LINE>:<COLUMN>` | the name written at a place of the subject's file, as one row: the path its declaration is declared at, what it is declared as — `definition`, `inductive`, `constructor`, `structure`, `concept`, `method`, `witness` or `test` — and where the declaration's name is written, as `<file>:<line>:<column>`, a tab between each. Beneath the row, a blank line before each, come the declaration's head as its author wrote it and the prose above it, where it has any. Nothing where no name stands. A place is written as a diagnostic prints one, a line and a column each counted from one |
| `--declarations` | every definition the subject's file declares, one such row per line, in the order they are written: a type's constructors after it, a concept's methods after it, and a witness under its head as it is written |
| `--server` | every question the others answer, asked by an editor over the language server protocol on standard input and output: what an editor integration launches. It publishes a document's diagnostics as the document is edited, and every open document's when one is saved, read from the disk as it then stands; it formats a document, and answers where the name under a cursor is declared, what that declaration states of it — its head and prose, as a hover — and what the document declares: each from the document's last check, so none waits on a check in hand, and a declaration edited since answers once it is checked again. It takes no subject, since an editor hands it its documents, and one written beside it is refused where the line is parsed |

No subject at all is the governing package entire for the check and `--tests`, and the sole or `default` executable for `--stage` and `--cost`, which both need a program and so refuse a file written as a module. For `--at` and `--declarations` it is that executable's entry file.

`--at` and `--declarations` are asked about one file: the one the subject names, an executable's entry where it names one, and a library's header where it names none of its files. They are answered by the lowering, which is where a written name is resolved, and by no checker, so a file that does not elaborate answers as one that does, and only one that does not lower stops the question as it stops a build.

A path is one name: a place anywhere in `Io/Reader/line` answers `line`. A name the lowering does not resolve answers nothing: a pattern's constructor, a field's label, a projection, a method reached through a witness, the bare head of a structure literal or of a structure pattern, a name on a `use` line, and a binder.

A declaration of the library the compiler was built with is placed where its source file still holds the text it was built from, and its row ends at what it is declared as where no file does, which is every such row under an installed compiler; its head and prose are read off the text the compiler holds of it, and are answered either way.

`--stage` reprints a library too, selected through one of its files, at the three rungs a unit has: `text` is the parse of the file asked through, `core` the library's lowered declarations, and `core-elab` its elaborated ones. From `anf` on a program's entry roots what is printed, and a library is refused as it is by any command that takes a program.

A file a package holds that no `mod` reaches is in no unit, so it is checked on its own — as a program when a final term follows its items, and as a module of its own otherwise, as a file no manifest above it claims is. That holds for the standard library's own modules: `/std` is a package like any other, and [the package named `std`](#what-a-package-is-made-of) stands where the archived unit stood rather than beside it, so a question about `curios-prelude/std/List.crs` is answered against the prelude it is part of and costs the closure of the edit rather than the library.

`--cost` says whether an optimization fired. A declaration is `absorbed` when nothing in the compiled program bears its name — inlined into its callers, or pruned once something else was — so it costs nothing of its own, and `specialized <n>` when a pass cloned it into `n` functions. The rows are read off the program that ships, and ordered by name, so a committed report diffs.

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
| `wasm-optm` | the module after Binaryen optimization, rendered by Binaryen's own text writer |

## `profile`

```sh
curios profile .curios/profile.tsv
```

Reads back a stream [`--profile`](#flags) filed and writes its summary to stdout: the spans by total time, the magnitude sites beneath them, and — for a run that never returned — the stack it was inside when it stopped.

Present only in a compiler built with the `profile` feature, which carries the flag too; `PATH` is spelled as it was to `--profile`. Reading is a pass of its own rather than the profiled run's last step, since the runs worth reading are the ones that never finish ([`curios-profile`](../curios-profile/README.md#the-library-emits-records-and-aggregation-is-one-of-its-consumers)).

## The manifest

A `curios.toml` declares a package or an umbrella, never both, and one that declares neither is refused naming what each would have had to state. Unknown keys are refused too, so a misspelling is an error rather than a setting that silently did nothing.

A package is a namespace for definitions and an umbrella a namespace for packages, which is why no manifest is both: an umbrella whose first member is this package is one directory away.

| Package key | Is |
| --- | --- |
| `name` | required: the package's canonical name, its mount prefix and every consumer's only way to refer to it |
| `compiler` | required for a standalone package: its minimum Curios version, a bare SemVer version; an umbrella member inherits this from its umbrella instead |
| `description` | one line, which opens the landing page [`document`](#document) writes |
| `default` | which executable a bare `run`, `compile` or `test` means; it may name a declared row or the package's own `exe.crs` program |
| `[format]` | how [`format`](#format) writes the package's files: `width`, the columns a line is written within, 50 through 100; an umbrella member inherits this from its umbrella instead |
| `[dependencies]` | one row per package this one reaches, keyed by that package's name |
| `[[executables]]` | one row per program beyond `exe.crs`: `name`, and `path` when the file is not `<name>.crs` |
| `[[foreign]]` | one row per provider of this package's [`foreign` declarations](language.md#foreign-declarations): what each target links, in `[foreign.native]` and `[foreign.browser]`, and what its exports answer |

```toml
name = "hello"
compiler = "0.16.9"
default = "bench"

[[executables]]
name = "bench"            # compiled from bench.crs

[[executables]]
name = "serve"
path = "tools/serve.crs"  # unless the row says otherwise
```

An executable's `path` is spelled plainly, relative to the manifest: no `.`, `..` or leading `/`, since the checks that keep a file in one unit compare it as written. Its own modules load from the directory it sits in, so a program with modules of its own and a directory to itself is `path = "tools/serve/serve.crs"` beside `tools/serve/*.crs`.

`compiler` states a minimum, not a range or an exact toolchain pin: `compiler = "0.16.9"` accepts that version and newer, using SemVer precedence and ignoring build metadata. An older compiler is refused before compilation or cached execution, naming the manifest, the required minimum and the running version. Every dependency is checked against its own effective requirement too. A standalone package must state one: there is no implicit minimum, so a manifest without `compiler` is refused, naming the line that would satisfy the running compiler, and `curios new` writes that line when it starts a package. Loose programs and standard input have no manifest requirement.

An umbrella must declare `compiler`, and every package its `members` enumerates inherits that minimum. A member cannot declare a local override; the refusal points to its umbrella. A package merely beneath an umbrella directory is still standalone unless enumerated. Inheritance follows each dependency's own membership, never its consumer's umbrella. A dependency delivered without the umbrella it needs cannot inherit and must declare a standalone minimum in its own manifest.

`[format]` is inherited as `compiler` is: an umbrella's table is every member's, a member that writes one of its own is refused naming its umbrella, and a package no umbrella enumerates states its own or takes every setting's default. A width outside 50 through 100 and a key the table does not take are refused where the manifest is read.

```toml
[format]
width = 72
```

A `[[foreign]]` row names a module in the table of the target that links it, and says which of its exports answers each of the package's `foreign` declarations it is named for:

```toml
[[foreign]]
name = "sha2"                    # a handle for `curios pin`; no program refers to it

[foreign.native]                 # the WebAssembly module the native target links
path = "foreign/libsha2.wasm"    # or `url`, never both
hash = "f1:9c3a…"                # either way

[foreign.answers]
"digest/sha256" = "SHA256_Digest"  # in the package `crypto`: `/crypto/digest/sha256`
```

`[foreign.browser]` names the JavaScript gateway a browser links instead, its exports answering the same declarations, and `[foreign.browser.static]` what the page serves beside it:

```toml
[foreign.browser]                # the JavaScript gateway a browser links
path = "web/dom.js"
hash = "f1:…"

[foreign.browser.static]         # page path = package path
"app" = "build/app"
"logo.png" = "art/logo.png"
```

A row states either table or both, and either may be written inline, `native = { path = "…", hash = "…" }`; a row with neither is refused where the manifest is read, saying where a module goes. A target that reaches a declaration whose row lacks its table is refused where the program is linked, naming the row and the table, so a row with only `[foreign.browser]` is its author's word that the native target links nothing for it.

Both sides of a `static` entry are plain relative paths, as an executable's `path` is, and none reads from `.curios/`. No file of any package may stand at `index.html`, `index.js` or `index.wasm`, the names a page holds for itself: a `static` key, a carried gateway's `path` and a fetched gateway in a row named `index` that would stand on one are refused where the manifest is read, the rule holding for a library as for a program.

`path` names a module the package carries, spelled plainly as an executable's is; `url` names one fetched once and filed in the shared store. `hash` is stated for both ([why](../curios-package/README.md#a-pin-is-derived-by-one-command-and-checked-everywhere-else)).

`f1:` is plain SHA-256 over the file's bytes, so `sha256sum` computes it and a release's checksum file already states it. A delivery that fails its hash is refused naming both digests, whichever way it arrived.

`[foreign.answers]` maps each declaration to the export of that module that answers it. A module answers its own package's declarations: each is a `foreign` of the package's library, named from the package's root and spelled plainly, as an executable's `path` is, so a row of another package or of the standard library has no spelling here. A path written in full, `/crypto/digest/sha256`, is refused with the spelling it has; an entry that names no `foreign` the library declares is refused where the library is compiled, listing the ones it does; and a package with no `lib.crs` has no declaration for a module to answer. A declaration is answered once: the table is keyed by what is answered, so a row names one export for it, and a second row naming one for a declaration an earlier row answers is refused where the manifest is read, naming both rows. Export names are local to their module, so two rows may name the same export.

A `foreign` no entry names is the host's to answer. A program asks only for what it reaches: a declaration nothing calls needs no implementation, and one a program reaches that neither a module nor the host answers is refused before the program starts, naming it. An export is held to its declaration when the two are linked, so a module that answers another type is refused there as well.

| Umbrella key | Is |
| --- | --- |
| `compiler` | required: the minimum Curios version all enumerated members inherit |
| `[format]` | how [`format`](#format) writes every enumerated member's files, which they inherit |
| `members` | the packages this umbrella governs, as paths; an umbrella governs a package **only if it enumerates it**, so a directory nothing enumerates is governed by nothing above it, however deep it sits |
| `catalog` | pins the members may draw on, keyed as a dependency table is |

A dependency is pinned exactly, and its name is how every consumer refers to it. A package named `json` mounts at `/json` and no consumer may rename it, which is what lets two dependents on one package share it instead of compiling it twice. Two dependents that pin one name two different ways is a refusal naming both, and it fires before anything is compiled.

```toml
name = "app"
compiler = "0.16.9"

[dependencies]
json = { source = "git", url = "https://github.com/you/json", rev = "…", hash = "c1:…" }
shape = { source = "path", path = "../shape" }
```

| `source` | Requires | Is |
| --- | --- | --- |
| `git` | `url`, `rev`, `hash` | a repository at an exact revision, accepted by hash |
| `path` | `path` | a live sibling on disk, for a project with no umbrella over it |
| `member` | — | a live member of the governing umbrella, unpinned because live code has no pin |
| `catalog` | — | the governing umbrella's `[catalog]` row of this name |

No `url` or `rev` may begin with `-`: [`curate`](#curate) hands both to `git` as positional arguments, which would read a leading `-` as an option. `member` and `catalog` are markers rather than resolvers, so a `[catalog]` row may name neither — umbrellas do not nest, and nothing sits above one to answer a marker it wrote. A catalog row on its own fetches nothing: activation lives in the package that names it.

```toml
compiler = "0.16.9"
members = ["app", "base"]

[catalog]
json = { source = "git", url = "https://github.com/you/json", rev = "…", hash = "c1:…" }
```

## Exit codes

Exit status is a tri-state, so tooling can tell "here is your goal batch" from "something is wrong" without parsing stderr.

| Code | Meaning |
| --- | --- |
| `0` | compiled, and for `run`, the program itself exited 0 |
| `1` | a hard error, alone or beside written goals; a command line that does not parse; for `lint`, a lint; for `test`, any test that failed, trapped, exited or could not be built, and a `--filter` that matched nothing |
| `2` | the program contains [written goals](language.md#written-goals) (`?`) and nothing else stopped it, and their report went to stderr — for `wonder --stage`, `--cost` and `--tests` too, when goals alone kept them from answering |

A running program's own exit code passes through untouched, so `0` never hides a failure — and once the program has started, the code is the program's, which the tri-state cannot tell from the compiler's: a program may exit 1 or 2 itself. Tooling that needs to know whether a program was ever started checks it first, with [`lint`](#lint) or [`wonder`](#wonder), or runs the executable [`compile`](#compile) wrote.

`run --target browser` serves until it is stopped, so it ends by the signal that stops it, or with `1` before it serves: a page that cannot be built, a port that is taken. `test --target browser` serves until every selected test has a verdict, then exits as `test` exits, and ends with `1` before it serves for the same reasons.

A command whose answer goes to standard output — a `wonder` query, `lint`, `format -`, `profile` — ends with `141`, and says nothing, once whatever reads the answer has gone, as `curios wonder --stage wasm | head` leaves early: the code a shell reports of any filter a broken pipe ended, and the one a Curios program's own `print` ends with.

The tri-state describes a command line that parsed. One that did not — an unknown command, an unknown flag, no command at all — exits 1, with the usage message on stderr rather than a compiler report; `--help` and `--version` exit 0.

## Flags

`--budget` and `--manifest` belong to the commands that read them, so they follow the command: `curios run --budget 200000000 serve`, `curios wonder --manifest ../app/curios.toml`.

| Flag | Taken by | Effect |
| --- | --- | --- |
| `--manifest <PATH>` | every command but `new` and `profile` | use this `curios.toml` as the governing package's, instead of the nearest one |
| `--budget <UNITS>` | every command that elaborates, which is every one but `new`, `curate`, `pin`, `format` and `profile` | units of reduction work each declaration may spend while type checking — a transition costs one, a construction costs what it builds |
| `--color <WHEN>` | every command, on either side of it | paint what the command reports on standard error: `auto`, `always` or `never` ([What a command reports](#what-a-command-reports)) |
| `-h`/`--help` | `curios` and every command | what that command takes, with the default each flag was built with |
| `-V`/`--version` | `curios` itself | the build's version, so a bug report can say which compiler produced the output |
| `--profile <PATH>` | every command, on either side of it | write one record per span and event to `PATH` — present only in a compiler built with the `profile` feature, and inert without it. [`profile`](#profile) reads the stream back |

The budget is restored at every declaration boundary, so it bounds the heaviest declaration rather than the compilation; `--help` prints the default this build carries, which is why no number is written here.

## Standard input

`-` runs whatever arrives on standard input, which is what makes a heredoc a program:

```sh
curios run - <<'EOF'
/std/print("Hello, Curios!\n")
EOF
```

It is loose in the same sense a file no unit declares is, and answers before anything looks for a manifest — so `-` means the same thing inside a package as outside one, never the package's default executable, and no `--manifest` can govern it. `compile -` builds it where `--output` says.

Standard input is asked for with `-`, and never read because a pipe is attached ([why](design/tools/an-argument-names-one-subject-and-each-command-states-what-it-accepts.md)).

One thing follows from the program being anonymous, and it is a refusal rather than a guess: **it resolves no file-backed modules.** `mod util;` looks in the directory its root sits in, and text on standard input sits in none, so it fails as an unfound module. Inline modules — `mod util … end` — work normally, so a program on standard input can still be structured, just not spread across files.

Text written as a module — items with no final term — is checked as one, as a file written as a module is: `wonder`, `wonder --tests`, `lint` and `test` answer it as a unit of its own mounted at `/stdin`, since standard input has no stem to mount it at, and `run`, `compile`, `wonder --stage` and `wonder --cost`, which need a program, refuse it as written as a module.

Diagnostics name it `<stdin>` where they would name a file, keeping line and column:

```text
╰─ error; <stdin>:1:19
```

The program's own standard input is spent on reading the source, so a read of it reports the end of the input. A program that reads its input wants a file, or a path the shell mints for it — `curios run <(your_generator)` is an ordinary file argument, and leaves standard input alone.

## Where things go

Everything generated lands under one `.curios/`, beside the governing manifest — the umbrella's when one governs, so its members share it — in nine entries, a product's named for the target it is built for and every other for what it holds:

| Entry | Holds | Under `CURIOS_CACHE` |
| --- | --- | --- |
| `native/<package>/<name>` | what `compile` emits for the native target | stays |
| `browser/<package>/<name>/` | what `compile` emits for a browser: the page's directory | stays |
| `documentation/<package>/` | what `document` emits | stays |
| `sources/<scheme>/<digest>` | materialized dependency sources, keyed by the hash they were accepted against | moves |
| `foreign/<scheme>/<digest>` | fetched foreign modules, keyed the same way | moves |
| `verdicts/<slot>` | judged units, one slot per unit: a library, or an executable's own declarations | moves |
| `objects/<slot>/<pack>` | what each verdict slot names, in packs beside it named by what they hold | moves |
| `payloads/<target>/<slot>` | precompiled payloads, one slot per executable and target, each target's in a directory of its own | moves |
| `compiler` | this machine's memo of the digest of the compiler running now, against a stamp of its binary | moves |

One transient entry passes through beside them: [`pin`](#pin) stages a delivery at `pinning.<pid>` while the digest that will name it is still being computed, then renames it into `sources/` or `foreign/`. It is removed however the command ends, so seeing one means a `pin` was interrupted, and nothing ever reads it.

`.curios/` is the only directory the toolchain writes into, unless `CURIOS_CACHE` names another for the shared half — everything above whose key says nothing about which project asked. `native/`, `browser/` and `documentation/` are products of the package that declared them, nested under its name so two members of one umbrella never collide, and always stay in the project's `.curios/`. A source's hash scheme is a directory of its own.

Set `CURIOS_CACHE` to share that half across projects; unset, each project keeps its own, and nothing is written anywhere else ([why](../curios-package/README.md#the-store-goes-where-a-manifest-governs-and-nowhere-it-was-not-pointed)).

## Reusing what was already built

Neither `run` nor `compile` recompiles a declared executable nothing has changed. The precompiled payload is filed in the store beside the units, and an invocation whose entry file, whose entry's own modules and whose dependencies all still hold what they held is served from it and narrates nothing, where a compile shows a tree for each unit it compiled and the program's own ([What a command reports](#what-a-command-reports)). One slot serves both subcommands, so `compile` after `run` only writes the executable, and `run` after `compile` compiles nothing. `test` and `document` file what they compile the same way, so an invocation of either whose sources are unchanged recompiles nothing.

An edit anywhere the program was built from is a miss, and so is a damaged or half-written store entry; the invocation that misses recompiles and refiles, and the one after it is fast again.

A question — [`wonder`](#wonder), and [`lint`](#lint) with it — reads the store and files the units it compiled from the text on disk, the units a build would have filed and no payload, so a build after a question compiles only what the question did not, and the question after it compiles nothing. A unit it finds filed from an earlier text of the same sources it compiles over that unit as a baseline, as a build does, keeping every declaration that read nothing the edit changed, which is what keeps a question about one declaration from costing the whole library it sits in; what it compiles that way it files as a build would have, so the question after a saved edit is the last to compile it. It files no unit compiled from text an editor holds unsaved, nor any unit after one, so a server asking on every keystroke files nothing.

A store that cannot be written costs the reuse and never the answer. The command goes on: each unit's `store` node closes `refused`, and why hangs beneath the first of them, once however many units went past it. [`wonder --server`](#wonder) says nothing of it.

A loose `.crs` file consults and writes nothing: it has no project, hence no store — the same declared-versus-loose split as everywhere else. Standard input has none either, so its test program is compiled every time.

Payloads are native code for the machine that built them, so an entry is found only by an engine that can run it; two machines share one only when their engines agree. Nothing has to be cleaned up by hand as sources change: each executable occupies one slot, overwritten in place.
