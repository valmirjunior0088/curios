# Usage

The complete command-line and package reference. The [README](../README.md) covers the happy path; this covers the rest.

- [What governs](#what-governs)
- [What a package is made of](#what-a-package-is-made-of)
- [Targets](#targets)
- [The surface](#the-surface)
- [`run`](#run) · [`compile`](#compile) · [`document`](#document) · [`test`](#test) · [`curate`](#curate) · [`new`](#new) · [`lint`](#lint) · [`format`](#format) · [`wonder`](#wonder)
- [The manifest](#the-manifest)
- [Exit codes](#exit-codes)
- [Flags](#flags)
- [Standard input](#standard-input)
- [Where things go](#where-things-go)
- [Reusing what was already built](#reusing-what-was-already-built)

## What governs

The governing package is the one whose `curios.toml` is nearest: in the working directory, or the first directory above it that holds one. A command run anywhere inside a package — in the directory of one of its modules, say — means that package, and a build names the manifest on its `Processing` line whenever it is not in the working directory: `Processing serve (../curios.toml)`. The walk stops at the first manifest it finds, so a package nested in another's directory is its own, and an umbrella found first is refused, since it declares no package to compile.

Above the package, only the umbrella is looked for, and only one that enumerates you governs you. When it does, the umbrella's directory is the governing root: that is where the store goes, and it is shared with your sibling members.

`--manifest <PATH>` overrides exactly which manifest is the package's. Which umbrella governs is still enumeration's answer, because a manifest cannot declare itself governed.

## What a package is made of

Two files are found by *presence* rather than declared, because they are the two things a package is rather than artifacts it opts into:

| File | Is |
| --- | --- |
| `lib.crs` | the package's library, mounted at its declared `name` |
| `exe.crs` | the package's own executable, run by that same name |

So the smallest complete package is one line of manifest:

```toml
name = "hello"
```

Neither stem enters a qualified name — `lib` and `exe` are spellings nothing can refer to, exactly so a package's own name never means two things at once. `lib.crs`'s modules load from the manifest's directory (`pub mod parse;` reads `parse.crs`), which is the one place a header's namespace is not its stem directory.

One name is reserved: a package named `std` is the standard library. Compiled first, it takes the archived `/std`'s place — compiled over it, with everything the edit did not reach reused — and everything after it is compiled against it; anywhere else in a compilation it collides with the archived root as any other claim on `/std` does. It is how the standard library is worked on, not a way to swap one under a dependency, which means what it means against the `/std` it was compiled after.

Everything else is declared, in the rows [the manifest](#the-manifest) states. A declared row always wins over the found file, so `[[executables]] name = "hello"` with a `path` overrides the `exe.crs` convention for the package's own program; and with more than one executable and no `default`, a bare `curios run` refuses rather than guessing.

**The package root has one stem space.** `lib.crs`, every module it enumerates, and every executable compiled from a file directly beside the manifest all claim a stem there, and a stem claimed twice is refused naming both claimants:

```
the package root claims the stem `hello` twice: `mod hello` in `lib.crs` and the executable "hello"
```

## Targets

Every command that takes a subject takes it one of four ways, so what a bare invocation means never depends on which one you asked.

| Argument | Means |
| --- | --- |
| *(none)* | the governing package, narrowed to what the command needs |
| an identifier | the executable declared under that name |
| anything ending in `.crs`, or holding a path separator | that file, placed by what declares it |
| `-` | the program on [standard input](#standard-input), loose |

The dispatch is lexical and never probes the disk: an executable's name is a single identifier, so it can hold neither `.crs` nor a path separator nor be `-`, and the spaces cannot overlap. `curios run scratch.crs` therefore means the file even when the package declares an executable called `scratch`.

**A file is placed the same way by every command:** it is an executable's when it is that executable's entry or a module the entry's `mod` lines reach, a library's when the `mod` lines of `lib.crs` reach it, and loose otherwise — compiled against the standard library alone. A file written as [a module](syntax.md#declarations-and-modules), items with no final term, has no program in it. The project is decided from the file's own location, not the working directory.

`run` and `compile` are the only commands that need a program's **own** file: a module of one is refused, naming the executable to run instead, and a library is refused as the non-program it is. Everywhere else a module reaches its unit as readily as the entry does, which is what lets a question or a lint be asked about the file in front of you.

A loose file brings no project with it — no dependencies, not even the library of the package it sits in. That is deliberate: project scope is reachable only through something a manifest declares, so a scratch file cannot quietly acquire one. When a scratch program does want the library, one `[[executables]]` line gives it one.

## The surface

Nine commands, and `wonder`'s five queries. What each takes and leaves:

| Command | Subject | Its own arguments | Writes | Store |
| --- | --- | --- | --- | --- |
| [`run`](#run) | a program | trailing `ARGS…` for the program | nothing | reads, files what it built |
| [`compile`](#compile) | a program | `-o`/`--output <PATH>` | a native executable | reads, files what it built |
| [`document`](#document) | a library | `--archive <FILE>`, `-o`/`--output <DIR>` | pages | reads, files what it built; none under `--archive` |
| [`test`](#test) | anything | `--filter <PREFIX>` | nothing | reads, files what it built |
| [`curate`](#curate) | the governing package | — | materialized sources | none |
| [`new`](#new) | — | `<DIR>` | a package | none |
| [`lint`](#lint) | anything | — | nothing | reads only |
| [`format`](#format) | files, any number | `--check` | the files, rewritten | none |
| [`wonder diagnostics`](#wonder) | anything | — | nothing | reads only |
| [`wonder tests`](#wonder) | anything | — | nothing | reads only |
| [`wonder cost`](#wonder) | a program | — | nothing | reads only |
| [`wonder stage`](#wonder) | a program | `<STAGE>`, before the target | nothing | reads only |
| [`wonder server`](#wonder) | — | — | nothing | reads only |

`--manifest` is taken by every command but `new`. `--budget` is taken by every command that elaborates, which is every one but `new`, `curate` and `format`. Both are stated under [Flags](#flags).

## `run`

```sh
curios run                       # the governing package's sole or `default` executable
curios run serve --port 8080     # a declared executable, and its own arguments
curios run scratch.crs           # a file
```

Everything after the target belongs to the program, not to `curios`, and reaches it through `/std/proc/args`. It is collected verbatim, hyphens included, so a program's own flags never collide with the compiler's — which is also why `run`'s own flags go before the target.

A program the runtime stops rather than one that exits prints why on stderr and exits 1: `panicked:` and one sentence naming the rule that refused it — a `Nat` or `Int` past its carrier and where larger values live, a read past the end of a packed value or list, a `Flt` decoded from the wrong number of bytes, a recursive value read while its own initializer was running — followed by the wasm frames where the build kept their names.

A running program's own exit code passes through untouched.

## `compile`

```sh
curios compile                   # under the store, beside the governing manifest
curios compile -o hello          # somewhere else
curios compile scratch.crs -o s  # a loose program, which has nowhere else to go
```

Dispatched through the same code as `run`, so the two cannot drift apart. A declared executable is written under the store beside the governing manifest, nested under the package and named after the executable it built; `-o`/`--output <PATH>` writes it somewhere else instead. A program no package declares — a loose file, or standard input — is filed nowhere, so `--output` is the only place it can go, and without one the command is refused naming the flag that would build it.

## `document`

```sh
curios document              # .curios/documentation/<name>/
curios document -o site      # somewhere else
curios document --archive curios-prelude-archive/.artifacts/std.rkyv -o site   # the standard library
```

`curios document` writes a library's interface as pages, read off the compilation that builds it: what each module exports, each declaration's head printed as written with every name in it linked to where it was declared, and the `---` documentation comments attached to each. The target names the library — none for the governing package's, or `lib.crs` or a module its `mod` lines reach — since a library is the one thing with an interface; a program, a loose file or standard input is refused.

`--archive <FILE>` reads a unit already archived instead, a verdict slot under a store or the prelude image the compiler was built with, and renders the record that unit carries: that is how the standard library is documented, since it has no package to be compiled from. It takes no target, and an archived unit has no store to file pages under, so `--archive` requires `--output`.

The pages are the library's consumers' view: a private declaration or module is absent rather than hidden, a type whose representation is private shows no constructors and is marked opaque, a test never appears, and a `pub use` is a link to the declaration it re-exports — unless that declaration's own module is private and so has no page, in which case the declaration is documented on the re-exporting module's page, the facade being the only way it reaches a consumer. A reference into a dependency or the standard library renders as its qualified name in plain text, since nothing hosts their pages yet. The landing page is the root module's page: it opens with the manifest's `description`, lists the modules, then the root's own declarations; every other module's page opens with the `---` block above the `mod` that declares it, whose first paragraph is also the module's gloss on its parent's page. In prose, a pair of backticks encloses a code span.

What is written is `index.html`, one page per other module at its source path with the suffix `.crs.html` — `/json/parse/lexer` is `parse/lexer.crs.html`, so a module named `index` never lands on the landing page — and, under `static/`, the stylesheet, the three brand fonts with their licenses, the mark, the search index as one script, and the script that runs the rail and the field. Files are overwritten by name and nothing else in the directory is touched. How the pages themselves are built, and why the search index is a script rather than a file to fetch, is [`curios-document`'s](../curios-document/README.md).

A library that does not compile is not documented: its diagnostics are reported as `run` reports them, no page is written, and the exit is non-zero. Success prints nothing.

## `test`

```sh
curios test                      # every test the package declares
curios test serve                # only the executable `serve`'s
curios test --filter /app/Map    # only the tests at or under /app/Map
```

`curios test` runs declared tests — the [`test name = body;`](syntax.md#test-declarations) declarations. With no target it runs the governing package's: its library's, then each executable's. A target narrows the run to what it selects: an executable runs that executable's tests, a library module runs the library's, a file no unit declares runs its own against the standard library alone. `--filter` narrows whatever was selected to the tests at or under a path, compared a whole segment at a time, so `/app/Map` never selects a test under `/app/MapBuilder`; a filter that matches nothing is an error naming it.

Each unit is compiled as its own test program — the same compilation `run` performs, with the final term replaced by a synthesized scheduler over that unit's tests — and every selected test runs in an instantiation of its own, so one test's effects, traps and exits never reach another.

The report is one line per test, path then outcome — `passed`, `failed`, `trapped`, `exited N` — with a failure's report indented beneath it followed by the test's body as written, and a final line counting outcomes: `N passed, N failed` always, since that pair is what the exit code turns on, then `trapped` and `exited` when they happened. On stderr each unit is taken on as `run` takes on a target — `Processing`, then its `↳ Compiling` step — and a unit with tests to run brackets their lines between a `↳ Testing` step and a `↳ Tested` step carrying that unit's own tally; a unit with nothing selected reports its compile and nothing more.

A test takes no parameters, so every verdict is a fact about the one description the author wrote, and unchanged sources report identically. A claim about *every* instantiation is [a `let` rather than a test](syntax.md#test-declarations), checked by the kernel wherever its unit is checked — so a broken one fails the build rather than a run, and `curios test` never sees it.

The test *verdicts* are never cached — every invocation runs every selected test, however much of the compiling was reused.

## `curate`

```sh
curios curate
```

`curate` materializes what the manifests reference, and it is the only part of the toolchain that reaches the network — the compiler itself never fetches. A delivered tree is accepted against its `hash` whatever transport produced it, so a mirror is no weaker than the origin, and a delivery that fails its hash is refused whoever fetched it.

## `new`

```sh
curios new <DIR>
```

The directory's name is the package's, and it is checked before anything is written — so a name the manifest could not hold fails with an empty disk rather than half a package.

It writes every part a package has: the manifest, `lib.crs`, and `exe.crs`, plus a `.gitignore` for `.curios/`. There is deliberately no flag asking for only one of them. A package may be a library alone or a program alone — deleting either file says so, and the manifest decides — but *starting* one is not the moment to be asked, and a flag asking would only be answerable by somebody who already knows what the two are.

## `lint`

```sh
curios lint              # the governing package entire: its library, then every executable, then its dependencies
curios lint app.crs      # one file, placed in its unit
curios lint -            # the program on standard input
```

A lint is an exact finding the compilation already has and nothing stops on: `run`, `compile` and `test` never mention one, `wonder diagnostics` and the language server report them beside the errors and goals, and `lint` is where they turn into an exit code. There are four, every one always on, and there is nothing to configure — what keeps a name is spelled in the program, and each message says how.

| Lint | Reports | Kept by |
| --- | --- | --- |
| `unused-import` | a `use` selector, or a glob, that no reference resolved through; a `pub use` is a re-export and its own use | deleting it |
| `unused-binder` | a parameter, `let` binder, pattern binder or motive label nothing references, implicit or shadowed included; a declaration holding a written goal reports none, since its binders are the goal's scope | naming it `_x` |
| `unused-declaration` | a non-`pub` `let`, `foreign`, `induct`, `struct` or `concept` unreachable from the unit's roots — its exported surface, its tests, its witnesses and its program's tail; a private `mod` none of whose declarations is reached is reported once, at the `mod` | naming it `_x`, or `pub` |
| `unused-dependency` | a `[dependencies]` row whose package no reference in the library or any executable resolved into; decided over the package, so reported only by the package-entire form | deleting the row |

Output is what `wonder diagnostics` prints — each diagnostic, goal and lint rendered as `run` reports it, a blank line between — and the exit is the tri-state: 1 when a lint or an error was reported, 2 when only goals were, 0 when nothing but a note was. A program that does not lower reports its error alone, since the lints are read off the lowering; one that lowers and is then refused reports its lints beside the refusal.

## `format`

```sh
curios format                   # every file the governing package declares, rewritten in place
curios format serve util.crs    # an executable's files, and one file
curios format --check           # write nothing, exit nonzero if anything would change
curios format - < draft.crs     # standard input, written canonically to standard output
```

Each target names files, and what several targets name is rewritten once: no target is every file the governing package declares — its library's header and the modules its `mod` lines reach, then each executable's entry and the modules the entry reaches — an executable's name is its own, a file is itself whatever declares it, and `-` is standard input, written back to standard output. A file nothing declares is formatted only when it is named.

There is one canonical style and no options to configure it. Formatting is verified before anything is written — the output must reparse to exactly the same program, with every comment preserved — so a formatter defect refuses rather than corrupts.

## `wonder`

`curios wonder <QUERY> [ARGS] [TARGET]` answers a question from the compilation that would build the target, and executes nothing. The answer goes to stdout and nothing else does, so it can be redirected; exit 0 means the question was answered, including when the answer is a list of errors, and non-zero that it could not be — no such target, no such stage, a scope that cannot be assembled — or that the program stopped before the answer, which exits as a build would: 2 when written goals alone stopped it, 1 otherwise.

| Query | Answers |
| --- | --- |
| `diagnostics [TARGET]` | every diagnostic, goal and lint — the errors and goals rendered as `run` reports them, each lint after them as [`lint`](#lint) describes — a blank line between; nothing when the target compiles clean. A unit that declares tests is also checked as its test program, so a parameterized test whose parameter nothing draws is reported here as `test` would report it |
| `tests [TARGET]` | every test the target declares, one path per line, in declaration order — the library's, then each executable's, for the package entire; nothing when it declares none, and nothing executes |
| `cost [TARGET]` | what became of each declaration by the time the optimizer settled — `<name>`, a tab, then `survived`, `specialized <n>` or `absorbed` — one row per line, ordered by name. Nothing is judged: a row states the fate and stops |
| `stage <STAGE> [TARGET]` | the program's representation at one rung of the pipeline, reprinted. A rung the compilation reached is answered even when a later stage refuses: the rendering goes to stdout, what stopped the program goes to stderr, and the exit is 0. Only a program that stops *before* the rung has not answered, and exits 2 when written goals alone stopped it and 1 otherwise |
| `server` | the same questions over the language server protocol, on standard input and output — what an editor integration launches. It takes no target |

No target at all is the governing package entire for `diagnostics` and `tests`, and the sole or `default` executable for `stage` and `cost`, which both need a program and so refuse a file written as a module.

A file a package holds that no `mod` reaches is in no unit, so it is checked on its own — as a program when a final term follows its items, and as a module of its own otherwise — behind a note saying so and naming the `mod` line that would declare it, printed as its message alone after `note: `; a note is no finding, and no exit code counts it. A file no manifest above it claims is loose, with nothing to note. That holds for the standard library's own modules: `/std` is a package like any other, and the package named `std` is compiled over the archived unit as a baseline — reusing every declaration the edit did not reach — standing where the archived unit stood rather than beside it, so a question about `curios-prelude-archive/std/List.crs` is answered against the prelude it is part of and costs the closure of the edit rather than the library.

`cost` answers the first of the questions [a profile is a fact about the program, not about the machine](roadmap/profiling-spec.md) separates: *which cliff am I on*. A declaration is `absorbed` when nothing in the compiled program bears its name — it was inlined into its callers, or pruned once something else was — which is the row saying it costs nothing of its own; `specialized <n>` when a pass cloned it, so `n` functions stand where one was written. The two counts are read off the continuation graph before and after optimization, so no pass is instrumented and the program measured is the program that ships. Ordering is by name because the report is meant to be committed and diffed: a regression is then something to read rather than something to judge.

The stages, in the order the compiler passes them:

| Stage | Is |
| --- | --- |
| `text` | the surface syntax tree, reprinted |
| `core` | the lowered core term, which nothing has checked yet |
| `core-elab` | the same after elaboration and zonking — the module every later stage consumes |
| `ersd` | the erased term |
| `ersd-optm` | the erased term after optimization |
| `cont` | the continuation IR |
| `cont-optm` | the continuation IR after optimization |
| `wasm` | the emitted WebAssembly module |
| `wasm-optm` | the module after Binaryen optimization, rendered by Binaryen's own text writer |

## The manifest

A `curios.toml` declares a package or an umbrella, never both, and one that declares neither is refused naming what each would have had to state. Unknown keys are refused too, so a misspelling is an error rather than a setting that silently did nothing.

A package is a namespace for definitions and an umbrella a namespace for packages, which is why no manifest is both: an umbrella whose first member is this package is one directory away.

| Package key | Is |
| --- | --- |
| `name` | required: the package's canonical name, its mount prefix and every consumer's only way to refer to it |
| `description` | one line, which opens the landing page [`document`](#document) writes |
| `default` | which executable a bare `run`, `compile` or `test` means; it may name a declared row or the package's own `exe.crs` program |
| `[dependencies]` | one row per package this one reaches, keyed by that package's name |
| `[[executables]]` | one row per program beyond `exe.crs`: `name`, and `path` when the file is not `<name>.crs` |

```toml
name = "hello"
default = "bench"

[[executables]]
name = "bench"            # compiled from bench.crs

[[executables]]
name = "serve"
path = "tools/serve.crs"  # unless the row says otherwise
```

An executable's `path` is spelled plainly, relative to the manifest: no `.`, `..` or leading `/`, since the stem-space checks compare it as written.

| Umbrella key | Is |
| --- | --- |
| `members` | the packages this umbrella governs, as paths; an umbrella governs a package **only if it enumerates it**, so a directory nothing enumerates is governed by nothing above it, however deep it sits |
| `catalog` | pins the members may draw on, keyed as a dependency table is |

A dependency is pinned exactly, and its name is how every consumer refers to it. A package named `json` mounts at `/json` and no consumer may rename it, which is what lets two dependents on one package share it instead of compiling it twice. Two dependents that pin one name two different ways is a refusal naming both, and it fires before anything is compiled.

```toml
name = "app"

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
members = ["app", "base"]

[catalog]
json = { source = "git", url = "https://github.com/you/json", rev = "…", hash = "c1:…" }
```

## Exit codes

Exit status is a tri-state, so tooling can tell "here is your goal batch" from "something is wrong" without parsing stderr.

| Code | Meaning |
| --- | --- |
| `0` | compiled, and for `run`, the program itself exited 0 |
| `1` | a hard error, alone or beside written goals; for `lint`, a lint; for `test`, any test that failed, trapped, exited or could not be built, and a `--filter` that matched nothing |
| `2` | the program contains [written goals](syntax.md#written-goals) (`?`) and nothing else stopped it, and their report went to stderr — for `wonder stage`, `cost` and `tests` too, when goals alone kept them from answering |

A running program's own exit code passes through untouched, so `0` never hides a failure.

The tri-state describes a command line that parsed. One that did not — an unknown command, an unknown flag, no command at all — also exits 2, with the usage message on stderr rather than a compiler report; `--help` and `--version` exit 0.

## Flags

`--budget` and `--manifest` belong to the commands that read them, so they follow the command: `curios run --budget 200000000 serve`, `curios wonder diagnostics --manifest ../app/curios.toml`. Written before the command, either is refused with the spelling that works.

| Flag | Taken by | Effect |
| --- | --- | --- |
| `--manifest <PATH>` | every command but `new` | use this `curios.toml` as the governing package's, instead of the nearest one |
| `--budget <UNITS>` | every command that elaborates, which is every one but `new`, `curate` and `format` | units of reduction work each declaration may spend while type checking — a transition costs one, a construction costs what it builds |
| `-h`/`--help` | `curios` and every command | what that command takes, with the default each flag was built with |
| `-V`/`--version` | `curios` itself | the build's version, so a bug report can say which compiler produced the output |
| `--profile <PATH>` | every command, on either side of it | write one record per span and event to `PATH`, rotating at 512 MiB — present only in a compiler built with the `profile` feature, and inert without it |

The budget is restored at every declaration boundary, so it bounds the heaviest declaration rather than the compilation; `--help` prints the default this build carries, which is why no number is written here.

## Standard input

`-` runs whatever arrives on standard input, which is what makes a heredoc a program:

```sh
curios run - <<'EOF'
/std/print("Hello, Curios!\n")
EOF
```

It is loose in the same sense a file no unit declares is, and answers before anything looks for a manifest — so `-` means the same thing inside a package as outside one, never the package's default executable, and no `--manifest` can govern it. `compile -` builds it where `--output` says.

Standard input is asked for rather than assumed. A bare `curios run` already means the governing package's default executable, so reading a pipe when one happens to be attached would decide between the two by whether a terminal is present — making one command line mean different things in a shell and in a pipeline, and leaving `curios run < input.txt` compiling the input it was meant to be fed. The spelling costs one character and removes the question.

One thing follows from the program being anonymous, and it is a refusal rather than a guess: **it resolves no file-backed modules.** `mod util;` looks in the header's stem directory, and there is no header on disk to take a stem from, so it fails as an unfound module. Inline modules — `mod util … end` — work normally, so a program on standard input can still be structured, just not spread across files.

Text written as a module — items with no final term — is checked as one, as a file written as a module is: `wonder diagnostics`, `wonder tests`, `lint` and `test` answer it as a unit of its own mounted at `/stdin`, since standard input has no stem to mount it at, and `run`, `compile`, `wonder stage` and `wonder cost`, which need a program, refuse it as written as a module.

Diagnostics name it `<stdin>` where they would name a file, keeping line and column:

```
   --> <stdin>:1:19
```

The program's own standard input is spent on reading the source, so `/std/read()` reports end-of-input. A program that reads its input wants a file, or a path the shell mints for it — `curios run <(your_generator)` is an ordinary file argument, and leaves standard input alone.

## Where things go

Everything generated lands under `.curios/`, beside the governing manifest, in six entries each named for what it holds:

| Entry | Holds | Where |
| --- | --- | --- |
| `executables/<package>/<name>` | what `compile` emits | beside the project |
| `documentation/<package>/` | what `document` emits | beside the project |
| `sources/<scheme>/<digest>` | materialized dependency sources, keyed by the hash they were accepted against | shared |
| `verdicts/<slot>` | judged units, one slot per unit | shared |
| `payloads/<slot>` | precompiled payloads, one slot per executable | shared |
| `compiler` | this machine's memo of the digest of the compiler running now, against a stamp of its binary | shared |

`.curios/` is the only directory the toolchain writes into, unless `CURIOS_CACHE` names another for the shared half — everything above whose key says nothing about which project asked. `executables/` and `documentation/` are products of the package that declared them and always stay beside it. A source's scheme is a directory of its own so a successor scheme can sit beside `c1` rather than replace it.

Set `CURIOS_CACHE` to share that half across projects; unset, each project keeps its own. There is deliberately no divined default, because a toolchain that writes into a home directory nobody pointed it at is doing something the person who ran it did not ask for.

## Reusing what was already built

Neither `run` nor `compile` recompiles a declared executable nothing has changed. The precompiled payload is filed in the store beside the units, and an invocation whose entry file, whose entry's own modules and whose dependencies all still hold what they held is served from it — reported as `↳ Compiling hello; reused` in place of the unit steps a compile would show. One slot serves both subcommands, so `compile` after `run` only writes the executable, and `run` after `compile` compiles nothing. `test` and `document` file what they compile the same way, so an invocation of either whose sources are unchanged recompiles nothing.

An edit anywhere the program was built from is a miss, and so is a damaged or half-written store entry; the invocation that misses recompiles and refiles, and the one after it is fast again.

A question — [`wonder`](#wonder), and [`lint`](#lint) with it — reads the store and never writes it, so asking costs nothing on disk and a server asking on every keystroke files nothing. A unit it finds filed from an earlier text of the same sources it compiles over that unit as a baseline, reusing every declaration the edit did not reach, which is what keeps a question about one declaration from costing the whole library it sits in.

A loose `.crs` file consults and writes nothing: it has no project, hence no store — the same declared-versus-loose split as everywhere else. Standard input has none either, so its test program is compiled every time.

Payloads are native code for the machine that built them, so an entry is found only by an engine that can run it; two machines share one only when their engines agree. Nothing has to be cleaned up by hand as sources change: each executable occupies one slot per dependency chain, overwritten in place.
