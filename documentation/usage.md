# Usage

The complete command-line and package reference. The [README](../README.md) covers the happy path; this covers the rest.

- [Running and compiling](#running-and-compiling)
- [Testing](#testing)
- [Programs on standard input](#programs-on-standard-input)
- [What a package is made of](#what-a-package-is-made-of)
- [Starting one](#starting-one)
- [Asking about a program](#asking-about-a-program)
- [Exit codes](#exit-codes)
- [Formatting](#formatting)
- [Dependencies](#dependencies)
- [Fetching](#fetching)
- [Umbrellas](#umbrellas)
- [Which manifest governs](#which-manifest-governs)
- [Where things go](#where-things-go)
- [Reusing what was already built](#reusing-what-was-already-built)
- [Profiling builds](#profiling-builds)
- [Global flags](#global-flags)

## Running and compiling

`run` and `compile` take the same four forms, so what a bare invocation means never depends on which one you asked.

| Argument | Means |
| --- | --- |
| *(none)* | the governing package's sole executable, or the one `default` names when it declares several |
| an identifier | the executable declared under that name |
| anything ending in `.crs`, or holding a path separator | that file, standalone |
| `-` | the program on standard input, standalone |

The dispatch is lexical and never probes the disk: an executable's name is a single identifier, so it can hold neither `.crs` nor a path separator nor be `-`, and the spaces cannot overlap. `curios run scratch.crs` therefore means the file even when the package declares an executable called `scratch`.

Everything after the target belongs to the program, not to `curios`, and reaches it through `/std/proc/args`:

```sh
curios run serve --port 8080
```

Everything from the target onward is collected verbatim, hyphens included, so a program's own flags never collide with the compiler's — which is also why every `curios` flag must precede the subcommand.

`compile` writes its executable beside you, named after the executable it built or after the input file's stem; `-o`/`--output <PATH>` names it something else, and is required when there is no stem to take a name from.

A file argument brings no project with it — no manifest, no dependencies, not even the library of the package you are standing in. That is deliberate: project scope is reachable only through something a manifest declares, so a scratch file cannot quietly acquire one. When a scratch program does want the library, one `[[executables]]` line gives it one.

## Testing

`curios test` runs the governing package's declared tests — the `test name() = body;` declarations of its library and of each executable, always the package entire, because a test's identity is its path and a path means the same thing whichever subcommand asks. The optional argument is a **filter**, not a target: a path prefix selecting which tests run.

```sh
curios test              # every test the package declares
curios test /app/Map     # only tests whose path starts with /app/Map
```

Each unit is compiled as its own test program — the same compilation `run` performs, with the final term replaced by a synthesized scheduler over that unit's tests — and every selected test runs in an instantiation of its own, so one test's effects, traps and exits never reach another. The report is one line per test, path then outcome — `proved`, `passed`, `failed`, `trapped`, `exited N` — with a failure's report indented beneath it followed by the test's body as written, and a final line counting outcomes: `N passed, N failed` always, since that pair is what the exit code turns on, then `trapped` and `exited` when they happened. On stderr each unit is taken on as `run` takes on a target — `Processing`, then its `↳ Compiling` step — and a unit with tests to run brackets their lines between a `↳ Testing` step and a `↳ Tested` step carrying that unit's own tally; a unit with nothing selected reports its compile and nothing more. A test declared with parameters reports `proved` when the kernel settled its body under the whole telescope, as a nullary theorem does; otherwise it is a property — every value of a finite domain small enough to fit the case budget, drawn arguments for any other — and its failure report opens with the counterexample, the arguments spelled in parameter order — `for 6, 6: the condition was false` — and the run is deterministic, so unchanged sources report identically. The exit code is the tri-state below: 0 when every selected test passed or proved, 1 when any failed, trapped, exited or could not be built — and 1 when the filter matched nothing, naming it — 2 when a unit under test holds a written goal.

Test programs are filed in the project's store exactly as `run`'s payloads are, so an invocation whose sources are all unchanged recompiles nothing; the test *verdicts* are never cached — every invocation runs every selected test.

## Programs on standard input

`-` runs or compiles whatever arrives on standard input, which is what makes a heredoc a program:

```sh
curios run - <<'EOF'
/std/print("Hello, Curios!\n")
EOF
```

It is standalone in the same sense a file argument is, and answers before anything looks for a manifest — so `-` means the same thing inside a package as outside one, and never the package's default executable.

Standard input is asked for rather than assumed. A bare `curios run` already means the governing package's default executable, so reading a pipe when one happens to be attached would decide between the two by whether a terminal is present — making one command line mean different things in a shell and in a pipeline, and leaving `curios run < input.txt` compiling the input it was meant to be fed. The spelling costs one character and removes the question.

Two things follow from the program being anonymous, and both are refusals rather than guesses:

- **`compile -` requires `-o`.** There is no stem to name an executable after, and a default would claim a path the invocation never mentioned.
- **It resolves no file-backed modules.** `mod util;` looks in the header's stem directory, and there is no header on disk to take a stem from, so it fails as an unfound module. Inline modules — `mod util … end` — work normally, so a program on standard input can still be structured, just not spread across files.

Diagnostics name it `<stdin>` where they would name a file, keeping line and column:

```
   --> <stdin>:1:19
```

The program's own standard input is spent on reading the source, so `/std/read()` reports end-of-input. A program that reads its input wants a file, or a path the shell mints for it — `curios run <(your_generator)` is an ordinary file argument, and leaves standard input alone.

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

Everything else is declared. A second program needs a row, and its file is named after it:

```toml
name = "hello"

[[executables]]
name = "bench"          # compiled from bench.crs

[[executables]]
name = "serve"
path = "tools/serve.crs"  # unless the row says otherwise
```

A row always wins over the file, so `[[executables]] name = "hello"` with a `path` overrides the `exe.crs` convention for the package's own program.

With more than one executable and no `default`, a bare `curios run` refuses rather than guessing:

```toml
name = "hello"
default = "bench"
```

**The package root has one stem space.** `lib.crs`, every module it enumerates, and every executable compiled from a file directly beside the manifest all claim a stem there, and a stem claimed twice is refused naming both claimants:

```
the package root claims the stem `hello` twice: `mod hello` in `lib.crs` and the executable "hello"
```

## Starting one

```sh
curios new <DIR>
```

The directory's name is the package's, and it is checked before anything is written — so a name the manifest could not hold fails with an empty disk rather than half a package.

It writes every part a package has: the manifest, `lib.crs`, and `exe.crs`, plus a `.gitignore` for `.curios/`. There is deliberately no flag asking for only one of them. A package may be a library alone or a program alone — deleting either file says so, and the manifest decides — but *starting* one is not the moment to be asked, and a flag asking would only be answerable by somebody who already knows what the two are.

## Asking about a program

`curios wonder <QUERY> [ARGS] [TARGET]` answers a question from the compilation that would build the target, and executes nothing. The answer goes to stdout and nothing else does, so it can be redirected; exit 0 means the question was answered, including when the answer is a list of errors, and non-zero that it could not be asked — no such target, no such stage, a scope that cannot be assembled.

| Query | Answers |
| --- | --- |
| `diagnostics [TARGET]` | every diagnostic and goal, each rendered as `run` would report it, a blank line between; nothing when the target compiles. A unit that declares tests is also checked as its test program, so a parameterized test whose parameter nothing draws is reported here as `test` would report it |
| `tests [TARGET]` | every test the target declares, one path per line, in declaration order — the library's, then each executable's, for the package entire; nothing when it declares none, and nothing executes |
| `stage <STAGE> [TARGET]` | the program's representation at one rung of the pipeline, reprinted. A rung the compilation reached is answered even when a later stage refuses: the rendering goes to stdout, what stopped the program goes to stderr, and the exit is 0. Only a program that stops *before* the rung has not answered, and exits 1 |
| `server` | the same questions over the language server protocol, on standard input and output — what an editor integration launches |

The target takes the four forms `run` takes, dispatched the same way, with one deliberate difference: **a file is placed in the unit that declares it** rather than compiled alone. Nothing executes, so nothing is escalated by supplying context, and a library module analysed without its library reports every import unresolved. A file under a package's directory is analysed as that package's library; one that is an executable's entry, or sits under its stem directory, as that executable; one no manifest above it claims, standalone. The project is decided from the file's own location, not the working directory, and `--manifest` overrides it. No target at all is the governing package entire for `diagnostics` — its library, then every executable — and the sole or `default` executable for `stage`, which needs a program.

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

A question reads the store — a dependency already built is reused exactly as `run` reuses it — and never writes it, so asking costs nothing on disk and a server asking on every keystroke files nothing.

## Exit codes

Exit status is a tri-state, so tooling can tell "here is your goal batch" from "something is wrong" without parsing stderr.

| Code | Meaning |
| --- | --- |
| `0` | compiled, and for `run`, the program itself exited 0 |
| `1` | a hard error |
| `2` | the program contains written goals (`?`), and their report went to stderr |

A running program's own exit code passes through untouched, so `0` never hides a failure.

## Formatting

```sh
curios format <files…>        # rewrite in place
curios format --check <files…>  # write nothing, exit nonzero if anything would change
```

There is one canonical style and no options to configure it. Formatting is verified before anything is written — the output must reparse to exactly the same program, with every comment preserved — so a formatter defect refuses rather than corrupts.

## Dependencies

A dependency is pinned exactly, and its name is how every consumer refers to it. A package named `json` mounts at `/json` and no consumer may rename it, which is what lets two dependents on one package share it instead of compiling it twice.

```toml
name = "app"

[dependencies]
json = { source = "git", url = "https://github.com/you/json", rev = "…", hash = "c1:…" }
shape = { source = "path", path = "../shape" }
```

A `git` row requires all three of `url`, `rev` and `hash`. A `path` row requires only `path`. Two dependents that pin one name two different ways is a refusal naming both, and it fires before anything is compiled.

## Fetching

```sh
curios curate
```

`curate` materializes what the manifests reference, and it is the only part of the toolchain that reaches the network — the compiler itself never fetches. A delivered tree is accepted against its `hash` whatever transport produced it, so a mirror is no weaker than the origin, and a delivery that fails its hash is refused whoever fetched it.

## Umbrellas

Packages developed together sit under an umbrella, which declares `members` rather than a `name`, and may declare a `catalog` of pins its members draw on.

```toml
members = ["app", "base"]

[catalog]
json = { source = "git", url = "https://github.com/you/json", rev = "…", hash = "c1:…" }
```

A member reaches a sibling with `source = "member"` and a catalog pin with `source = "catalog"`. A catalog row on its own fetches nothing: activation lives in the package that names it.

An umbrella governs a package **only if it enumerates it**, so a directory nothing enumerates is governed by nothing above it, however deep it sits.

## Which manifest governs

The governing package is the one whose `curios.toml` sits in the working directory. There is no search above it, so what a command compiles is whatever an `ls` shows — and a subdirectory that holds modules rather than a manifest is not a package at all.

Only the umbrella is looked for further up, and only one that enumerates you governs you. When it does, the umbrella's directory is the governing root: that is where the store goes, and it is shared with your sibling members.

## Where things go

Everything generated lands under `.curios/`, beside the governing manifest — built executables, materialized sources, compiled units, and precompiled payloads. It is the only directory the toolchain writes into, unless `CURIOS_CACHE` names another for the content-addressed half.

Set `CURIOS_CACHE` to share the content-addressed half across projects; unset, each project keeps its own. There is deliberately no divined default, because a toolchain that writes into a home directory nobody pointed it at is doing something the person who ran it did not ask for.

## Reusing what was already built

Neither `run` nor `compile` recompiles a declared executable nothing has changed. The precompiled payload is filed in the store beside the units, and an invocation whose entry file, whose entry's own modules and whose dependencies all still hold what they held is served from it — reported as `↳ Compiling hello; reused` in place of the unit steps a compile would show. One slot serves both subcommands, so `compile` after `run` only writes the executable, and `run` after `compile` compiles nothing.

An edit anywhere the program was built from is a miss, and so is a damaged or half-written store entry; the invocation that misses recompiles and refiles, and the one after it is fast again. A question about a program (`wonder`) reads the store and never writes it. A bare `.crs` file consults and writes nothing: it has no project, hence no store — the same declared-versus-bare split as everywhere else.

Payloads are native code for the machine that built them, so an entry is found only by an engine that can run it; two machines share one only when their engines agree. Nothing has to be cleaned up by hand as sources change: each executable occupies one slot per dependency chain, overwritten in place.

## Profiling builds

`profile` exists only in a compiler built with the `profile` feature: there is nothing to report without the instrumentation the feature compiles in, so the subcommand is absent rather than empty. `cargo x profile <PATH>` builds such a compiler and runs it:

```sh
cargo x profile programs/hello_world.crs
```

It takes one path to a `.crs` entrypoint — not the four forms, since there is no project question to answer — compiles it once, and prints per-span aggregate timings sorted by total time. The instrumentation mechanics belong to `curios-profile`'s rustdoc.

## Global flags

| Flag | Effect |
| --- | --- |
| `--manifest <PATH>` | use this `curios.toml` as the governing package's, instead of the working directory's |
| `--unit <DIR>` | mount the package in `DIR` ahead of the entry program, with no manifest edge; repeat for more, in dependency order |
| `--budget <UNITS>` | units of reduction work each declaration may spend while type checking — a transition costs one, a construction costs what it builds |
| `--version` | the build's version, so a bug report can say which compiler produced the output |

The budget is restored at every declaration boundary, so it bounds the heaviest declaration rather than the compilation; `curios --help` prints the default it was built with.

`--manifest` overrides exactly which manifest is the package's. Which umbrella governs is still enumeration's answer, because a manifest cannot declare itself governed.
