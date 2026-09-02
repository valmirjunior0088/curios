# A command-line interface is a value, and the values it parses are typed by it

## Status

Designed, not started. This file records a survey of what twenty-five argument parsers across fourteen languages agree on, the smallest surface that covers what a user of any of them expects, the mechanisms the tree's compiler was probed to support on 2026-09-01 — every claim below marked *probed* was elaborated through `wonder` and, where it says so, executed through `run` — the contract, the items that land it in order, the shortcuts it takes and the compiler items they stand in for, and the decisions taken. The module keeps its own contract in `curios-prelude-archive/std/Cli.crs` once written, and nothing here restates one. Nothing is started.

## Why it exists

Everything after the target of `curios run` reaches the program as `/std/proc/args`, and nothing above that exists: an executable that takes an option walks `List(Bytes)` by hand, as `/syn/Test/main` does for its one index argument. `/std/Parse` parses bytes, `/std/Fmt` renders a format string whose arity is computed from its text, and neither touches argv. A language whose executables are its product has no way to declare an interface, print a help screen, or refuse a misspelled flag.

The survey covered the full-featured tier (Rust's clap, Python's argparse, click and Typer, Haskell's optparse-applicative, OCaml's cmdliner, Go's cobra with pflag, Swift's ArgumentParser, Scala's decline, Kotlin's kotlinx-cli, Lean 4's lean4-cli, Elm's cli-options-parser, TypeScript's cmd-ts and Optique), the deliberately minimal tier (Rust's lexopt, pico-args and argh, Node's `util.parseArgs`, Deno's `parseArgs`, Elixir's `OptionParser`, Go's `flag`, Zig's zig-clap), the type-level tier (Haskell's commander-cli and optparse-generic, an Idris prototype of exclusivity by type), the conventions (the POSIX utility syntax guidelines, GNU's argument syntax, clig.dev), and one retrospective (five years of decline). What every member of the first tier has and every member of the second cuts is the line this specification draws.

## What is certain

Read from the survey, from `/std` source, and from the probes.

- **The expected surface is the same everywhere.** Every full-featured parser has long options with values, boolean flags, short aliases, positional arguments, required versus defaulted versus optional, repeated options collecting a list, subcommands, a generated `--help`, a `--version`, and a usage error printed with a usage line and a nonzero exit. Every minimal parser cuts help, subcommands, typed conversion and exit handling, and each says so in its scope statement. Those ten items are the deliverable.
- **What every parser leaves to a later release is a longer list than what it ships.** Mutually exclusive groups, argument groups in help, environment-variable fallback, configuration-file precedence, shell completion, prefix abbreviation, counted flags, negatable flags, options with an optional value, fixed-arity `nargs`, arguments read from a file, colour, suggestions, hidden options and subcommand aliases are each present in some parsers and absent from most, and decline's author counts keeping them out as the decision that kept the library alive. clig.dev advises against abbreviation outright.
- **The token grammar is settled by GNU and pinned by lexopt.** `--long value`, `--long=value`, `-s value`, `-svalue`, clustered flags `-abc` with only the last member allowed a value, `--` ending options, a lone `-` as an ordinary positional, options and positionals in any order, and a value token taken verbatim even when it begins with `-`. Neither single-dash long options nor abbreviation is standard, and both lexopt and clig.dev refuse them.
- **Help and error layout is the modern consensus.** A one-line description, `Usage: name [OPTIONS] <POSITIONAL>`, then `Arguments:`, `Options:` and `Commands:` sections with aligned `-s, --long <METAVAR>  help` rows; on a usage error the message and the usage line on standard error, a pointer to `--help`, and exit status 2 — argparse, clap and Go's `flag` all exit 2 — and `-h` wins over every other error on the line.
- **Arguments arrive as `Bytes`.** `/std/proc/args` is `Io(List(Bytes))` (probed), which is lexopt's `OsString` decision: the host does not promise text. `Str/of_bytes` decides, so a non-UTF-8 argument is refused by name rather than lossily converted, which is what [Numeric carriers narrow by refusing](../design/toolchain/numeric-carriers-narrow-by-refusing-never-by-changing-a-value.md) says about a value that does not fit.
- **A record type computed from the spec reduces** (probed, run). With `Arg` a struct and `Carrier(a: Arg) -> Type` a match on its kind, `Values: (List(Arg)) -> Type` declared as an indexed inductive — `nil(): ([])` and `cons(@a, @rest, head: Carrier(a), tail: Values(rest)): ([a, ..rest])` — is inhabited, and a spec-driven builder `fill(spec, raw) -> Result(Str, Values(spec))`, written as a list fold under the motive `(s) => Result(Str, Values(s))`, elaborates and runs. This is what clap's derive macro, Typer's type hints, Swift's property wrappers and zig-clap's comptime spec each do outside the language, done inside it.
- **Access by name is typed by a type-level lookup, and the spec is inferred from the value** (probed, run). `Lookup(spec, name) -> Type` walks the list comparing `Str/eql(a.long, name)` and reduces to `Nat` for `"port"` and `Str` for `"name"` at a literal spec. `get(@spec, v: Values(spec), name, @ok: Has(spec, name)) -> Lookup(spec, name)` elaborates — the value-level walk matches on `v` under the motive `(s, w) => (ok: Has(s, name)) -> Lookup(s, name)` and on the comparison under a motive that refines both the bound and the result — and `get(v, "port")` inside a handler infers the spec from the index of `v`'s type. `Has` is decided in the sense of [A bound is stated in a decided proposition and discharged by reduction](../design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md), so `get(v, "prot")` is refused at compile time.
- **A dependent field types the handler** (probed, run). `struct Command { name: Str, args: List(Arg), run: (Values(args)) -> Io({}) }` is accepted, and inside a literal `run(v) = …` sees `v` at the `args` written two fields above, so `Cli/get(v, "port")` there is `Nat` with nothing annotated. A subcommand therefore carries its own typed handler, which is how cobra, click, Swift and lean4-cli dispatch, without the parent computing a sum type over its children.
- **Spec validity is a decided proposition too** (probed). `Distinct(spec) -> Prop`, a structural walk that is `False` when a later entry repeats an earlier long name and `True` at the end, is discharged silently for a well-formed literal and refused for one with a duplicate. bpaf's `check_invariants` and clap's debug assertions run this at program start; here it runs at elaboration.
- **A type-level walk must be structural** (probed). An index loop under an invariant, which is what `/std/List/find` used to be, is refused in a `Prop`-valued function as "not known to terminate". The same `find` written as a case split with general recursion on the tail — `[h, ..t] => match pred(h) | true => some(h) | false => find(t, pred) end` — keeps the early exit and is accepted in a type position, under the rule that a call on a constructor payload bound by the pattern decreases; `/std/List`'s predicates and searches are all written that way now.
- **The cost scales** (probed, run). A fifteen-argument spec with `get` on its last entry and `Distinct` demanded at `main` answers `wonder` and compiles under two seconds each, prelude restoration included.
- **A spec entry may carry its value type** (probed, run). `struct Value { A: Type, metavar: Str, read: (Str) -> Result(Str, A) }` is accepted and `Value { A = Nat, … }` constructs it, so a reader is data rather than a witness — optparse-applicative's `ReadM`, cmdliner's converters and cmd-ts's `Type` — and a program's own type joins a spec by supplying one.
- **A well-formedness check cannot recurse through a struct's list field, and can through an inductive's** (probed). `Ok(n: Node) -> Prop` matching `n.kids` and calling `Ok(k)` on an element is refused as "does not terminate on every input" when `Node` is a struct, and accepted when `Node` is an `induct` whose arm binds `kids` as a payload. The size order grades a constructor payload below its constructor and does not grade a projection below the struct it came from.
- **The kernel refuses one shape the module wants** (probed, run). With `Arg` carrying a `Type` field, a top-level `main(cmd: Command)` that calls a spec-polymorphic `fill` and hands the result to `cmd.run` is accepted by the elaborator and refused by the certifier with `expected Values.{w,u,v}((cmd).0), found Values.{0,1,0}((cmd).0)`. The same program passes with the `Type` field removed, with the `Result` passed to `main` as a parameter, or with `fill` declared as a local `let` inside `main`. `wonder diagnostics` reports the refusal. The cause is the one `curios-elab/src/reduce.rs` states on its own conversion key: universe levels are erased while they are being solved, so no equality between `fill`'s fresh instance levels and the field's concrete levels is recorded, and the instance levels generalize into `main`'s own. The kernel fails closed, which is the safe direction and a refusal of a correct program.

## The contract

One module, `/std/Cli`, whose namesake is the interface a program declares. Six declarations carry the whole surface.

- `Value` — `{A: Type, metavar: Str, read: (Str) -> Result(Str, A)}`, with `str`, `nat`, `int` and `flt` supplied and `reader(metavar, f)` for a program's own type.
- `Arg` — `{long: Str, short: Option(Char), help: Str, kind: Kind}`, where `Kind` is `flag()`, `option(v: Value, presence: Presence(v.A))`, `many(Value)`, `positional(Value)` or `rest(Value)`, and `Presence(A)` is `required()`, `optional()` or `default(A)`. The smart constructors `flag`, `option`, `optional`, `default`, `many`, `positional` and `rest` write the common shapes; a short alias or another metavar is a struct update, `Arg { ..Cli/flag("verbose", "Log every request"), short = Option/some('v') }`, so there is no builder to learn.
- `Carrier(a: Arg) -> Type` — `Bool` for a flag, `A` for a required or defaulted option and for a positional, `Option(A)` for an optional one, `List(A)` for `many` and `rest`.
- `Values: (List(Arg)) -> Type` and `get(v, name)` — the parsed record and its only accessor, typed as above. There is no access by label: labels are syntactic and `v.port` cannot be computed from `"port"`, so `Cli/get(v, "port")` is the spelling.
- `Command` — `{name: Str, about: Str, version: Option(Str), args: List(Arg), run: (Values(args)) -> Io({}), commands: List(Command)}`. A command either has arguments and a handler or has subcommands. Sharing an argument between subcommands is writing the same `Arg` value into both lists, `[..common, port]`, since a spec is a value.
- `WellFormed(cmd: Command) -> Prop` — distinct long names, distinct short names, nothing named `help`, `h` or `version`, at most one `rest` and it after every positional, an empty `args` wherever `commands` is nonempty, distinct subcommand names, and every subcommand's own `commands` empty. Decided by reduction and demanded by `main`.

Three functions run it. `parse(args, argv: List(Str)) -> Outcome(args)` is the pure core — the tokenizer and matcher, answering `parsed(Values(args))`, `help`, `version` or `failure(Failure)` — and is what the tests exercise. `help(cmd) -> Str` and `usage(cmd) -> Str` render the layout below from the same data. `main(cmd, @ok: WellFormed(cmd)) -> Io({})` reads `/std/proc/args`, drops the program name, refuses a non-UTF-8 argument by its position, selects a subcommand by the first positional, prints help or the version to standard output and exits 0, prints a failure with the usage line to standard error and exits 2, and otherwise runs the handler.

The tokens, in the order a token is tried:

| Token | Reads as |
| --- | --- |
| `--` | every later token is a positional |
| `-` | a positional |
| `--name=value` | the option `name` with `value`, `=` included in `value` after the first |
| `--name` | the flag `name`, or the option `name` taking the next token verbatim as its value |
| `-abc` | the flags `a` and `b`, then `c` as a flag or as an option taking the rest of the token or the next token |
| anything else | a positional, filled in declaration order, then collected by `rest`, else `unexpected` |

An option written twice keeps its last value; `many` keeps every value; a flag written twice is set. `-h` or `--help` anywhere on the line answers `help` before any failure is reported, and `--version` likewise when the command declares one. A long name is matched whole: there is no abbreviation and no `-name`.

The program that results, and what it prints:

```crs
use /std/{Cli, Nat, Str, Option, Io, print};

let serve: Cli/Command =
    Cli/Command {
        name = "serve",
        about = "Serve a directory over HTTP",
        version = Option/some("0.1.0"),
        args = [
            Cli/Arg { ..Cli/flag("verbose", "Log every request"), short = Option/some('v') },
            Cli/default("port", Cli/nat, 8080, "Port to listen on"),
            Cli/positional("root", Cli/str, "Directory to serve"),
        ],
        run(v) =
            let port = Cli/get(v, "port");
            let root = Cli/get(v, "root");
            print(Str/flatten(["serving ", root, " on ", Nat/to_str(port), "\n"])),
        commands = [],
    };

Cli/main(serve)
```

```text
Serve a directory over HTTP

Usage: serve [OPTIONS] <ROOT>

Arguments:
  <ROOT>  Directory to serve

Options:
  -v, --verbose   Log every request
      --port <N>  Port to listen on
  -h, --help      Print help
      --version   Print version
```

```text
error: unexpected argument '--prot'

Usage: serve [OPTIONS] <ROOT>

For more information, try '--help'.
```

## What lands, in order

Each item names the test that pins it; the tests live in `curios/src/tests/cli.rs` and script argv through the mock host, which already serves `args`.

### 1. `List/find` becomes structural

Landed ahead of this module, with `/std/List`'s other predicates and searches: `/std/List/find` is the case split above — the same signature, the same early exit, no index loop and no invariant, accepted in a type position — and `curios/src/tests/aggregates/list_tests.rs` pins a `Prop` computed through it at a literal. Nothing remains for this item.

### 2. The declarations and the pure core

`Value`, `Arg`, `Kind`, `Presence`, `Carrier`, `Values`, `Lookup`, `Has`, `get`, the smart constructors, `WellFormed` over a single command, `Failure`, `Outcome` and `parse`. Pinned by tests over `parse` alone: each row of the token table, each `Presence`, `many` and `rest`, a duplicate option, an unknown option, a missing required option, a reader refusing a value with the reader's own reason, `--` and `-`, and `get` at each carrier. Two compile-time tests: `get` at a misspelled name is refused, and a duplicate long name is refused at `WellFormed`.

### 3. Help and the entry

`help`, `usage` and `main`, with `Command` at one level. Pinned by tests over `help` for the layout above, over `main` for the exit status and stream of each outcome, and for a non-UTF-8 argument refused by position.

### 4. Subcommands, one level deep

`commands` on `Command`, selection by the first positional, `Commands:` in help, an unknown or missing subcommand as a failure listing the names, and `WellFormed` extended by the rules on `commands`. Pinned by a two-command program run both ways and with neither.

## Shortcuts taken, and the compiler items they stand in for

Each is recorded here because the roadmap's rule is that a shortcut is recorded when it is taken. None of these is part of this campaign; each is a compiler or library item in its own right, kept in this file until someone picks it up.

- **`fill` is a local `let` inside `main`, not a public function.** The kernel refusal above is the reason. The repro is twenty lines: `struct Value { A: Type }`, `struct Arg { long: Str, v: Value }`, `Values` indexed over `List(Arg)` with `head: a.v.A`, `struct Command { args: List(Arg), run: (Values(args)) -> Io({}) }`, a top-level `fill(spec: List(Arg)) -> Result(Str, Values(spec))` folding `spec` under the motive `(s) => Result(Str, Values(s))`, and `main(cmd: Command) -> Io({})` matching `fill(cmd.args)` into `cmd.run`. The item is in `curios-elab`: conversion of two instances of one global should record a level equality per instance position instead of erasing both, or the key's own note — "concrete levels kept apart, undecided ones collapsed. None is written." — should be written. It is a candidate for the unsoundness hunt even though the kernel's direction here is the safe one, since the same looseness that refuses this program is the one the hunt's regression at `recheck::tests::a_case_equation_does_not_refine_an_occurrence_at_another_universe_instance` already closed on the kernel's side.
- **Subcommands are one level deep and `WellFormed` does not recurse.** A recursive `WellFormed(child)` reached through `cmd.commands` is refused for the projection reason above; the fix is to grade a projection out of a struct below the struct in `curios-analysis/src/totality.rs`, where a struct is the one-constructor inductive it is. Until then the rule "every subcommand's own `commands` is empty" is stated flat, and nesting waits for that grading or for a fuel-bounded walk, which `Fmt`'s parser shows the shape of.
- **The tokenizer writes no prefix helpers of its own.** `Str/starts_with`, `Str/split_once` and the rest of `/std/Str`'s decomposition surface landed before this module, so the tokenizer is written over them directly.
- **The library never writes a `Values` literal by hand, and the tests do not either.** `Values/cons(8080, …)` is refused because the numeral is realized as `Nat` before the constructor's index is solved, while `Values/cons(x, xs)` with `x` already typed is accepted: `curios-elab/src/typing.rs`'s `blocked_on_metavar` parks only lambdas, lists and tuples against an expectation stuck on a metavariable, and `elaborate_num_lit` commits to the stuck type as its target. The item is a numeral joining the parked forms, with a settle arm that defaults it when nothing arrives. Every `Values` in this module is built by `fill`, so the module does not meet it.
- **The refusal of `get(v, "prot")` reads "implicit argument 'ok' of '/get' was not inferred; supply it explicitly".** It names the binder and not the bound, exactly as an out-of-range `Str/get` does today. The birth record at `curios-elab/src/zonk.rs`'s implicit branch holds the metavariable's type, and the witness branch beside it already renders one; rendering the bound would say what was not discharged. Beside it, the printer spells a `Str` literal by its representation — `Str { x[0x70, …], of_scan_eq(…) }` in a goal report — so the rendered bound is legible only once a literal spells as `"port"`. Both are the elaborator's and the printer's, not this module's.

## Decisions taken

- The module is `Cli`, named for the value a program declares, as `Parse` and `Fmt` are; `Args` names the input.
- `Values` is an indexed inductive, not a computed tuple, so `get(v, name)` infers the spec from `v` and a handler needs no annotation.
- The handler is a field of `Command`, so a subcommand carries its own typed handler and no sum type is computed; `main(cmd, handler)` was rejected because it cannot type a subcommand's handler.
- A repeated non-`many` option keeps its last value, as argparse, Node and Go do; `many` is the spelling for keeping every occurrence.
- A command with subcommands has no arguments of its own, as Swift and cmd-ts have it; an argument is shared by writing the same value into each child. Inheritance of a parent's arguments is a feature with its own specification when a consumer wants it.
- A non-UTF-8 argument is refused by position. A `Bytes` reader is one constructor when a consumer needs one.
- Positionals are required; `rest` covers the trailing list and `optional` the named case. Optional positionals are not in the MVP.
- Exit status 2 on a usage error, 0 for help and version. `--version` is long-only, since `-v` is verbose and `-V` is clap's alone. Help wins over any other failure on the line.

## Deliberately not specified

Mutually exclusive groups, which the Idris prototype shows can be a proposition on the values and which no consumer has asked for. Options whose carrier depends on another option's value, which Optique's `dependency` does and which the `Values` telescope could express. Counted flags, negatable flags, optional-value options, environment-variable and configuration fallback, shell completion, abbreviation, colour, suggestions, hidden entries, subcommand aliases, showing a default in help, nesting deeper than one level, and the derivation of a spec from a `struct` declaration, which is a derivation slot rather than a library. The diagnostic wording when a decided bound is refused and the spelling of a `Str` literal in a diagnostic, which are recorded above as items and are not this module's.
