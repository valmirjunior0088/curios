# A command line is one certified grammar

Working specification for `/std/Cli` as one algebra: the arguments a program takes, the record they are read into and the commands it is split into are nodes of one certified grammar; certification costs what the declaration holds; a declaration that cannot describe one deterministic command line is refused with its reason, at compile time as at run time; and what a command line is expected to offer attaches to a leaf of that grammar.

## What this builds on

- **The grammar** ([CLI grammars carry structural evidence about their actual tree](../../../curios-std/README.md#cli-grammars-carry-structural-evidence-about-their-actual-tree)). `Grammar(A)` is a transparent `tree: Node(A)` with erased `Sound(tree)`: twelve node kinds (`pure`, `flag`, `option`, `positional`, `rest`, `map`, `validate`, `product`, `choice`, `optional`, `default`, `repeat`), certified by `checked` at run time and by `declare`, whose guard reduces, at compile time. One scanner owns tokens (`--name=value`, `-abc` clusters, `--`), positionals are allocated structurally, and only selected leaves are decoded.
- **The declaration** ([Complete CLI specifications precede invocation processing](../../../curios-std/README.md#complete-cli-specifications-precede-invocation-processing)). `Declaration` is a tree of command leaves, each a grammar and a handler `(A) -> Io({})`, and of groups of declarations, which take no arguments of their own; `Cli` carries the tree's `Sound` evidence; `main` reads the process's arguments, answers help and version, and exits 2 on a refused invocation and 1 on a refused specification.
- **The record adapter** (`curios-std/src/Cli.crs`, `Cli/record.crs`). `Arg`, `Kind`, `Presence` and `Value` describe a flat list of arguments, `record` compiles it to a chain of binary products, and `get(v, "name")` reads a field at the type its entry decides, the name checked by `Has` at compile time.
- **Its users.** The 64 tests under `curios-std/src/tests/cli/`; nothing else in the workspace calls `/std/Cli`.

## The gap

1. **Two vocabularies over one engine.** `Cli/flag`, `option`, `optional`, `default`, `positional`, `rest` and `many` build `Arg` data; `Grammar/flag`, `option`, `optional`, `default`, `positional`, `rest` and `repeat` build nodes: one word, two meanings. `Value` and `Grammar/Reader` are one idea twice, `Reader` being `Value` without its type. A short name is given by a `Named` literal in one and by a structure update of an `Arg` in the other, and the record imposes the last-occurrence policy on every option, which its own doc calls legacy.
2. **Certification grows with the square of a declaration.** `Sound` of a `product` or a `choice` walks both subtrees again for `aliases`, and for the positional and selector facts (`has_positions`, `required_block`, `nullable_positions`, `requires_named`, `unnamed_alternatives`, `owns_input`), and `disjoint` looks each name of one side up in the list of the other's (`curios-std/src/Cli/Grammar.crs`). `record_tree` nests its fields to the right, so the `k`-th of `n` fields walks and compares against the `n - k` after it, and a record of `n` options costs on the order of `n²` steps to certify, at compile time by reduction and at run time alike. Interpretation recomputes `Prepared/size` at every node in the same way.
3. **A refusal at compile time hides its reason.** Probed at `00627ea06`, a fixed grammar declaring `--port` twice reports that the implicit argument `apart` of `Grammar/product` was not inferred, that nothing discharged `Holds(Grammar/disjoint(Grammar/aliases(…), Grammar/aliases(…)))`, which reduces to `False`, and advises a guard; `checked` refuses the same grammar as `duplicate option names: --port, -p`. A misspelled `get` reads the same way.
4. **A group takes no options, and a command is a second tree.** Options every command shares, as `git -C dir commit` has, have no place; a subcommand's grammar is certified as its own and again inside its group; selection, help and dispatch run on the declaration tree beside the grammar engine.
5. **What a command line is expected to offer and does not** (each from the prior art below): an environment variable standing for an absent option; a counted flag (`-vvv`); a negated switch (`--no-color`); a value from a closed set, listed in help; defaults shown in help; aliases and options help hides; a suggestion for a misspelled option or command; a negative number as a value without `--`.
6. **Code.** `shout` is written in `Cli.crs` and again in `Cli/render.crs`, and the `nat` and `flt` readers refuse with one message.

## Prior art

- **optparse-applicative** ([0.18.1.0](https://hackage-content-origin.haskell.org/package/optparse-applicative-0.18.1.0/docs/Options-Applicative.html)): `Parser` is `Applicative` and `Alternative`; `command :: String -> ParserInfo a -> Mod CommandFields a` embeds a whole sub-parser whose result is the parser's own, so subcommands are nodes of the grammar; `showDefaultWith` renders a default; `disambiguate` accepts abbreviated long options, off by default.
- **decline** ([usage](https://ben.kirw.in/decline/usage.html)): `Opts` with `orElse`, `withDefault`, `orNone`, `validate` and `mapValidated`; `Opts.subcommand(command)` is one more `Opts`, so a program's options and its commands combine as a product; `Opts.flags` counts; `Opts.env` reads the environment.
- **cmdliner** ([`Arg`](https://erratique.ch/software/cmdliner/doc/Cmdliner/Arg/index.html), [tutorial](https://erratique.ch/software/cmdliner/doc/tutorial.html)): a converter pairs a parser with a printer, which renders the absent value in the manual (`absent=10`); `Arg.info ~env` names a variable read where the argument is absent; `enum` maps names to values; manual pages, `--help` and the exit statuses are generated.
- **clap** ([derive reference](https://docs.rs/clap/latest/clap/_derive/index.html), [`Arg`](https://docs.rs/clap/latest/clap/struct.Arg.html)): `env`, `ArgAction::Count`, `value_enum`, `global`, `hide`, `visible_alias`, `allow_negative_numbers`; `Command::debug_assert` checks a declaration in a test, where this library certifies it.
- **argparse** ([3.14](https://docs.python.org/3/library/argparse.html)): `count`, `BooleanOptionalAction` (`--foo` and `--no-foo`), `choices`, `suggest_on_error`, `allow_abbrev`, subparser `aliases`, exit status 2 on misuse.
- **swift-argument-parser** ([repository](https://github.com/apple/swift-argument-parser)): commands as types, `@OptionGroup` shared between a command and its subcommands, `validate()`.

None of them proves a declaration deterministic before it runs; that stays this library's.

## Decisions

- **One vocabulary, the grammar's.** `/std/Cli` exposes leaves, readers, combinators, the record and commands; `Arg`, `Kind`, `Presence` and `Grammar/Reader` retire. An optional or defaulted option is `optional` or `default` over `option`, and an option states its own `Multiplicity`.
- **The record is the grammar's named product.** A field is a name and a grammar, `Field { A: Type, name: Str, grammar: Grammar(A) }`; `record(fields)` is a grammar of `Values(fields)`, and `get` keeps its type and its compile-time name check. `product` and `map` stay for a program that builds its own structure.
- **A reader is a converter.** `Value` holds its type, its metavar, its parser and its printer; the printer renders a default in help, and `one_of` builds a reader of a closed set, which help lists.
- **Commands are grammar nodes.** `command(name, about, grammar)` is a node, and `commands([...])` a choice that owns the first positional word and hands the rest of the line to the command it names; options every command shares are a `product` with it. A program is `Cli(A) { name, about, version, grammar: Grammar(A) }`, certified whole, and `main(cli, run: (A) -> Io({}))` runs over what the line parsed into, so a parse is a value a test reads. `Declaration`, `leaf`, `group`, `select` and `Chosen` retire; help and usage are of the command the line selected.
- **Certification costs what the declaration holds.** Each node carries a summary computed from its children's: its names, as a set, and the positional and selector facts the rules read. An obligation reads two summaries and never a subtree, and the names of a record's `n` fields are certified in `n log n` comparisons.
- **A refusal names its reason, once.** Each rule is a proposition that reduces to `True` or to an empty proposition holding the rule and the parts that break it (`NamesCollide(["--port", "-p"])`), so `declare` at compile time and `checked` at run time say the same thing from one statement: `Error` is that proposition's data, and `Error/to_str` reads it.
- **What a leaf states.** Its long name, its short name and its aliases, its help, whether help hides it, and the environment variable that stands for it when absent; the environment is an argument of interpretation, which `main` reads from the process.
- **Out of scope.** Abbreviated long options, which a later option can make ambiguous. Completion scripts and manual pages, readings of the same tree, are a specification of their own once a program asks for one.

## Open questions

- **What `count` and `toggle` are.** Both are written with today's nodes: `map(repeat(flag(names)), List/len)` counts, and a `choice` of `--x` and `--no-x` under a `default` toggles. Either they are functions of `/std/Cli` built from those nodes, which help renders as their parts, or node kinds of their own, which help renders as `-v...` and `--[no-]x` at the cost of two more kinds every walk, rule and summary handles.
- **How a node's names are held.** A sorted list, merged at each node with a merge that refuses an equal pair, or a `/std/Set(Str)`, whose insertions cost the length of a name: stage 1 measures both on a record of 64 options and keeps the cheaper at compile time.

## Stages

1. **Linear certification and refusals that name their reason**, the public surface unchanged. Check: every refusal under `tests/cli/` reads as it does; a fixed grammar declaring `--port` twice reports `NamesCollide(["--port", "-p"])` at compile time; the units a record of 64 defaulted options costs to certify, counted before and after; `Cli.crs` and `render.crs` share one `shout`.
2. **One vocabulary**: converters, leaves, the record over fields, `Arg` and its kin retired, the tests restated. Check: the restated tests, and `get` refusing a misspelled name with the names the record has.
3. **Commands as nodes**: `command`, `commands`, `Cli(A)`, `main(cli, run)`, help and usage of the selected command, `Declaration` retired. Check: today's selection, help and refusal tests restated over the grammar; a shared option read before and after its command's name; a misspelled command refused with the commands there are.
4. **What a leaf states and the scanner's rules**: aliases, hidden options, the environment fallback, `one_of`, defaults and value lists in help, `-5` read as a value where no short name is a digit, certification refusing a digit as a short name, and a suggestion by edit distance for an unknown option or command. Check: one test for each, and help rendered for a leaf of each kind.

## Verification

Each stage's check, and once the last lands: every test under `curios-std/src/tests/cli/` passes, and the units a record of 64 options costs to certify are recorded with their commit, counted: the `budget::consumed` samples of the declaration in the stream a profiling build files for `wonder diagnostics` over a program declaring it with `record` and `declare`.

## Design decisions this overturns or corrects

- **Complete CLI specifications precede invocation processing** loses `Declaration` and its groups to commands in the grammar, and the record's last-occurrence policy to each option's own; what it decides of certifying the whole declaration before reading a line, of the exit statuses, and of help preceding version stands.
- **CLI grammars carry structural evidence about their actual tree** gains the summaries and the named refusals; what it rejects stays rejected.

## Rejected

- **A monadic grammar**, which reads a later argument by an earlier one's value and loses the static tree help, certification and completion are read from.
- **A grammar of `Io({})`**, each command mapped to its action as decline's `Command[Unit]` is, which leaves a test no parse to read.
- **Deriving a command line from a structure's fields**, as clap derives one: a derivation is the compiler's closed set (`DerivationSyntax`), and the record already reads fields by name.
- **Keeping both vocabularies, one as an alias of the other**, which states one fact twice.
- **Choosing an alternative because its reader succeeds**, and **sealing the representation**, both already rejected and not reopened.

## Completion and retirement

Each stage rewrites this specification to what remains, and its contracts and rationale go to `curios-std/README.md`'s two decisions as it lands; once the fourth lands, the tests hold the shape and this file is deleted.
