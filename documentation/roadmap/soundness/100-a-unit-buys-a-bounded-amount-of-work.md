# A unit buys a bounded amount of work

Working specification for what a judgment's budget bounds. It is held to [A reduction step costs what it builds](../../design/soundness/a-reduction-step-costs-what-it-builds.md), where one counter prices transitions, constructions and depth, and to [Every stage but reduction costs the graph it is handed](../../design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md), where a compilation ends, accepted or refused for its budget, in time proportional to what was written plus what the budget counted. It closes the places where a unit buys work without bound, by the rows the budget already has, and holds the result by fixtures and a measurement. It prices no walk.

## What this builds on

- **The budget has three rows, charged before the work.** A transition costs one unit, a value a fold builds its logical size, and a level of guarded recursion the frame it takes, charged for each new peak (`curios-core`'s `cost`, `Reducer::spend`, each checker's `Spend::enter_level`).
- **A fold over the sum normal form charges what it writes back**, by its operands' sizes and before it builds (`curios-core/src/reduce/intrinsic/cost.rs`). A sum is charged the collection its summands are read into and a node a summand (`sum_bound`) wherever one is written: at an addition, at a subtraction and a comparison for the residuals a cancellation leaves, at the split a division makes of its dividend, and for the sides an `Int` comparison splits by sign. A product is charged a node for each monomial it distributes into and for each factor those hold, and the limbs of their coefficients (`product_bound`), where it distributes: at the fold over a literal or a single monomial, and in `Nat::normalize` and `int_normalize` for two sums, which stand as written until a comparison asks; a monomial the fold wrote is handed back as it stands, and a term is forced once, at its root, the nodes under a weak-head term being weak-head already. An image under the embedding of `Nat` in `Int` is one spine a monomial, charged a node a summand and two a factor (`image_bound`) at the widening, the narrowing, the magnitude and the comparison that reads two images back. The literal a sum computes is charged by its operands' widths wherever two are added. A sum built where no reducer is in hand (`words.rs`, `successor_comparison`) spells a term its reader already holds the summands of.
- **A comparison assumes no goal and unfolds a pair of calls once** ([Conversion recurrence](../../design/soundness/conversion-recurrence.md)). Neither checker keeps a history of goals or a key for one, a pair of calls whose unfolding meets the pair again is apart, and two recursions of one shape are compared as the binders they are. What a comparison repeats is bounded by the terms it compares.
- **Programs that grow are fixtures, and what a unit buys is measured.** `curios`'s `tests::runaways` states eleven programs whose one claim makes a checker do more the longer it is let run, each with the budget it answers inside and its verdict: a pair of calls met again is told apart and not left to its budget, and a sum, a product and an image that grow a step at a time answer inside a budget their fixture names. `runaway_measurements`, among the instrumented tests, reads each program's units and looks at a budget and at twice it, and is taken again where a fold's charge or a rule of conversion changes.
- **A key that holds a graph is told apart by its hash.** A table probed for a key compares it with every key it meets under the same control tag, and which keys those are follows the keys a process draws for its tables. `curios-utilities`'s `Keyed` keeps a key's own hash beside it and compares that first, over every table of the checkers whose key holds a term or a recursive group beside anything else; the two tables keyed on a term and the reduction that read it are a table a reduction. So a probe walks no key but the one it asks for, and two runs of one build agree in every judgment's looks: over `/std`, 15,255 judgments of its elaboration and 3,809 of its certification, where 543 and 244 differed (counted at `6fe03a4c9` and with the keys in, by *Two runs compared* below).
- **The kernel's comparison nests no deeper than the terms it compares.** Certifying `/std` nests `compare` at most ten deep. Where one side is opened alone a step at a time it nests a level a step, 2,544 bytes of stack each, and reaching a level costs it units: `tests::runaways` holds that program by the budget both checkers answer it inside, and what it costs the kernel beyond the elaborator is [a finding](000-findings.md). Counted at `6fe03a4c9`, by *The frames a comparison nests* below and by a count of `compare`'s depth, kept in a copy of the tree and filed where the kernel restores its budget.
- **A walk costs its graph, and a count holds that.** A read visits a node once and a rebuild rebuilds one once a depth, which `curios`'s `tests::towers` holds by fixtures that compile at sixty lines and `tower_measurements` (`curios/src/tests/instrumented/towers.rs`) counts in units and node looks. A look is a term handing out its node (`take_looks`, `curios-core/src/term.rs`), sampled for each judgment as `term::looks` beside `budget::consumed` where a checker restores its budget, under the `profile` feature.
- **The gate builds what ships.** Lint and tests run with default features; what reads the instrumentation is gated on `profile`, linted by `cargo xtask instrumented lint` and taken by hand, by the command written beside it (`curios/src/tests/instrumented.rs`). So a count of looks is no assertion of the gate's.
- **Peers.** A deterministic limit is priced one of two ways. By a meter the runtime keeps: Lean counts small allocations and adds one heartbeat for each pair of nodes its structural equality enters, "because some code … may spend a lot of time comparing" ([`expr_eq_fn.cpp`](https://github.com/leanprover/lean4/blob/master/src/kernel/expr_eq_fn.cpp), [heartbeats](https://lean-lang.org/doc/reference/4.34.0/IO/Timing/)); Wasmtime offers fuel where a trap must be deterministic and epochs where it must be cheap ([`Config`](https://docs.wasmtime.dev/api/wasmtime/struct.Config.html)). Or by a price for each operation in the size of what it takes: Ethereum repriced the operations an attack showed underpriced ([EIP-150](https://eips.ethereum.org/EIPS/eip-150)), and Tezos fits its gas to benchmarks and revises it between protocols ([Delphi](https://research-development.nomadic-labs.com/smarter-contracts-thanks-to-delphi-part-12.html)). A walk met with fuel is called implementation-dependent where it is offered: Agda's `--syntactic-equality=N` ([command-line options](https://agda.readthedocs.io/en/latest/tools/command-line-options.html)). rustc replaced the hard limit on constant evaluation by a lint ([#103877](https://github.com/rust-lang/rust/pull/103877)). Where unfolding is what grows, it is bounded and not priced: smalltt "only performs limited speculation" ([smalltt](https://github.com/AndrasKovacs/smalltt)). Equality of two shared terms is linear in their graphs ([Sharing Equality is Linear](https://arxiv.org/abs/1907.06101)).

## The gap

Read off the code, as each paragraph says; nothing here is counted.

**An `Int` sum writes every monomial again.** Every sum of the signed carrier is written through `int_from_combination` (`curios-core/src/int.rs`) — a merge, a negation, the residuals of a cancellation, the sides split by sign — which rebuilds each summand from its factors, a node a factor, where `Nat`'s keeps the monomial it read (`Nat::summand`'s origin); each is charged by its summands. The work is linear in a monomial's degree, and a degree costs its square to build, so what a unit buys here is bounded by what the budget already paid. Read off the code, not measured.

## Decisions

**What a walk reads is not priced.** Three things stand against a price on reading, by the node or by the walk.

- No program grows the walks a comparison makes of its own: `Term`'s equality at conversion's entry, `level_differences` and `mentions_rec_member` take a constant number of looks a goal at every budget.
- What a walk enters follows how its graphs share their nodes: `Term`'s equality ends at a pair that is one allocation. Nothing holds a term's sharing the same between one elaborated in this process and one restored from a stored unit, and the budget is a fact about the program.
- A price by the node is a second price list: at a unit a look, `/std`'s heaviest elaboration costs between three and four times its units and its heaviest certification a twentieth more.

A walk costs its graph, so it is bounded by the nodes the budget charged when they were built. What broke that was a fold that built for no charge, a walk a loop repeated at every level of what it walked, and a comparison that met its own goal again; each is closed where it stood, and none by a price.

**A look is a count of the program.** A key that holds a term is told apart from another by its hash before any field of it is compared, so a probe walks a key only where it is the key asked for. What a judgment looks at is then the same in every run, and `tower_measurements` and `runaway_measurements` print what a reader retakes.

**The kernel's comparison is not charged for its depth.** It nests no deeper than the terms it compares, each level of which the budget paid to reach. A level entered as reduction enters one would price again what opening the level already cost, in the checker that already pays the more.

**The objective is held by programs that answer, and measured by hand.** A fixture of the gate states a program that grows and the budget it answers inside, with its verdict; one that a rule decides is refused as apart and not for its budget, so a loop put back fails the fixture. Beside them one measurement reads units and looks at a budget and at twice it, taken where a price or a rule of conversion changes.

## Stages

Each lands alone, on its own check.

1. **An `Int` sum is charged by its factors**, or keeps the monomial it read as `Nat`'s does.
2. **Completion**, below.

## Verification

- **A declaration's units and looks.** `cargo build --release --package curios --features profile`, then `curios --profile <stream> wonder diagnostics --budget <N> -` with the program on standard input. The stream's `D` rows name its callsites and its `V` rows hold a judgment's `budget::consumed` and `term::looks`, filed where the next judgment restores the budget; the kernel files none for a declaration it refuses unless a later declaration reads it. A program that grows is run under a memory limit and a time limit, one budget a command, the smallest first.
- **Two runs compared.** `cargo xtask profile-prelude elaboration <PATH>` and `certification <PATH>`, each twice, the `term::looks` row of every judgment beside its `budget::consumed` compared between the two runs in the order filed. Held fixed, the keys are a library preloaded into the run (`LD_PRELOAD`) whose `getrandom` fills its buffer with one byte: the standard library draws a table's keys through that symbol.
- **The frames a comparison nests.** The release compiler under `gdb`, interrupted while the kernel compares: the stack pointers of its outermost and innermost frames of `compare`, and the count of those frames between them.
- **Peak memory** is the resident set `/usr/bin/time` reports for one run.
- `cargo xtask workspace lint`, which certifies `/std`, at every stage, and format, lint and the workspace's tests at completion.

## Rejected

- **A price on every look**, a meter the terms keep, as Lean keeps its heartbeats. It follows how a term is shared, and is a second price list beside the one a construction is charged by.
- **A price on the walks a comparison makes**, by the nodes each enters. No program grows them, and the count is the same meter read in fewer places.
- **One fixed key for every table**, so that two runs probe alike. The looks would repeat and still follow which keys the fixed one sets side by side, a fact of the hasher and none of the program; and a table whose keys a program can choose keeps the keys a process draws.
- **An assertion of the gate on looks.** The gate builds no instrumentation.
- **Another default budget.** What was uneven is what a unit bought, not how many units a declaration has.

## Non-goals

A classing asked again at every fold, and what a kept verdict would be keyed on, is [a finding](../arithmetic/000-findings.md) of the carriers' algebra: the budget bounds it, a question a unit at least, and `runaway_measurements` reads what it costs as its `classes` row.

## Completion and retirement

- Every program of `tests::runaways` is answered by each checker that reaches it inside the budget its fixture states, and `runaway_measurements` prints the same table twice.
- The two decisions state what holds the budget's premise.
- This file is deleted once the roadmap's line is its checked summary.
