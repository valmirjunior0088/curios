# A declaration is asked through one selector

Adopted: `curios wonder` asks about one declaration through one selector, `--name <PATH>`, and every view of a declaration is an option beside it — `--name <PATH> --witnesses`, `--name <PATH> --footprint`, `--name <PATH> --stage <STAGE>` — so a reader asking what one declaration became, weighs or reads as at a rung gets that declaration and nothing of the library's. Today `--witnesses <PATH>` is a selector of its own, `--cost` lists every named function of a program, and `--stage <STAGE>` prints a program whole at its rung. Settled with the user on 2026-11-18, lifted from the findings as a delivery of its own.

`--name <PATH>` answers a declaration's block, and `--witnesses <PATH>` that block and the witnesses it takes part in ([`wonder`](../../usage.md#wonder)). `--cost` counts the functions bearing each declaration's name either side of the optimizer (`curios-cps/src/survey.rs`) and states one outcome a row, `survived`, `specialized <n>` or `absorbed`, the last both inlining and pruning; `--stage <STAGE>` prints its rung's module whole (`Stage`'s `Display`, `curios-pipeline/src/stage.rs`), rooted at the program's entry from `anf` on.

`--at <LINE>:<COLUMN>` stays the hover's question, answered with the record [The editor is answered at a position](101-the-editor-is-answered-at-a-position.md) makes of a place, and takes no view. A view of a module, `--name <PATH> --exports`, has landed, with the parse of a view beside `--name` that stage 1 extends (`View`, `curios/src/cli.rs`).

The next step is stage 1, a delivery.

## What is known

- **`--cost` answers almost nothing of the program asked.** It is one row per named function the continuation IR held before the optimizer, the program's and the library's alike: 104 rows for a program of four declarations reading its input, five of them its own and a lambda among those (`/sum_squares/1`), the rest `/std`'s and the runtime's, and 93 for the same program closed, its computation folded before the IR and none of its own declarations counted — counted at `bc3f3817c`.
- **What a reader asks of a declaration is in the graphs already read, with nothing instrumented**, which the survey's module decision keeps: a loop the optimizer made of a function keeps the function's name on its continuation, so a function absorbed by contification is told inlined into the loop its caller holds; an emitted Wasm function keeps its name, `$func/N$<name>`, before Binaryen and after, through the finish hook the command line already runs; a frame's weight is read off its `frame/enter`, and a function's rebuilds off its `Rebuilt` values.
- **Each rung can be narrowed to one declaration by what bears its name**: at `text`, `core` and `core-elab` the items it lowered to, spelled as Core text spells them; at `anf` and `cps`, either side of their optimizers, the functions bearing its name — its own, its lambdas, its workers, the optimizer's copies, a loop that kept the name — printed as the binding subtrees they are with the row declarations they name; at `wasm` the `$func/N$<name>` functions; at `wasm-optm` the same, as Binaryen prints one function. rustc filters its MIR dumps by function (`-Zdump-mir=<filter>`), and LLVM what it prints (`-filter-print-funcs`).
- **A query is one option at most, and the subject is the one positional argument**, so a second value on a query would take the subject for a path.
- **`--name` answers off the unit, without compiling a program**; what `--footprint` reads exists once a program is compiled to Wasm.
- **`--name` and `--at` answer two questions, each where the other cannot** (probed on 2026-11-18 over a file one of whose declarations is refused): `--name` answers a declaration as its unit was elaborated and certified, a dependency's or `/std`'s in full whatever the subject does, and refuses the subject's own where the subject does not check, having nothing elaborated of it; `--at` answers what a place in a file being edited holds, the editor's hover in the terminal, whatever the subject does (`curios-wonder/src/named.rs`).

## Decided

- **`--name <PATH>` is the one selector.** With no view it answers as it does today, the block then the declaration's whole text as its author wrote it; with a view, the block then the view. A query is one view at most beside the selection.
  - Rejected: `--at` as a second selector taking the views (the user, 2026-11-18): it is the hover's question, answered with the record a place holds, and kept to it.
  - Rejected: one block for `--name` and `--at`, the facts a compilation established of the declaration: `--name` would answer a refused subject's own declarations from the lowering, a second question under a name that asks for what was elaborated.
  - Rejected: a selector of its own, `--of <PATH>`, beside `--name`, a second word for one declaration.
  - Rejected: a rung and a path joined in one value, `--stage cps-optm:<PATH>`, a separator to learn.
- **`--witnesses` is a view**, answering as it does today under the selection, and the block's pointer to it is spelled `--name <PATH> --witnesses`. A view that needs a program compiled, `--footprint` or `--stage`, compiles one, and where the program stops before what the view reads, the block answers and the view reports what stopped it, as `--stage` does today.
- **`--footprint` takes `--cost`'s place**, asked of a selected declaration alone, the program-wide listing going with `--cost`: a row for each function bearing the declaration's name — its own, its lambdas, the copies the optimizer made — reading `kept`, `copied <n>`, `inlined into <caller>` or `pruned`, beside its bytes before Binaryen and after, its frame weight and its rebuilds. Only a program subject asks it. It is named for what it answers, what the compiled program holds of the declaration and what that weighs; `--cost` was a misnomer for a view that runs and times nothing.
  - Rejected: a field of `--name`'s block, which would compile a program to answer a question asked off the unit.
  - Rejected: keeping the listing beside the view, almost every row of it the library's.
  - Rejected: reasons in a row, why a pass inlined or copied, which a pass would have to record as it decides and the survey's decision keeps out.
- **`--stage <STAGE>` under a selection prints, at its rung, what bears the declaration's name**, the set `--footprint` reports from `anf` on; alone it prints the program whole, as today.

## Stages

1. **The selector and its views.** `--name` takes `--witnesses` as a view, the command line reading it beside a selection as the parse `--exports` brought reads its view (`impl FromArgMatches for Query`, `curios/src/cli.rs`), a second view refused; `documentation/usage.md`'s query rule, its `--name` and `--witnesses` rows and the block's pointer restated, and the language server's use of the queries traced. Check: the parse of each combination held by a test, a view beside `--at` refused; `--name <PATH>` and `--name <PATH> --witnesses` answering as `--name` and `--witnesses` do today; `curios-wonder`'s suites. One commit.
2. **`--footprint`.** The survey answers one declaration's functions with what became of each, `inlined into` read off a continuation's name; their bytes either side of Binaryen; their frame weights and rebuilds; `--cost` and its listing go, with `usage.md`'s row and paragraph. Check: a program's declaration answered with its lambdas' rows, a contified one `inlined into` its caller's loop, a folded one `pruned`, bytes on both sides matching the `$func/N$<name>` functions printed at `wasm` and `wasm-optm`. One commit.
3. **`--stage` narrowed.** Each rung prints the part bearing the selected declaration's name; `usage.md`'s `--stage` row and paragraph restated. Check: every rung under a selection, each printing that declaration's part and nothing of the library's; a declaration absorbed by the optimizer answered at `cps-optm` with the loop that kept its name. One commit.

## Verification

`cargo xtask workspace lint` and `curios-wonder`'s and `curios`'s CLI suites at each stage; the whole workspace's tests once stage 3 has landed.

## Retirement

Once stage 3 lands, `documentation/usage.md` holds the queries and the survey's module documentation what `--footprint` reads, and this file is deleted.
