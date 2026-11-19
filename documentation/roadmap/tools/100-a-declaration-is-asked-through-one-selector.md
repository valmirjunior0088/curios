# A declaration is asked through one selector

Adopted: `curios wonder` asks about one declaration through one selector, `--name <PATH>`, and every view of a declaration is an option beside it — `--name <PATH> --witnesses`, `--name <PATH> --footprint`, `--name <PATH> --stage <STAGE>` — so a reader asking what one declaration became, weighs or reads as at a rung gets that declaration and nothing of the library's. Today `--stage <STAGE>` prints a program whole at its rung, whatever a reader asks it of. Settled with the user on 2026-11-18, and restated the same day from its objective alone.

What a program holds of a declaration past Core is read here off a fact each function and continuation carries: the function of the declaration's text it was made of, its lineage (`curios_common::Lineage`), stamped where erasure mints it and kept by every copy, and never off the hint a dump prints.

The next step is stage 1, a delivery.

## What is wrong today

- **The whole rung.** `--stage <STAGE>` prints its rung's module whole (`Stage`'s `Display`, `curios-pipeline/src/stage.rs`), rooted at the program's entry from `anf` on. Over a program of four declarations reading `n` from its arguments — `square`, `sum_squares` folding `square` over `List/range(0, n)`, the tail-recursive `count` adding `sum_squares(p)` for each `p` below `n`, and `execute` — the rungs from `anf` to `wasm` are 2,721, 2,515, 2,315 and 39,091 lines, counted at `af957cfd0`, and `/count` is one binding among them at `cps-optm`, the loop `~k1206$/count` inside a copy of `io/bind`, which `--footprint` answers as `inlined into io/bind`.

## What this builds on

- **A function carries its lineage.** Erasure stamps every function it mints inside a declaration's binding with the declaration's path and the function's place among the declaration's, every copy keeps it, the CPS lowering gives it to each continuation it makes of a function's body, contification moves it onto its loop and a heap twin keeps its original's (`curios_common::Lineage`); the survey reads what a graph holds of each (`held`, `curios-cps/src/survey.rs`), which `--footprint` answers. Nothing prints it.
- **The survey reads the graphs either side of the optimizer and instruments no pass**, a decision of its module: `Stage::CpsOptm` is observed after the optimizer, the rebuild in place and the frame bound (`lower_from_anf`, `curios-pipeline/src/compile.rs`), so the graph it reads holds every copy, twin, rebuild and guard.
- **The emitter names a CPS function's Wasm functions by one rule, which it hands out** (`emitted_functions`, `curios-emit/src/table.rs`), so a reader finds a function's code in the emitted module by looking each up, and the `wasm` rung is WebAssembly's own text format, which any reader of it reads.
- **A declaration's items are its text's.** A unit's anchors give what each declaration's text lowered to, which `--at` and `--name` already read (`curios-wonder/src/named.rs`), and a unit keeps every declaration's text, which `--name` prints.
- **A query is one option at most, and the subject is the one positional argument.** `--name` answers off the unit, without compiling a program; `--name` and `--at` answer two questions, each where the other cannot (`curios-wonder/src/named.rs`), probed on 2026-11-18 over a file one of whose declarations is refused.

## Prior art

- **A dump narrowed to one function.** rustc filters its MIR dumps by function (`-Zdump-mir=<filter>`), and LLVM what it prints (`-filter-print-funcs`).
- **One name asked, many facts answered.** Idris 2's `:di <name>` dumps a name's multiplicity, erasable arguments, compiled version, runtime-linked terms and size-change graph, beside the whole-file `--dumpcases` and `--dumpanf` ([Debugging the compiler](https://idris2.readthedocs.io/en/latest/reference/debugging.html)).
- **Where inlined code went.** LLVM tells it two ways: a pass remark, `-Rpass=inline` printing `'foo' inlined into 'bar'` as the inliner decides (clang's [`optimization-remark.c`](https://llvm.googlesource.com/llvm-project/clang/+/refs/heads/master/test/Frontend/optimization-remark.c)), and a location every instruction carries, whose `inlinedAt` names the call it was inlined through ([`DILocation`](https://llvm.org/docs/LangRef.html#dilocation)).

## Decided

- **`--name <PATH>` is the one selector, and a view stands beside it.** With no view it answers as it does today, the block then the declaration's whole text as its author wrote it; with a view, the block then the view. A view is one of `--witnesses`, `--exports`, `--footprint` and `--stage`, one at most, and each conflicts with every query but `--name`, a relation the parse states once, so a view beside another query is refused where the line is parsed rather than dropped. `--stage` alone stays the query that prints a program whole. `--at` takes no view: it is the hover's question, answered with the record [The editor is answered at a position](101-the-editor-is-answered-at-a-position.md) makes of a place.
  - Rejected: `--at` as a second selector taking the views (the user, 2026-11-18).
  - Rejected: one block for `--name` and `--at`, the facts a compilation established of the declaration: `--name` would answer a refused subject's own declarations from the lowering, a second question under a name that asks for what was elaborated.
  - Rejected: a selector of its own, `--of <PATH>`, beside `--name`, a second word for one declaration.
  - Rejected: a rung and a path joined in one value, `--stage cps-optm:<PATH>`, a separator to learn.
- **`--stage <STAGE>` under a selection prints, at its rung, the declaration's part.** At `text`, its text parsed and reprinted; at `core`, the items the subject's own declaration lowered to, a unit in scope keeping no lowering, so a declaration of one is refused there with the command that asks it of its unit's file; at `core-elab`, its items in its unit's elaborated module, spelled as Core text spells them; from `anf` to `cps-optm`, the functions and continuations of its lineage, each as the binding subtree it is, under the chain of functions it stands in, which [A printer states each fact once, where it is bound](../../design/tools/a-printer-states-each-fact-once-where-it-is-bound.md) takes as a rule, so a capture reads as bound by the function named above it; at `wasm`, the Wasm functions the emitter made of them. Alone `--stage` prints the program whole, as today.

## Stages

1. **`--stage` narrowed to a unit's rungs.** Under a selection, `text`, `core` and `core-elab` print the declaration's part, a unit in scope's declaration refused at `core` with the command that asks it of its file. Check: each of the three rungs over a program's own declaration, a library's and `/std`'s, nothing of any other declaration printed. One commit.
2. **`--stage` narrowed past Core.** From `anf` to `wasm`, the part of the declaration's lineage, each under its chain of functions, and the printer decision restated with the rule. Check: every rung from `anf` to `wasm` under a selection printing that declaration's part and nothing of the library's; a declaration absorbed by the optimizer answered at `cps-optm` with the loop of its lineage under the function it stands in. One commit.

## Verification

`cargo xtask workspace lint` and the stage's tests by name at each stage — `curios-wonder`'s and the CLI's suites at each — with `curios-prelude`'s corpus tests at stage 2, which changes what reaches `curios-cps`, `curios-emit` and `curios-wasm`; the whole workspace's tests once stage 2 has landed. The figures over the program above are counted, by the release compiler.

## Retirement

Once stage 2 lands, `documentation/usage.md` holds the queries and views, the survey's module documentation what `--footprint` reads, `curios_common::Lineage`'s documentation what a function's lineage is and who keeps it, and the printer decision the narrowed print, and this file is deleted.
