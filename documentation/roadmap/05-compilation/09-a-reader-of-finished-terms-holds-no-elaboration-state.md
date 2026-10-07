# A reader of finished terms holds no elaboration state

Working specification for deciding what erases through no engine that elaborates. Erasure reads through a reader it makes for itself, which cannot name elaboration's state; the state is still there, empty, in the context the reader keeps, and every type erasure reads is reduced by the elaborator's reducer, which puts its questions to the elaborator's conversion.

## What this builds on

- **A reader of finished terms cannot name elaboration's state** ([`curios-elab`'s README](../../../curios-elab/README.md#a-contexts-state-is-two-halves-and-a-reader-of-finished-terms-cannot-name-the-second)). `Context`'s state is two halves, `Typing` and `Underway`, and `Retyping` (`curios-elab/src/context/retyping.rs`) owns a context it makes, offering the binder supply, the frame bracket, the lookups, the budget and six entries of the engine: a type's head, its sort, whether it is a proposition, a neutral's type, an arm's equation and a match's indices. Erasure's entry points take a budget and the syntax registry, and no caller hands erasure a context.
- **A finished module is a type.** Erasure takes a `Zonked<Module>` and projects it through `UniverseErased` (`curios-core`): that its input holds no metavariable and no level is evidence a caller hands over, never a condition a caller is trusted to have met.
- **An erased form is its item's** ([`curios-unit`'s README](../../../curios-unit/README.md#an-erased-form-is-its-items)): each item erases into a fragment on a budget restored for it (`erase_item`, `curios-elab/src/into_ersd/lower.rs`), reading every other declaration by name.
- **Erasure is directed by types and types no term** ([`curios-elab`'s README](../../../curios-elab/README.md#erasure-reads-a-heads-type-off-the-finished-term)): it reads a head's type off the finished term (`Lowering::type_of`) and asks the engine for that type's head.
- **The two checkers share what can be shared.** `curios-core` holds the intrinsic folds, the closed machine and the `Reducer` trait that hands a fold its checker's reducer; `curios-analysis` holds the inversion, positivity and totality analyses over `Env` and `Judge`. Each checker implements those traits, and each owns its weak-head reduction of open terms, its sorts and its conversion.

## The gap

**The reader's context holds an elaboration half.** `Retyping` hides it and its type does not lack it: the context inside is the elaborator's `Context`, whose `Underway` is the empty one a fresh context has for as long as nothing the reader offers writes it.

**Erasure reduces through the engine that elaborates.** `into_ersd` decides what erases by the sort of each type and reads the types its walk is directed by off the terms (`typed.rs`, `classify.rs`). To do it, it reduces a type to its head, asks a type's sort and whether it is a proposition, reads a neutral's type, installs a match arm's equations, and solves a match's indices through the shared inversion (`eliminate.rs`), which compares two forcings by `Judge::convert_at`. Each goes through the elaborator's reducer, which puts a question to the elaborator's conversion where a fold is stuck or an equation is missed. `curios-core` shares no weak-head reducer for open terms, so erasure has no reducer of its own to drive: the one in reach is the one that elaborates.

**It cannot ask less.** Erasing the prelude puts 312 questions to conversion, 310 of a stuck fold's atoms and 2 of an arm's equation, and no answer changes what the prelude erases to: with the reader's reduction plain, `sys.rkyv` and `std.rkyv` are the bytes they were. A program is another matter. A callee typed through a decision only conversion makes — `g: Pick(f(same(n)) == f(n))`, where the fold pairs atoms by spelling and conversion holds the two calls one — has a function type to both checkers and none to plain reduction, and erasure, which needs the head of every callee, record and scrutinee it eliminates, stops at `erase: applied a non-function`. Counted, as *Verification* states.

## Prior art

- **A later phase is built below the unifier.** Lean 4's `MetaM` is `ReaderT Context $ StateRefT State CoreM`, its state the metavariable context, the caches and the postponed level constraints ([`Lean.Meta.Basic`](https://github.com/leanprover/lean4/blob/master/src/Lean/Meta/Basic.lean)), and the code generator's `CompilerM` is `ReaderT CompilerM.Context $ StateRefT CompilerM.State CoreM` ([`Lean.Compiler.LCNF.CompilerM`](https://github.com/leanprover/lean4/blob/master/src/Lean/Compiler/LCNF/CompilerM.lean)), over `CoreM` with a local context of its own, where no metavariable context can be named.
- **Erasure by a recorded quantity reduces nothing.** Idris 2: "Elaboration and the case tree compiler check ensure that 0-multiplicity arguments are not inspected in case trees. In the compiler, 0-multiplicity arguments to constructors are erased completely, whereas 0-multiplicity arguments to functions are replaced with a placeholder erased value" ([Implementation overview](https://github.com/idris-lang/Idris2/blob/main/docs/source/implementation/overview.rst)).

## Decisions

1. **What erases is decided through no engine that elaborates.** The type erasure holds then has no elaboration state to hide, and one reducer decides both what a term's type is and what of the term is kept. Which reducer, or none, is the open question below.
2. **Erasure reads a type by the reduction that accepted the term.** Whatever it reduces through answers what a stuck fold and a missed equation ask, as both checkers' reducers do; a reducer that decides less leaves erasure a type it cannot read on a program both checkers accept.

## Open questions

- **What erasure reduces through.** Two forms are known, and each is a campaign. *Through the kernel*: erasure sits above `curios-cert` and reads types through the kernel's reducer and scope, which asks the kernel's conversion what the elaborator's reducer asks the elaborator's. `curios-elab` takes `curios-cert` as a dev-dependency only, so `into_ersd` leaves the crate and is written against the kernel's scope, and the prelude is erased where it is certified (`curios-prelude`'s build) and not where it is elaborated (`curios-prelude-archive`'s). *By a recorded relevance*: the elaborator records on each binder whether what it binds is kept, the kernel checks the record, and erasure reads the record where it reads a sort today, so no reduction decides what is kept, as Idris 2 erases by quantity. Core's binders, the stored format and the trusted base each take the record, and what erasure reads of a type besides its sort — a field's carrier shape, whether the entry is an `Io` — is recorded with it or keeps a reduction. [`curios-elab`'s README](../../../curios-elab/README.md#erasure-reads-a-heads-type-off-the-finished-term) rejects both as the source of a head's type, for these costs: a certifier's walk the crate cannot depend on and a build script erasing the prelude never runs, and a record the certifier has to check or erasure has to believe. They are weighed again here against more than a head's type.

## The components

| Component | Treatment | Stage |
| --- | --- | --- |
| Erasure's reductions | Through the kernel's reducer, or none | Engine |
| The reader's context | No elaboration half | Engine |

## Stages

1. **Engine.** Decisions 1 and 2, once the question is settled. Expected to move: where the prelude is erased, and what a unit stores if a record is taken.

## Verification

- **Nothing moves.** Every item of `/std` erases to the form it has: the prelude's images, `sys.rkyv` and `std.rkyv` as `curios-prelude-archive`'s build writes them under `target/`, are byte for byte the ones the commit before writes. Counted.
- **A head only conversion shows is read.** `curios`'s `tests::erasure::head_tests` hold four programs whose callee has a function type through a question to conversion: two spellings of one call, an arm's equation met at another spelling, two calls differing in a proof, a function beside its expansion.
- **What erasure asks of conversion.** The profile the prelude's elaboration files, `curios-prelude-archive/.artifacts/profile.tsv`, written by a build with every feature on: a `D` row names a callsite, an `S` row opens a span of one and a `C` row closes it. The figure is the `S` rows of `reduce::asked_classes` and of `reduce::asked_scrutinee` between an `erase_unit` span's `S` and its `C`. Counted.
- **Plain reduction.** `Retyping::new` makes its context plain (`Asking::set_plain`), the release compiler is built, and the images it writes are compared with the ones the unchanged tree writes; the four programs above are then run through it. Counted.

## Design decisions this overturns or corrects

- [`curios-elab`'s README](../../../curios-elab/README.md#erasure-reads-a-heads-type-off-the-finished-term), *Erasure reads a head's type off the finished term*: its rejection of the certifier's walk and of a record on Core is weighed again. Engine.
- [`curios-elab`'s README](../../../curios-elab/README.md#a-contexts-state-is-two-halves-and-a-reader-of-finished-terms-cannot-name-the-second), *A context's state is two halves, and a reader of finished terms cannot name the second*: the reader holds no second half, and what it reads through is not the engine that elaborates. Engine.

## Rejected

- **A third weak-head reducer, for erasure**, driving `curios-core`'s folds and closed machine. It needs refinements for a match arm's equations, sorts, and a conversion to answer what a stuck fold asks: the kernel's `whnf`, `scope`, `sort`, `convert` and `infer` again (`curios-cert/src/kernel/`), in the elaborator's crate, held to the elaborator on every arity and mask and to the kernel on every type.
- **A reducer for erasure that asks nothing.** It erases the prelude and not the language: decision 2.

## Completion and retirement

Done when what erases is decided through no engine that elaborates. The rule goes to [`curios-elab`'s README](../../../curios-elab/README.md), its two decisions on erasure and on the context corrected in place, or to the README of the crate erasure moves to. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
