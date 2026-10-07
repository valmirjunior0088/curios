# A reader of finished terms holds no elaboration state

Working specification for making what a judgment can reach follow what the judgment is for: one made while a unit is elaborated reads and writes that elaboration's state, and one made over finished terms, which is every judgment erasure makes, holds none of it. Both are handed one type today, so that erasure meets no metavariable, no unfinished declaration and no use site's module is its callers' care and not a fact of its types.

[A compile costs what it reads](03-a-compile-costs-what-it-reads.md) builds on this for its views: a scope read through to its units is read by a context erasure makes for itself, over the forms erasure reads.

## What this builds on

- **The context's state is its module tree's.** `Context`'s fields are reached from `curios-elab/src/context.rs` and the files under `context/` alone. Every other module of the crate goes through its methods, so how the state is grouped is internal to that tree.
- **Four stores are types already** (`Caches`, `Frames`, `Solutions`, `Program`, `curios-elab/src/context/`), with the rule that keeps them apart: a method that writes one store and must stamp or clear a cache is `Context`'s, which names both.
- **So are the budget, the asking protocol and the stance** (`Spend`, `Asking`, `Posture`, beside them): what a judgment may spend and has drawn, the protocol by which reduction asks conversion, and the module a use site is judged from with whether the prover is running.
- **A declaration is elaborated in a state of its own** ([A declaration is a function of what it reads](../../design/compilation/a-declaration-is-a-function-of-what-it-reads.md)): `Context::exchange` swaps what one attempt mints and solves for a state no other declaration wrote, and `Attempted` is that state set aside.
- **A finished module is a type.** Erasure takes a `Zonked<Module>` and projects it through `UniverseErased` (`curios-core`): that its input holds no metavariable and no level is evidence a caller hands over, never a condition a caller is trusted to have met.
- **An erased form is its item's** ([`curios-unit`'s README](../../../curios-unit/README.md#an-erased-form-is-its-items)): each item erases into a fragment on a budget restored for it (`erase_item`, `curios-elab/src/into_ersd/lower.rs`), reading every other declaration by name.
- **Erasure is directed by types and judges none** ([`curios-elab`'s README](../../../curios-elab/README.md#erasure-reads-a-heads-type-off-the-finished-term)): it reads a head's type off the finished term (`Lowering::type_of`), and nothing in `into_ersd` calls `elaborate`.
- **The two checkers share what can be shared.** `curios-core` holds the intrinsic folds, the closed machine and the `Reducer` trait that hands a fold its checker's reducer; `curios-analysis` holds the inversion, positivity and totality analyses over `Env` and `Judge`. Each checker implements those traits, and each owns its weak-head reduction of open terms, its sorts and its conversion.

## The gap

**One type holds every lifetime.** `Context` holds what stands for as long as anything is read: the names the type-directed features emit, the budget, the registries, the mounts, totality, the witness table and the base frame. It holds what stands for one unit's walk: where each of its names is written, which of its declarations have not finished, what is on offer to a proof, the witnesses it declares, the binders its proofs credited, the terms it settled and its imports. It holds what stands for one attempt at a declaration: its metavariables with their solutions, its universe solver and what it noted of its goals. And it holds the declaration a judgment is made for. A method's signature says none of which it touches, and a field's lifetime is its documentation's to state.

**An attempt's state is exchanged a field at a time.** What one attempt mints and solves is fields of `Context` that `Context::exchange` swaps one by one into an `Attempted`. That a field is an attempt's is said by its being listed there, and one that is not listed outlives its declaration.

**A walk keeps part of its state in the registries.** The witnesses a unit declares and has not elaborated, and the keys its refused ones stood at, sit in `Program` beside the witness table, which is what stands whichever unit is being elaborated.

**Erasure is handed a context, and every caller hands it an empty one.** `erase_unit`, `erase_unit_over` and `erase_program` (`curios-elab/src/into_ersd/lower.rs`) take a `&mut Context`, and `curios-pipeline` and the prelude's build each make one for the call and drop it. That it is fresh is each caller's to arrange: the type takes any context, an elaboration's among them, with its attempt's solutions, its unfinished declarations and its registries in it.

**Erasure reduces through the engine that elaborates.** `into_ersd` decides what erases by the sort of each type and reads the types its walk is directed by off the terms (`typed.rs`, `classify.rs`). To do it, it reduces a type to its head, asks a type's sort and whether it is a proposition, reads a neutral's type, installs a match arm's equations, and solves a match's indices through the shared inversion (`eliminate.rs`), which compares two forcings by `Judge::convert_at`. Each goes through the elaborator's reducer, which puts a question to the elaborator's conversion where a fold is stuck or an equation is missed, so *judges none* holds of typing and not of conversion. `curios-core` shares no weak-head reducer for open terms, so erasure has no reducer of its own to drive: the one in reach is the one that elaborates.

## Prior art

- **A layer has a context and a state of its own, and a later phase is built below the unifier.** Lean 4's `MetaM` is `ReaderT Context $ StateRefT State CoreM`, its state the metavariable context, the caches and the postponed level constraints ([`Lean.Meta.Basic`](https://github.com/leanprover/lean4/blob/master/src/Lean/Meta/Basic.lean)); `TermElabM` is `ReaderT Context $ StateRefT State MetaM`, adding the synthetic and the pending metavariables ([`Lean.Elab.Term.TermElabM`](https://github.com/leanprover/lean4/blob/master/src/Lean/Elab/Term/TermElabM.lean)); and the code generator's `CompilerM` is `ReaderT CompilerM.Context $ StateRefT CompilerM.State CoreM` ([`Lean.Compiler.LCNF.CompilerM`](https://github.com/leanprover/lean4/blob/master/src/Lean/Compiler/LCNF/CompilerM.lean)), over `CoreM` with a local context of its own, where no metavariable context can be named.
- **Unification state is a value beside the environment, and a term that may hold an unsolved variable is a type of its own.** Rocq's `Evd` "defines the pervasive unification state used everywhere in Rocq tactic engine", its `evar_map` the "Type of unification state. Essentially a bunch of state-passing data needed to handle incremental term construction" ([`engine/evd.mli`](https://github.com/rocq-prover/rocq/blob/master/engine/evd.mli)). Of `EConstr.t`: "The main difference with kernel-side types is that observing them requires an evar-map at hand in order to normalize evars on the fly", and "Before EConstr, a lot of bugs were lurking in the code base" ([`dev/doc/econstr.md`](https://github.com/rocq-prover/rocq/blob/master/dev/doc/econstr.md)). `Zonked` is that device here, read from the other side: a term with the type holds none.
- **A function lists the states it may touch.** Idris 2's unifier takes `{auto c : Ref Ctxt Defs}` and `{auto u : Ref UST UState}`, the definitions and the unification state as two labelled references ([Implementation overview](https://github.com/idris-lang/Idris2/blob/main/docs/source/implementation/overview.rst)).
- **One record, parted by lifetime.** Agda's checker is `IORef TCState -> TCEnv -> m a`, and its state is a record of three, `stPreScopeState`, `stPostScopeState` and `stPersistentState` ([`Agda.TypeChecking.Monad.Base`](https://hackage.haskell.org/package/Agda-2.6.4.1/docs/Agda-TypeChecking-Monad-Base.html)).
- **Inference state is made where inference is wanted, and none of it leaves.** rustc: "The main purpose of the inference context is to house a bunch of inference variables – these represent types or regions whose precise value is not yet known, but will be uncovered as we perform type-checking" ([Type inference](https://rustc-dev-guide.rust-lang.org/type-inference.html)), and a body's results leave through a pass "which creates a new `TypeckResults` which doesn't contain any inference variables" ([`rustc_hir_typeck::writeback`](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_hir_typeck/writeback/index.html)).
- **Erasure by a recorded quantity reduces nothing.** Idris 2: "Elaboration and the case tree compiler check ensure that 0-multiplicity arguments are not inspected in case trees. In the compiler, 0-multiplicity arguments to constructors are erased completely, whereas 0-multiplicity arguments to functions are replaced with a placeholder erased value" (the same overview).

## Decisions

1. **A context's state is two halves, by what each exists for.** `Typing` is what any judgment reads and the machine it runs on: the binder supply, the budget, the frames, the registries, the caches over them, the asking protocol and the names the type-directed features emit. `Underway` is what exists because a unit is being elaborated. A context that reads finished terms holds the second empty.
2. **Each lifetime is a value.** What one attempt mints and solves is an `Attempt`, exchanged whole. What a walk knows of the declarations that have not finished is `Needs`, and the witnesses a unit declares before they elaborate are `DeclaredWitnesses`, out of the registries. The binders the elaborator's proofs read are `Credits`, and the terms elaboration settled are `CheckedTerms`. What is set aside, taken back or forgotten together is one value, and its type holds the behaviour that touches nothing else.
3. **`Context` is where the stores coordinate, and the crate's one door to them.** A write to one store that must stamp or clear another's cache, and a lookup whose miss records a need, are `Context`'s. No module outside its tree names a store.
4. **The engine is one.** Reduction asks conversion, conversion checks a solution by typing it, and typing drives parked work: the three are one engine over `Context`, parameterized by no mode and split by no trait.
5. **A reader of finished terms cannot name elaboration's state.** `Retyping` owns a context it makes, and offers what a walk over finished terms needs and nothing else: the binder supply, the frame bracket, the lookups, the budget, and the engine's entries by name, which are a type's head, its sort, whether it is a proposition, a neutral's type, an arm's equation and a match's indices. Erasure's entry points take a budget and the syntax registry and make one, and no caller hands erasure a context.
6. **What erases is decided through no engine that elaborates.** The type erasure holds then has no elaboration state to hide, and one reducer decides both what a term's type is and what of the term is kept. Which reducer, or none, is the open question below.

## Open questions

- **What erasure reduces through.** Two forms are known, and each is a campaign. *Through the kernel*: erasure sits above `curios-cert` and reads types through the kernel's reducer and scope. `curios-elab` takes `curios-cert` as a dev-dependency only, so `into_ersd` leaves the crate and is written against the kernel's scope, and the prelude is erased where it is certified (`curios-prelude`'s build) and not where it is elaborated (`curios-prelude-archive`'s). *By a recorded relevance*: the elaborator records on each binder whether what it binds is kept, the kernel checks the record, and erasure reads it and reduces nothing, as Idris 2 erases by quantity. Core's binders, the stored format and the trusted base each take the record. [`curios-elab`'s README](../../../curios-elab/README.md#erasure-reads-a-heads-type-off-the-finished-term) rejects both as the source of a head's type, for these costs: a certifier's walk the crate cannot depend on and a build script erasing the prelude never runs, and a record the certifier has to check or erasure has to believe. They are weighed again here against more than a head's type.
- **Whether erasure's reduction can be plain.** Reduction that asks conversion nothing (`Context::plainly`) is what the shared analyses read a term by. Whether any item erases differently with every reading of `Retyping`'s plain is not known, and is one measurement: the prelude's images compared byte for byte with the reader plain. Where none does, the reader holds it by construction, and inversion's `convert_at` is the one question erasure still puts to conversion.

## The components

| Component | Treatment | Stage |
| --- | --- | --- |
| An attempt's state | One value, exchanged whole | Lifetimes |
| A walk's state | `Needs`, and `DeclaredWitnesses` out of `Program` | Lifetimes |
| What elaboration notes for a lint and for the erasure obligations | `Credits`, `CheckedTerms` | Lifetimes |
| `Context` | `Typing` and `Underway` | Halves |
| Erasure's context | `Retyping`, made by erasure | Reader |
| Erasure's reductions | Through the kernel's reducer, or none | Engine |

## Stages

Each lands alone, on its own check. The first three change nothing a compile produces, and a move there is a finding.

1. **Lifetimes.** Decision 2.
2. **Halves.** Decisions 1 and 3.
3. **Reader.** Decision 5. Erasure's entry points take a budget and the syntax registry, and `curios-pipeline` and the prelude's build stop making a context for them.
4. **Engine.** Decision 6, once its question is settled. Expected to move: where the prelude is erased, and what a unit stores if a record is taken.

## Verification

- **Nothing moves.** Through the first three stages every suite holds what it held, and the prelude's images, `sys.rkyv` and `std.rkyv` as `curios-prelude-archive`'s build writes them under `target/`, are byte for byte the ones the commit before the stage writes: every declaration of the prelude elaborates and erases to what it did. Counted.
- **The reader is the only way in.** No file under `curios-elab/src/into_ersd/` names `Context`, and `Retyping`'s context is a private field: held by the crate compiling.
- **Plain reduction.** The measurement the second open question names, taken in the Reader stage. Counted.
- **Engine.** Every item of `/std` erases to the form it has.

## Design decisions this overturns or corrects

- [`curios-elab`'s README](../../../curios-elab/README.md#erasure-reads-a-heads-type-off-the-finished-term), *Erasure reads a head's type off the finished term*: that erasure judges none is held by the type it reads through, and its rejection of the certifier's walk and of a record on Core is weighed again. Reader, Engine.

## Rejected

- **The engine generic over a mode**, so that the type erasure holds has no second half: reduction and conversion compiled twice, typing reached through a hook where conversion checks a solution, and every function of the engine made generic to remove a value that is empty.
- **A third weak-head reducer, for erasure**, driving `curios-core`'s folds and closed machine. It needs refinements for a match arm's equations, sorts, and a conversion to answer what a stuck fold asks: the kernel's `whnf`, `scope`, `sort`, `convert` and `infer` again (`curios-cert/src/kernel/`), in the elaborator's crate, held to the elaborator on every arity and mask and to the kernel on every type.
- **The reader alone**, over the context as it is: erasure cannot name the state, and the elaborator still holds every lifetime in one struct.
- **A context the caller keeps empty.** It is the form this replaces: correct for as long as every caller remembers.
- **The stores as the crate's interface**, each caller handed the one it reads. Most lookups coordinate two, a miss recording a need and a write stamping a cache, so the door they share stays one type.

## Completion and retirement

The first three stages are done when every lifetime of the context's state is a value in one of two halves and erasure makes the reader it reads through. Their decisions and what they reject then go to [`curios-elab`'s README](../../../curios-elab/README.md), `Retyping`'s contract to its rustdoc, and this specification shrinks to the Engine stage and its questions. It is done when what erases is decided through no engine that elaborates: the rule goes to the README's *Erasure reads a head's type off the finished term*, corrected in place. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
