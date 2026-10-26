# A term by name cannot reach a reader

The kernel's readers take their terms by a type only reading by value makes, so a term that names a `let`-bound local is refused where it would be handed to one, by the compiler, rather than met by an assertion in one reader or read as written in the others. Its next step is stage 1, a ready delivery.

## What this builds on

- **A `let` is bound for typing and read by value** ([`curios-cert`'s README](../../../curios-cert/README.md#a-let-is-bound-for-typing-and-read-by-value)): typing walks a `let`'s tail by name, so a value named twice is typed once, and everything that reads a term for more than its type reads it with each `let`-bound name unfolded, at that reader's entry. An arm binds each variable its case solves the same way ([An arm is checked in a context specialized by index inversion](../../design/theory/an-arm-is-checked-in-a-context-specialized-by-index-inversion.md)).
- **The scope's snapshots are kept by value where they are taken.** The scope stores every local's type and every `let`'s value by value, and `Kernel::bind`, binding over a local that stood for nothing, assumes again every local whose type names it and binds again every `let` whose value does, so a snapshot never names a local a later binding made `let`-bound.
- **A test runs with its checks on** ([A test is compiled optimized with its checks on](../../design/a-test-is-compiled-optimized-with-its-checks-on.md)): a debug assertion holds in every test run and costs a release build nothing.

## What is missing

**The rule is held by where a term is, not by what it is.** A term by value and a term by name are one `curios_core::Term`. Each reader keeps the rule at its entry by a convention: `Kernel::reading` raises a flag under which `Kernel::by_value` hands its term back as it is, `Kernel::naming` lowers it for a `let`'s tail, and an entry reached from typing unfolds its term and raises the flag — `whnf`, `Reducer::reduce_forced` (`curios-cert/src/kernel/whnf.rs`), `Sort::of` (`sort.rs`) and `convert` (`convert.rs`) — or calls `by_value` on every entry: an elimination's scrutinee, which keys its case equation (`eliminate::assume_case_value`); a recorded position and the type its erased half is filed under (`Kernel::record_checked`); a graded call's arguments (`calls.rs`); a recursive group, whose verdict is filed under it, a `let`'s and a group's tail type, an elimination's head, its ambient result and motive (`infer.rs`). Inside the bracket a term is believed to be by value because it was reached there: a subterm, a reduct, a snapshot, a memo's answer or what a shared analysis built and handed back.

**One reader checks what it is handed, and the others read it as written.** `step_var` (`whnf.rs`) asserts that no `let`-bound local reaches reduction by name. Nothing checks an equation's key, a position or a call: handed a term by name, an equation is keyed on a spelling no reader in its arm is handed and answers nothing, a position records a name where what is decided of it reads what the term reaches, and a call is graded on a name rather than the argument it stands for. `kernel::infer::binding_tests` holds each entry's unfolding as it stands, one fixture per reader mutation-checked against reading as written; a reader added later, or a path into a bracket that hands a term by name, is held by nothing.

## Decisions

- **The type is the kernel's.** It lives in `curios-cert`, beside `Kernel::by_value`: the elaborator keeps a `let` as a local definition and reads through it, so it has no such discipline to hold, and `Term` stays the one representation both checkers build.
- **What makes one is the door.** A value of the type is made by `Kernel::by_value`, and by a reader from values of it alone — a subterm, a reduct, a body opened at a binder the reader minted or at another such value — and by nothing else; its constructor is private to the readers' module, so a term that has not been read by value cannot be passed off as one.
- **Typing stays by name.** `infer` and `check` take `Term`: a `let`'s tail is typed over its binder, which is what types a value once, and a reader is entered from typing through `by_value`, as it is today.
- **The type marks a term already read, not one to be read.** The README rejects unfolding a `let`-bound name inside reduction: the reduct tables carry no binder. So the type does not read through the scope as it is taken apart; every value of it was unfolded once, at the outermost entry.

## Prior art

- **Rocq's `EConstr`** draws the same line with a type. An `EConstr.t` is "essentially a wrapper around `Constr.t` ensuring that `Constr.kind` does not observe defined evars", and `EConstr.kind` takes the evar map and expands evars as it reads a node; `to_constr` gives the evar-normal form, meant for a term with no free evars left ([`EConstr`](https://docs.rocq-prover.org/v8.19/api/coq-core/EConstr/index.html)). Rocq's store is its evar map, read lazily at every node; Curios's is the scope's `let`s, read once at a reader's entry, so the type here is closer to `to_constr`'s output than to `EConstr.t`: a term that needs nothing more of the store.
- **A check's result held as a type** is the argument of [Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/): a validation the caller must remember to make is a convention, and a constructor only the validation reaches makes forgetting it a type error.

## Open questions

- **The shared doors.** `curios_core::Reducer` and `curios_analysis::Env` take `Term`: shared reduction (`reduce_intrinsic`, the folds) and the shared analyses (`invert`, `retyped`, `grade`, `decide`) hand the kernel terms they built from the ones it gave them. At each such door the kernel either trusts that it is still inside a bracket, as the flag does today, reads the term by value again — `Scope::by_value` walks any term that names a local, which the flag exists to spare — or the trait takes an associated term type, which the elaborator fills with `Term`. Which, door by door, decides how much of the flag survives stage 3.
- **How far into a reader the type runs.** At the entries alone it makes every caller outside a reader unfold, and the walk inside — `whnf_within`, `compare` — keeps building `Term`s, private to its module. Running through the walk makes each node it takes apart and builds one of the type, which is the cost in signatures stage 2 counts before it is paid.

## Stages

1. **Every reader asserts, with its checks on, that what it reads names no `let`-bound local.** At each reader, on the term it is about to read — `whnf`, `reduce_forced` and `Sort::of` past their unfolding, `compare`, `Scope::refine`, the position `Kernel::record_checked` files and the arguments a call is graded on — a `debug_assert!` makes of the whole term, against its memoized free variables, the check `step_var` makes of one name. It sits on what the reader reads rather than in `by_value`, so a caller that skips the unfolding is caught where its term arrives. The tests run with it, so the whole suite holds every reader to the rule, and a release build pays nothing. Check: the workspace lint and tests; one mutation of each kind — the unfolding dropped at `convert`'s entry, at an elimination's scrutinee, at a position and at a call's arguments — each refused by the assertion in a test that reaches it; the suite's time named before and after. Answers whether anything breaks the rule today: a violation found is a finding of this area, or a ticket on the soundness board where a proof of `False` is built from it.
2. **The kernel's own readers take the type.** `whnf`, `Sort::of`, `convert`, the case equation's key (`Scope::refine`), a recorded position and the call grader take it at their entries, typing hands each `kernel.by_value(term)`, and the entries' `reads_by_value` branches go; the walks inside stay as the second open question settles, counted in signatures first. Check: the workspace lint and tests, and `curios-prelude`'s images byte-identical at one path, since no verdict, read or position moves.
3. **The shared doors, as the first open question settles.** Then `Kernel::reading`, `Kernel::naming` and `step_var`'s assertion go wherever no door is left that hands a reader a `Term`. Check: as stage 2's.

## Verification

The workspace lint and tests at each stage, and the certification records byte-identical at one path from stage 2 on. After stage 2, each binding test's mutation — a reader taking its term as it is handed — no longer compiles, and the tests state what they hold then.

## Retirement

Once stage 3 lands, the README's `let` decision states the type as what holds the rule, `binding_tests`' notes say what each fixture holds under it, nothing cites this file, and it is deleted.
