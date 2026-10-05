# The certifier confirms what it skips

Working specification for making the kernel's skip a checked step. [An independent kernel re-checks what the elaborator accepts](../../design/soundness/an-independent-kernel-re-checks-what-the-elaborator-accepts.md) states the rule: the certifier judges every declaration it is handed, and passes one over only where the environment already holds that very declaration. Its walk decides by name alone ([Judging only what is not in scope](../../design/soundness/admission/judging-only-what-is-not-in-scope.md)), so a declaration under a name the environment holds is passed over whatever it is; here it is passed over only where it is the environment's own, and refused otherwise. One comparison at the gate, in `curios-cert` alone.

It is independent of every other spec. [A declaration is a function of what it reads](../05-compilation/02-a-declaration-is-a-function-of-what-it-reads.md) later replaces the walk's gate with a declaration's write-once cells and inherits the rule stated here.

## What this builds on

- **The gate.** `verdicts_within` (`curios-cert/src/recheck.rs`) computes `fresh` as `!globals.in_scope(name)`. An item is judged unless every name it declares is in scope, and `fresh` also gates each registry entry's universe context, its residue passes and `check_induct_decl` or `check_struct_decl`. `Globals::in_scope` is one predicate over four namespaces: definitions, the two registries and the concept names.
- **The skip has two readers.** A whole compile mounts the predecessors (`curios-pipeline`'s `globals`), and the mount discipline keeps the unit's names apart from theirs, so its gate passes over nothing. An item-level recompile mounts the baseline's reused items beside the predecessors and hands the kernel the reassembled module (`recheck_over`, `curios-pipeline/src/recompile.rs`), so there the gate is what leaves the reused items unjudged. The skip is a mechanism, and a rule that refused every name in scope would refuse every recompile.
- **The registry is declared unfiltered.** `Kernel::declare_induct` and `declare_struct` run over every entry the module carries, after `Kernel::seed`, overwriting the mounted copy "with an equal one", as `recheck_over` puts it.
- **What holds the premise today**, all of it outside the trusted base: `curios-text`'s `into_core` refuses a claimed prefix a predecessor holds (`Error::MountCollision`), the elaborator's registries refuse a duplicate key (`curios-elab/src/context/program.rs`), `reassemble` copies each reused item from the baseline (`curios-elab/src/elaborate/module.rs`), and its `agrees_with` compares the recomputed stamps in a debug build.
- **Equality is cheap where it holds.** `Term`'s `PartialEq` answers by pointer, then by cached hash, then by one walk that enters a pair of shared nodes once, and a reused item is the baseline's own allocation.

## The gap

Read from the code and from `recheck::scope_tests`:

- **A definition under a name in scope is replaced, not judged.** `a_definition_under_a_name_already_in_scope_is_replaced_rather_than_judged`: the kernel certifies a module in which one name means the environment's definition, while erasure compiles the body the module holds.
- **A registry entry under a name in scope is live and unchecked.** `a_declaration_under_a_name_already_in_scope_is_live_but_unchecked`: the module's entry is what every rule reads, and its size condition, its residue passes and its universe context's satisfiability are skipped. Positivity alone still runs.
- **A name in one namespace covers an item in another.** A definition whose name the environment holds only as a concept or a registry key is passed over, with no entry it could be compared to.
- **`Globals::mount` holds definitions and nothing else.** It asserts a mounted definition is new, and extends the registries and the concept names over whatever was there.
- **Whether the second resolution reaches `False` is not yet known.** A module built by hand holding one registry entry with a constructor under the key of the prelude's `False`, the type `xboard`'s `false_type` names, and closing with that constructor, is put to the kernel as `xboard`'s `put_module` puts any module, the prelude in scope: by the code the entry overwrites the prelude's, `check_induct_decl` passes over it, and the closing term types at `False`. It has not been run.

No source program reaches any of them, since the mount discipline refuses the collision first.

## Prior art

- **Lean 4's kernel** refuses a declared name before it types anything: `check_name` throws `already_declared_exception` when `env.find(n)` answers, and `check_constant_val` calls it ahead of the checker ([`src/kernel/environment.cpp`](https://github.com/leanprover/lean4/blob/master/src/kernel/environment.cpp)). [`lean4checker`](https://github.com/leanprover/lean4checker) replays a module's declarations through that kernel over the environment its imports give.
- **Rocq's kernel** keeps the labels a structure has declared and refuses a second one: `add_field` calls `check_objlabel`, `check_objlabels` or `check_modlabel` by the field's kind, each raising `Modops.error_existing_label` ([`kernel/safe_typing.ml`](https://github.com/rocq-prover/rocq/blob/master/kernel/safe_typing.ml)).
- **Names are where kernels have been wrong.** Rocq's list of critical bugs keeps a section for its module system, "kernel and checker accept incorrect name aliasing information" among its entries ([`dev/doc/critical-bugs.md`](https://github.com/rocq-prover/rocq/blob/master/dev/doc/critical-bugs.md)).

Neither kernel has a skip: its environment is only ever added to, and it is handed nothing it already holds. Curios's is handed a whole module, some of whose items the environment answers for, which is why the rule here compares rather than refuses outright.

## Decisions

1. **A name is declared once.** A declaration arriving under a name the environment holds is the environment's own, and is passed over, or it is refused as `Error::Redeclared`, naming it.
2. **The same declaration is the same terms.** A definition is the environment's when its type, its body and its universe context equal the entry `Globals` holds for its name — a group member's body being its folded selection, as `Globals::of` records it. A registry entry is the environment's when it equals the entry of its own registry. Elaboration's stamps take no part in a definition's comparison, as `Globals` holds none.
3. **A namespace answers for itself.** An item is covered by definition entries alone, a registry entry by its own registry alone. A name the environment holds elsewhere and not there is a refusal.
4. **A group is covered whole or judged whole.** A group some of whose names are in scope and some not is refused: judging it would overwrite the names in scope.
5. **Mounting holds every namespace to one rule.** `Globals::mount` asserts for a registry entry and a concept name what it asserts for a definition: a driver that mounts one name twice is a construction bug, in any namespace.

## Stages

1. **The witness.** The module under *The gap* is put to the kernel. Admitted, it is an open ticket in `xboard/src/board/admission.rs`, committed alone; refused, the rule that refuses it is recorded in the board entry and no ticket is filed.
2. **The gate confirms.** `verdicts_within` compares a covered item and a covered registry entry with the environment's, per decisions 1 to 4, and `Globals::mount` asserts per decision 5. The two scope tests turn to refusals under names that say so, the ticket turns to fixed, and a third test holds the admitting half: an item equal to the environment's is passed over and the module certified.

## Verification

- `recheck::scope_tests`: a definition and a registry entry differing from the environment's under one name are each refused as `Redeclared`; an equal one is passed over; a definition whose name the environment holds only as a registry key is refused. Each is mutation-checked against the comparison answering `true`.
- `curios-pipeline/src/tests/incremental_tests.rs` passes unchanged: every reused item of every fixture is confirmed, none refused.
- `curios-pipeline`'s `std_recompile_closure_census` is retaken for the leaf edit and the hub, and the kernel's phase is named beside the figure it had: the comparison's cost on the standard library, where every reused item is one pointer test per term.
- `cargo xboard` and `cargo xtask clippy`, whose prelude build certifies all of `/std`.

## Rejected

- **Refusing every name in scope, the kernel handed the closure alone**, which is Lean's and Rocq's shape. The module that goes on to erasure is then one the kernel never saw whole, and that its reused items are the mounted ones is again held outside the trusted base. It is the shape one environment reaches, where a verdict is a cell of its declaration and no module is handed over.
- **The kernel checking mount disjointness itself.** It says nothing of a recompile, whose reused items lie under the unit's own mounts, and it brings mounts into `Globals`, which holds names.
- **Comparing digests the unit carries.** A digest the kernel did not compute is an input it would believe.
- **Leaving the premise where it is.** It is discharged by three crates the trusted base does not include, one of them in a debug build only.

## Completion and retirement

Done when no declaration is passed over unconfirmed. [Judging only what is not in scope](../../design/soundness/admission/judging-only-what-is-not-in-scope.md) is restated in the same change: it assumes only that the environment's entries were judged by the walk that built them, which is [Cached verdicts](../../design/soundness/admission/cached-verdicts.md)' premise, and its evidence is the tests above. `recheck_over`'s and `Globals`' documentation say what is compared. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
