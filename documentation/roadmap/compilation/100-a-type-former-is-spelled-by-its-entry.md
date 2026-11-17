# A type former is spelled by its entry

[A nominal declaration's entry is held by its type former](../../../curios-core/README.md#a-nominal-declarations-entry-is-held-by-its-type-former) states that a former's type, body, universe context, totality and variance vector are read off its entry and never stored beside it. Each is stored beside it today, nothing holds the two to each other, and the certifier admits a module where they disagree.

**Not refined yet.** The next step is the decision under *Refinement*, between two shapes that each make the disagreement unrepresentable; the stages follow from it.

## What is wrong

A nominal declaration's entry is held by the definition that is its type former, and that definition stores a type, a body, a universe context, a totality and a variance vector beside it. Each is a function of the entry. `Definition::former` (`curios-core/src/definitions.rs`) spells the first three; a former calls nothing, so it is total; and a former that is no member of a group holds each level as its entry does, a member of one storing no vector. `std_former_derivation` (`curios-pipeline/src/standard/tests.rs`, counted at `6a8f997d1`) spells each type former of `/sys` and `/std` from the entry it carries, 82 families, 95 records and 19 concepts, and finds every part of all 196 equal to the one the archive holds.

Each stage pays for the second copy in its own way. The lowering writes a former's type and its entry's arity from one written telescope. Elaboration checks the type, replaces the entry's parameters with the ones the type came out with (`share_struct_params`, `curios-elab/src/elaborate/module.rs`), elaborates the entry's fields or constructors against them, finalizes both under one universe context (`finalize_definition`), and writes the finished entry into the former where the item leaves the walk (`carrying`); the elaborator reads a scheme from whichever of the two is in hand, so their agreement is load-bearing there. The certifier types the former as a definition and checks the entry as a declaration (`check_induct_decl`, `check_struct_decl`, `curios-cert/src/certifier/module.rs`) and reads each scheme where it is authoritative. Two of its tests pin the admission as intended: `recheck::registry_tests::a_registry_and_its_type_former_may_declare_different_schemes` certifies a module whose former declares another scheme than its entry, in both directions, and `recheck::implied_tests::a_former_stated_otherwise_is_read_as_it_is_stated` one whose former is stated as another proposition with a proof. Core text spells each, since it spells every module Core holds: a former's `type_`, `body` and `universe_context` decorators stand beside its `entry = {…}`. No program elaborates to such a module. A family's constructors are no such case, being made from its elaborated entry (`Definition::constructors`), and a concept's methods are its fields, which no definition holds.

## Refinement

- **What a former stores**: its name, its island and its entry, with the type, the body and the context read through the entry where a reader asks, as a group's member is unfolded from the group that holds it (`RecItem`, `curios-core/src/definitions.rs`); or all of them materialized once, from the entry alone, where an item is made or restored, which leaves every reader of a `Definition` as it is. Either keeps the stored and the textual form from stating them; the choice is what a reader pays.
- **What the elaborator elaborates**: the entry once, with the former's signature published from it, so that `share_struct_params`, the entry's half of `finalize_definition`, `carrying` and the agreement the zonk validates (`curios-elab/src/zonk.rs`) have nothing to reconcile; and what a telescope elaborated once does to where a refusal is reported and to the order a declaration's levels settle in.
- **What the certifier judges**: the entry, with a former's type answered from the entry the environment holds, so that a former at another type or scheme is as unrepresentable in a module as in a term.
- **What Core text spells**: a nominal declaration by its entry alone, the former's decorators gone from its grammar, its printer and the tree-sitter grammar of `.crsc`.

## Verification

The stored bytes of every unit holding a former change; every verdict over `/std` and the corpus in `programs/`, every refusal pinned by a test, and the emitted Wasm stay equal. The two certifier tests above go with the forms they wrote, a Core text decorator on a former refused where it is read; `std_former_derivation` goes with the second copy it compares.

## Retirement

Once no `Definition` stores what its entry spells, the decision's crate and `curios-cert`'s README state what the certifier reads of a former, no reference to this filename remains, and it is deleted.
