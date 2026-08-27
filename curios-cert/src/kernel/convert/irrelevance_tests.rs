//! Definitional proof irrelevance: where it fires, where it must not leak, and what a binder's stand-in type decides.

use super::test_support::*;
use {
    crate::{KernelError, convert},
    curios_core::{Free, Intrinsic, Level, Term},
};

/// Proof irrelevance: at a `Prop`-sorted type any two terms convert, *without* either being examined. This is what licenses erasure to drop proofs wholesale.
#[test]
fn any_two_inhabitants_of_a_proposition_convert() {
    let mut kernel = kernel();
    let proposition = declare(&mut kernel, "P", Term::prop());
    let (left, right) = (binder(0, "p"), binder(1, "q"));

    assert_eq!(
        convert(
            &mut kernel,
            &proposition,
            &Term::free_var(&left),
            &Term::free_var(&right),
        ),
        Ok(true),
    );
}

/// The same two terms at a *relevant* type are not interchangeable. Irrelevance is a property of the type, and this is the direction that would be unsound to get wrong.
#[test]
fn does_not_leak_into_a_relevant_type() {
    let mut kernel = kernel();
    let data = declare(&mut kernel, "D", Term::type_ground());
    let (left, right) = (binder(0, "p"), binder(1, "q"));

    assert_eq!(
        convert(
            &mut kernel,
            &data,
            &Term::free_var(&left),
            &Term::free_var(&right),
        ),
        Ok(false),
    );
}

/// Definitional proof irrelevance at a *computed* proposition — the shape every real firing takes, and the one this crate's copy had never been tested at.
///
/// `any_two_inhabitants_of_a_proposition_convert` above uses a nominal `Prop`-sorted family, which is the easy rung: the registry says outright that the type is a proposition. Every one of the 37 firings measured in `curios-elab` across the whole prelude is at a type that only *computes* to one — a stuck `match` at motive `Prop`, which is how `/std/BigNat` states a validity predicate over a `Bool`. Here the rule is inert, 0 firings in 86,547 goals, because a proof reaching conversion in this crate does so in an untyped child position compared at `Type`; so nothing in any program brings the rule and the shape together, and only a fixture can.
///
/// The shape is worth its own fixture because of what irrelevance trusts. It accepts *without inspecting either term*, and `Sort::of` classifies a stuck `match` by its **motive** rather than by its arms — a claim the term makes about itself, which is exactly why `check_motive` exists to type a motive under its real binders before any rule reads it. At a computed proposition the motive is therefore the whole of what this rule rests on.
///
/// The control is the identical construction with the motive at `Type` and relevant arms in both, which must *not* converge. It separates "reads the motive" from "accepts every stuck match", and without it a rule that skipped the sort test entirely would satisfy the witness.
#[test]
fn fires_at_a_computed_proposition() {
    let mut kernel = kernel();
    let held = declare(&mut kernel, "Held", Term::prop());
    let empty = declare(&mut kernel, "Empty", Term::prop());

    let scrutinee = binder(40, "b");
    kernel.assume(&scrutinee, &Term::intrinsic(Intrinsic::BoolType));

    let computed = Term::bool_match(Term::free_var(&scrutinee), None, Term::prop(), empty, held);

    let (left, right) = (binder(41, "p"), binder(42, "q"));
    kernel.assume(&left, &computed);
    kernel.assume(&right, &computed);

    assert_eq!(
        convert(
            &mut kernel,
            &computed,
            &Term::free_var(&left),
            &Term::free_var(&right),
        ),
        Ok(true),
        "two inhabitants of a proposition the motive computes did not convert",
    );
}

/// The control for the fixture above: the same stuck `match`, with the motive at `Type` and relevant arms, must still distinguish its inhabitants.
#[test]
fn does_not_fire_at_a_computed_relevant_type() {
    let mut kernel = kernel();
    let one = declare(&mut kernel, "One", Term::type_ground());
    let two = declare(&mut kernel, "Two", Term::type_ground());

    let scrutinee = binder(50, "b");
    kernel.assume(&scrutinee, &Term::intrinsic(Intrinsic::BoolType));

    let computed = Term::bool_match(
        Term::free_var(&scrutinee),
        None,
        Term::type_ground(),
        one,
        two,
    );

    let (left, right) = (binder(51, "x"), binder(52, "y"));
    kernel.assume(&left, &computed);
    kernel.assume(&right, &computed);

    assert_eq!(
        convert(
            &mut kernel,
            &computed,
            &Term::free_var(&left),
            &Term::free_var(&right),
        ),
        Ok(false),
        "irrelevance leaked into a relevant type the motive computes",
    );
}

/// **The stand-in `ground_scope` opens its binders at, held against the types those binders really carry.**
///
/// `ground_scope` opens both scopes at one shared set of binders and assumes every one of them at `Type`, whatever it really is. Its own comment licenses that by an inventory — "a binder's recorded type feeds only the conversion history's context key, identically on both sides" — and the inventory is false. [`synth_neutral`](super::super::sort::synth_neutral) reads the same recorded type through `Kernel::type_of`, so it reaches `Sort::of`, and `Sort::of` is what [`compare`] asks before *every* goal: the proof-irrelevance test at the top of the rule.
///
/// What actually holds the stand-in up is narrower, and is about the value rather than about the readers: `Type` is the least informative answer `Sort::of` can return for a binder. Irrelevance fires on `Sort::Prop` and on nothing else, and eta dispatches on the goal type's own *shape* rather than on the binder's, so a binder recorded at `Type` can only lose the accepting rules, never gain one. This walks one goal at each type the binder could really carry and records what each decides.
///
/// The grid is two side-pairs against four assumed types, because a single pair cannot separate the two things being asked. Distinct sides expose which types *discharge* the goal without comparing — only `Prop` does — and convertible-but-not-identical sides expose which types get as far as comparing at all. The stand-in's row matches the relevant-sort row in both, which is the null: it decides every goal the way a real relevant type decides it.
///
/// **One row is not a forfeiture, and it is the one to carry forward.** A binder whose real type is not a sort at all leaves `Sort::of` with nothing to decode, and the typed opening refuses the whole certification with `NotASort` — while the stand-in classifies it `Type 0` and goes on to accept. There the stand-in is strictly *more* permissive than the truth. Nothing in `ground_scope` fences that off; what does is a property of its callers, the same shape as the one `struct_eta`'s neutral restriction turned out to rest on. A match motive is typed under its real binders by `infer`'s `check_motive` before any comparison grounds it, so a motive using a `Bool`-typed binder as a type never reaches here. That is written in neither place, and it is what this row exists to record.
#[test]
fn a_binders_stand_in_type_decides_a_goal_the_way_a_relevant_type_does() {
    let distinct = || {
        (
            Term::free_var(&binder(70, "u")),
            Term::free_var(&binder(71, "v")),
        )
    };

    // Convertible without being syntactically equal, so the goal survives `compare`'s reflexivity fast path and has to be decided by a rule.
    let convertible = || {
        let x = binder(72, "x");

        (
            Term::apply(
                Term::func([(x.clone(), nat_type())], Term::free_var(&x)),
                [nat(1)],
            ),
            nat(1),
        )
    };

    let at = |assumed: Term, sides: (Term, Term)| {
        let mut kernel = kernel();
        let hypothesis = binder(73, "h");
        kernel.assume(&hypothesis, &assumed);

        convert(
            &mut kernel,
            &Term::free_var(&hypothesis),
            &sides.0,
            &sides.1,
        )
    };

    let relevant = Term::type_at(Level::constant(3));
    let stand_in = Term::type_ground();
    let not_a_sort = || Err(KernelError::NotASort(nat_type()));

    // Distinct sides: only a proposition discharges them, and the stand-in is not one.
    assert_eq!(
        at(Term::prop(), distinct()),
        Ok(true),
        "a hypothesis really at `Prop` stopped licensing irrelevance",
    );
    assert_eq!(
        at(relevant.clone(), distinct()),
        Ok(false),
        "a hypothesis at a relevant sort discharged two distinct inhabitants",
    );
    assert_eq!(
        at(stand_in.clone(), distinct()),
        Ok(false),
        "the stand-in discharged a goal a relevant type refuses",
    );
    assert_eq!(
        at(nat_type(), distinct()),
        not_a_sort(),
        "a hypothesis at a non-sort was classified rather than refused",
    );

    // Convertible sides: every sort compares and accepts, and the non-sort still refuses before comparing.
    assert_eq!(
        at(Term::prop(), convertible()),
        Ok(true),
        "a proposition stopped discharging its inhabitants",
    );
    assert_eq!(
        at(relevant, convertible()),
        Ok(true),
        "a relevant sort refused two convertible terms",
    );
    assert_eq!(
        at(stand_in, convertible()),
        Ok(true),
        "the stand-in refused a goal a relevant type accepts",
    );
    assert_eq!(
        at(nat_type(), convertible()),
        not_a_sort(),
        "the non-sort row stopped being the one place the stand-in is the more permissive of the two",
    );
}

/// The same stand-in reached through [`ground_scope`] itself rather than through an assumption written by hand.
///
/// Two stuck `bool_match`es differing only inside their motive scopes. Each motive body is `Wit(<motive binder>, i)`, and `Wit`'s index type is its own parameter, so the index pair is compared at the motive binder — which `ground_scope` has opened at `Type`. The pair is refused, matching the grid's stand-in row above rather than its `Prop` row, which is what pins that the production path really does record the stand-in and not something the term carries.
///
/// The counterfactual is the second half: assume that same binder at `Prop` and compare the two motive bodies directly, and the goal is discharged by irrelevance. So the verdict does move when the binder's real type differs from the stand-in, and it moves toward refusal — which is the direction this row's **Assumes** claims and the direction the two-checker matrix already records one instance of, in `curios`'s `a_grounded_argument_forfeits_irrelevance`.
///
/// The control between them is a motive pair that differs only by a beta redex. It must still converge through the same `ground_scope`, so the refusal above is `u ≠ v` decided at a relevant sort rather than the grounded scope declining to compare its bodies at all.
#[test]
fn a_grounded_motive_binder_carries_the_stand_in_rather_than_its_real_type() {
    let mut kernel = kernel();
    let wit = declare_indexed(&mut kernel, "Wit", Term::prop());

    let scrutinee = binder(80, "b");
    kernel.assume(&scrutinee, &Term::intrinsic(Intrinsic::BoolType));

    let carried = binder(81, "P");
    let (u, v) = (binder(82, "u"), binder(83, "v"));

    let body = |index: Term| Term::induct_type(wit.clone(), [Term::free_var(&carried)], [index]);
    let elimination = |index: Term| {
        Term::bool_match(
            Term::free_var(&scrutinee),
            Some(&carried),
            body(index),
            nat(0),
            nat(0),
        )
    };

    let redex = |name: &Free| {
        let x = binder(84, "x");

        Term::apply(
            Term::func([(x.clone(), nat_type())], Term::free_var(&x)),
            [Term::free_var(name)],
        )
    };

    assert_eq!(
        convert(
            &mut kernel,
            &Term::type_ground(),
            &elimination(Term::free_var(&u)),
            &elimination(Term::free_var(&v)),
        ),
        Ok(false),
        "a grounded motive binder discharged two distinct index actuals",
    );

    assert_eq!(
        convert(
            &mut kernel,
            &Term::type_ground(),
            &elimination(Term::free_var(&u)),
            &elimination(redex(&u)),
        ),
        Ok(true),
        "the grounded scope stopped comparing its bodies up to reduction",
    );

    kernel.assume(&carried, &Term::prop());

    assert_eq!(
        convert(
            &mut kernel,
            &Term::type_ground(),
            &body(Term::free_var(&u)),
            &body(Term::free_var(&v)),
        ),
        Ok(true),
        "the same binder at its real `Prop` stopped licensing the irrelevance grounding forfeits",
    );
}
