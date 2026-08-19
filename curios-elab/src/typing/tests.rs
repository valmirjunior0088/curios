use curios_core::*;
use {
    crate::*,
    curios_cert::{Kernel, carries_information},
    curios_utilities::Qualifier,
};

#[test]
fn display_unbound_variable() {
    let err = Error::unbound_variable(Subterm::Intrinsic(Intrinsic::NatType));
    assert_eq!(err.to_string(), "unbound variable: Nat");
}

#[test]
fn display_not_a_function() {
    let err = Error::not_a_function(Subterm::Intrinsic(Intrinsic::NatType));
    assert_eq!(
        err.to_string(),
        "applied a non-function\n  head has type: Nat"
    );
}

#[test]
fn display_type_mismatch_shows_both_types() {
    let err = Error::type_mismatch(
        Subterm::Intrinsic(Intrinsic::NatType),
        Subterm::Intrinsic(Intrinsic::BoolType),
    );
    let s = err.to_string();
    assert!(s.contains("Nat"), "should contain inferred Nat: {s}");
    assert!(s.contains("Bool"), "should contain expected Bool: {s}");
}

// A witness for a parameterless concept once reused `InvalidWitnessHead` at a fabricated position 0, telling the user "parameter 1" of a concept that has none; the dedicated variant states the actual rule — key on parameter heads, or supply the concept through a local `use` binder.
#[test]
fn display_parameterless_witness_concept_states_the_rule() {
    let err = Error::parameterless_witness_concept("w", "Nothing");
    assert_eq!(
        err.to_string(),
        "witness 'w' cannot be registered: concept 'Nothing' has no parameters to key on\n  a global witness keys on its concept's parameter heads; supply a parameterless concept through a local 'use' binder instead"
    );
}

/// The predicate deciding what a `Prop` may carry is written twice, and this is where the two are put to each other.
///
/// [`is_prop`] is this side's answer and `curios_cert::carries_information` is the kernel's. The large-elimination guard's singleton condition and the `Prop`-field rule both turn on one or the other, with the shared `pinned_by_targets` walk supplying the rest of each. The two disagreed once, in the direction that matters: the kernel additionally exempted a position whose *type is a universe* — a position holding a type — reasoning that erasure deletes a type either way, and a closed inhabitant of `False` followed. `curios-cert/src/recheck/tests.rs` holds that derivation.
///
/// No compile could have observed the disagreement. This side refuses such a declaration during elaboration, so the kernel's copy is asked only about declarations that already passed here — which is what `Expect::NotAsked` records for `informative_prop_field` and `multi_constructor_prop` in `curios/src/tests/perimeter.rs`, a cell that file's own documentation calls "not a pass". So the comparison has to be made directly, as `curios-cert`'s `satisfy` module now does for universe contexts.
///
/// Both checkers are told the same single thing — one nullary `Prop`-sorted family — and nothing else, so a disagreement here is about the rule rather than about what either was handed. That declaration is unavoidable rather than convenient: `Prop` is itself `Type`-sorted, so no closed proposition can be built out of intrinsics and type formers alone. The first draft of this table had no declaration and therefore compared eleven *informative* types against each other, agreeing perfectly while testing nothing; the control below is what caught it, and that is what a control is for.
///
/// The table spans the shapes the rule turns on: both universes, a proof, an impredicative `Π` into a proposition beside a `Π` into a *universe* — a type family, not a proposition — a `Σ` whose fields are all propositions, and the unit type, which is `Type`-sorted deliberately, being what an effect returns, so calling it a proposition would erase it.
#[test]
fn both_checkers_decide_non_informativeness_alike() {
    let mut context = Context::new(100_000, crate::SYNTAX);
    let mut kernel = Kernel::new(100_000, crate::fixture::SYNTAX);
    kernel.set_local_floor(1_000);

    let nat = || Term::intrinsic(Intrinsic::NatType);
    let binder = |index: u32| Free::local(index, Some("x"));

    // A `Prop`-sorted family has to be declared: `Prop` itself is `Type`-sorted, so no closed proposition can be built out of intrinsics and type formers alone. Registering it in both checkers is the whole of the setup either one needs.
    let held = Global::Authored(Qualifier::from(["Held"]));
    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: Vec::new(),
        result_sort: Term::prop(),
        module: Qualifier::from(["Held"]),
        rep_public: true,
        polarities: Vec::new(),
    };
    context
        .register_induct(&held, declaration.clone())
        .expect("the family registers");
    kernel.declare_induct(&held, &declaration);
    let proof = || Term::induct_type(held.clone(), Vec::<Term>::new(), Vec::<Term>::new());

    let types = vec![
        ("Nat", nat()),
        ("Bool", Term::intrinsic(Intrinsic::BoolType)),
        ("List(Nat)", Term::intrinsic(Intrinsic::ListType(nat()))),
        // The position that diverged: a term at `Type` is a type, and conversion reads it back in full.
        ("Type 0", Term::type_ground()),
        (
            "Type 1",
            Term::type_at(Level::zero().succ().expect("level zero has a successor")),
        ),
        // The other half of the clause that admitted the first.
        ("Prop", Term::prop()),
        ("the unit type", Term::tuple_type_unit()),
        // A proof, which is the one thing that genuinely carries nothing.
        ("a proposition", proof()),
        // `Π` into a proposition is a proposition whatever it quantifies over — impredicativity, and what makes `(n : Nat) -> P(n)` erasable.
        (
            "(Nat) -> a proposition",
            Term::func_type([(binder(0), nat())], proof()),
        ),
        // `Π` into a *universe* is a type family, not a proposition: `Prop` is itself `Type`-sorted.
        (
            "(Nat) -> Prop",
            Term::func_type([(binder(1), nat())], Term::prop()),
        ),
        (
            "(Nat) -> Type 0",
            Term::func_type([(binder(2), nat())], Term::type_ground()),
        ),
        (
            "a record of propositions",
            Term::tuple_type([(binder(3), proof()), (binder(4), proof())]),
        ),
        (
            "a record mixing a proposition with data",
            Term::tuple_type([(binder(5), proof()), (binder(6), nat())]),
        ),
    ];

    let mut verdicts = Vec::new();
    for (label, type_) in &types {
        let elaborator = is_prop(&mut context, type_).expect("the elaborator classifies it");
        let kernel_says =
            !carries_information(&mut kernel, type_).expect("the kernel classifies it");

        assert_eq!(
            elaborator,
            kernel_says,
            "the two checkers disagree about `{label}`: the elaborator calls it {}, the kernel calls it {}",
            match elaborator {
                true => "non-informative",
                false => "informative",
            },
            match kernel_says {
                true => "non-informative",
                false => "informative",
            },
        );
        verdicts.push(elaborator);
    }

    // The control: a table that had drifted to all-informative, or all-non-informative, would pass this while comparing nothing.
    assert!(
        verdicts.contains(&true),
        "no type in the table is non-informative",
    );
    assert!(
        verdicts.contains(&false),
        "no type in the table is informative",
    );
}
