//! Strict positivity through a real checker: a negative or strict self-occurrence, a payload the driver cannot read refused, and the polarity vectors recomputed rather than believed.

use {
    super::test_support::{kernel, single_payload},
    curios_analysis::{
        Declarations, PositivityRefusal, positivity_vectors, struct_reaches_itself,
        test_support::SYNTAX,
    },
    curios_cert::Kernel,
    curios_core::{
        Free, Global, InductDecl, Intrinsic, Level, Nat, Polarity, StructDecl, StructType, Subterm,
        Telescope, Term, UniverseContext, Universes,
    },
    curios_utilities::Qualifier,
    std::collections::BTreeMap,
};

/// The four-line route to `False`, refused by the kernel running the shared analysis: `Bad`'s constructor takes `(Bad) -> False`, a negative self-occurrence.
#[test]
fn a_negative_self_occurrence_is_refused() {
    let mut kernel = kernel();

    let false_name = Global::Authored(Qualifier::from(["False"]));
    let bad_name = Global::Authored(Qualifier::from(["Bad"]));
    let false_type = Term::induct_type(false_name, Vec::<Term>::new(), Vec::<Term>::new());
    let bad_type = Term::induct_type(bad_name, Vec::<Term>::new(), Vec::<Term>::new());

    let mut inducts = BTreeMap::new();
    inducts.insert(
        false_name,
        InductDecl {
            constructors: Vec::new().into(),
            ..single_payload(Term::type_ground(), Term::prop())
        },
    );
    inducts.insert(
        bad_name,
        single_payload(
            Term::func_type([(Free::local(1, Some("x")), bad_type)], false_type),
            Term::type_ground(),
        ),
    );
    for (name, entry) in &inducts {
        kernel
            .declare_induct(name, entry)
            .expect("a name is declared once");
    }

    let refusal = positivity_vectors(&mut kernel, Declarations::of(&inducts, &BTreeMap::new()))
        .expect_err("a negative self-occurrence must be refused");
    assert!(
        matches!(&refusal, PositivityRefusal::NotPositive(refusal) if refusal.name == bad_name),
        "refused by the rule, at `Bad`: {refusal:?}",
    );
}

/// The same route to `False` behind an alias the driver cannot unfold: `Bad`'s constructor takes `D`, a definition standing for `(Bad) -> False`, and the kernel is given no budget to unfold it. A refused reduction has to leave the analysis in the refusing direction — the walk follows what the name defines at `Mixed` — where a bare name recording nothing would admit the declaration outright. The refusal is the budget's: the alias was never read, so the analysis has no verdict of its own to give.
#[test]
fn a_payload_type_the_driver_cannot_reduce_is_refused_not_admitted() {
    let mut kernel = Kernel::new(0, SYNTAX);

    let false_name = Global::Authored(Qualifier::from(["False"]));
    let bad_name = Global::Authored(Qualifier::from(["Bad"]));
    let false_type = Term::induct_type(false_name, Vec::<Term>::new(), Vec::<Term>::new());
    let bad_type = Term::induct_type(bad_name, Vec::<Term>::new(), Vec::<Term>::new());

    let alias = Free::local(1, Some("D"));
    kernel
        .define(
            &alias,
            &Term::type_ground(),
            &Term::func_type([(Free::local(2, Some("x")), bad_type)], false_type),
            &UniverseContext::default(),
        )
        .expect("a name is declared once");

    let mut inducts = BTreeMap::new();
    inducts.insert(
        false_name,
        InductDecl {
            constructors: Vec::new().into(),
            ..single_payload(Term::type_ground(), Term::prop())
        },
    );
    inducts.insert(
        bad_name,
        single_payload(Term::free_var(&alias), Term::type_ground()),
    );
    for (name, entry) in &inducts {
        kernel
            .declare_induct(name, entry)
            .expect("a name is declared once");
    }

    let refusal = positivity_vectors(&mut kernel, Declarations::of(&inducts, &BTreeMap::new()))
        .expect_err("an alias the driver cannot unfold is followed, not admitted");
    assert!(
        matches!(&refusal, PositivityRefusal::Exhausted { name, .. } if *name == bad_name),
        "refused for the budget `Bad`'s walk ran out of: {refusal:?}",
    );
}

/// The same route to `False` behind an alias named as a universe instance: `Bad`'s constructor takes `D@{0}`, an instance of a definition generic in one level whose value is `(Bad) -> False`. An instance is a variable head the levels spell, which the walk records nothing at unless it forces the instance to the value it stands for, as it forces a bare name; the instance of a parameter is the occurrence itself (the board's `A parameter named through an instance is an occurrence positivity does not see`), and the instance of a definition is its value. With the budget to unfold it the set is refused by the rule, at `Bad`.
///
/// Mutation-checked against `forceable` declining an `Instance`: the instance then falls to the walk's variable arm, names no parameter, and `Bad` is admitted.
#[test]
fn a_payload_behind_a_universe_instance_of_an_alias_is_refused_by_the_rule() {
    let mut kernel = kernel();

    let false_name = Global::Authored(Qualifier::from(["False"]));
    let bad_name = Global::Authored(Qualifier::from(["Bad"]));
    let false_type = Term::induct_type(false_name, Vec::<Term>::new(), Vec::<Term>::new());
    let bad_type = Term::induct_type(bad_name, Vec::<Term>::new(), Vec::<Term>::new());

    let alias = Free::local(1, Some("D"));
    kernel
        .define(
            &alias,
            &Term::type_ground(),
            &Term::func_type([(Free::local(2, Some("x")), bad_type)], false_type),
            &UniverseContext {
                parameter_count: 1,
                ..UniverseContext::default()
            },
        )
        .expect("a name is declared once");

    let mut inducts = BTreeMap::new();
    inducts.insert(
        false_name,
        InductDecl {
            constructors: Vec::new().into(),
            ..single_payload(Term::type_ground(), Term::prop())
        },
    );
    inducts.insert(
        bad_name,
        single_payload(
            Term::instance_of(&alias, vec![Level::zero()]),
            Term::type_ground(),
        ),
    );
    for (name, entry) in &inducts {
        kernel
            .declare_induct(name, entry)
            .expect("a name is declared once");
    }

    let refusal = positivity_vectors(&mut kernel, Declarations::of(&inducts, &BTreeMap::new()))
        .expect_err("an alias named as a universe instance is read through, not admitted");
    assert!(
        matches!(&refusal, PositivityRefusal::NotPositive(refusal) if refusal.name == bad_name),
        "refused by the rule, at `Bad`: {refusal:?}",
    );
}

/// An alias standing for the declaration itself is a strict occurrence, and with no budget to unfold it the set is still refused — the alias is read at `Mixed`, a non-strict path back to `Good` — but for the budget, which is all the verdict rests on. With the budget to unfold it the same set is admitted, and that control is what says the refusal was the budget's.
///
/// Reporting it as not strictly positive would have a kernel out of budget blame a sound declaration, as it would `/std/Toml/Toml`, whose recursion reaches the walk through `Map`.
#[test]
fn a_strict_payload_the_driver_cannot_reduce_is_refused_for_the_budget() {
    let good_name = Global::Authored(Qualifier::from(["Good"]));

    for budget in [0, 100_000] {
        let mut kernel = Kernel::new(budget, SYNTAX);

        let alias = Free::local(1, Some("D"));
        kernel
            .define(
                &alias,
                &Term::type_ground(),
                &Term::induct_type(good_name, Vec::<Term>::new(), Vec::<Term>::new()),
                &UniverseContext::default(),
            )
            .expect("a name is declared once");

        let mut inducts = BTreeMap::new();
        inducts.insert(
            good_name,
            single_payload(Term::free_var(&alias), Term::type_ground()),
        );
        for (name, entry) in &inducts {
            kernel
                .declare_induct(name, entry)
                .expect("a name is declared once");
        }

        let verdict = positivity_vectors(&mut kernel, Declarations::of(&inducts, &BTreeMap::new()));
        match budget {
            0 => assert!(
                matches!(&verdict, Err(PositivityRefusal::Exhausted { name, .. }) if *name == good_name),
                "with no budget, refused for the budget rather than by the rule: {verdict:?}",
            ),
            _ => assert!(
                verdict.is_ok(),
                "with the budget to unfold the alias, `Good` is strictly positive: {verdict:?}",
            ),
        }
    }
}

/// A set the driver cannot finish reading is refused for the budget though nothing read reaches back: `Wrap`'s constructor takes `D`, an alias of `Nat` the kernel has no budget to unfold, so the walk follows the alias at `Mixed` and finds no path to `Wrap`. Accepted, the set would file a vector computed past that reading, and one of a parameter standing under the alias holds a `Mixed` no declaration put there, which a family declared later and stored in that parameter is refused by with no word of the budget.
#[test]
fn a_set_the_driver_cannot_finish_reading_is_refused_for_the_budget() {
    let wrap_name = Global::Authored(Qualifier::from(["Wrap"]));

    for budget in [0, 100_000] {
        let mut kernel = Kernel::new(budget, SYNTAX);

        let alias = Free::local(1, Some("D"));
        kernel
            .define(
                &alias,
                &Term::type_ground(),
                &Term::intrinsic(Intrinsic::NatType),
                &UniverseContext::default(),
            )
            .expect("a name is declared once");

        let mut inducts = BTreeMap::new();
        inducts.insert(
            wrap_name,
            single_payload(Term::free_var(&alias), Term::type_ground()),
        );
        for (name, entry) in &inducts {
            kernel
                .declare_induct(name, entry)
                .expect("a name is declared once");
        }

        let verdict = positivity_vectors(&mut kernel, Declarations::of(&inducts, &BTreeMap::new()));
        match budget {
            0 => assert!(
                matches!(&verdict, Err(PositivityRefusal::Exhausted { name, .. }) if *name == wrap_name),
                "with no budget, the set is refused for it and files no vector: {verdict:?}",
            ),
            _ => assert_eq!(
                verdict
                    .ok()
                    .and_then(|vectors| vectors.get(&wrap_name).cloned()),
                Some(Vec::new()),
                "with the budget to unfold the alias, `Wrap` is accepted",
            ),
        }
    }
}

/// A strictly positive self-occurrence — the payload *is* the family — is the ordinary recursive datatype and is admitted.
#[test]
fn a_strict_self_occurrence_is_admitted() {
    let mut kernel = kernel();

    let name = Global::Authored(Qualifier::from(["Chain"]));
    let self_type = Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new());
    let mut inducts = BTreeMap::new();
    inducts.insert(name, single_payload(self_type, Term::type_ground()));
    for (entry_name, entry) in &inducts {
        kernel
            .declare_induct(entry_name, entry)
            .expect("a name is declared once");
    }

    let vectors = positivity_vectors(&mut kernel, Declarations::of(&inducts, &BTreeMap::new()))
        .expect("a strictly positive declaration is admitted");
    assert_eq!(vectors.get(&name), Some(&Vec::new()));
}

/// Declare `struct <path>(n: Nat)` with the fields given.
fn declare_struct_over_a_number(kernel: &mut Kernel, path: &str, fields: Vec<Term>) -> Global {
    let name = Global::Authored(Qualifier::from([path]));
    let fields = fields
        .into_iter()
        .zip(1..)
        .map(|(field, index)| (Free::local(index, None), field))
        .collect::<Vec<_>>();

    kernel
        .declare_struct(
            &name,
            &StructDecl {
                universe_context: UniverseContext::default(),
                arity: Telescope::build(
                    [(
                        Free::local(0, Some("n")),
                        Term::intrinsic(Intrinsic::NatType),
                    )],
                    Telescope::build(fields, ()),
                ),
                result_sort: Term::type_ground(),
                module: Qualifier::from([path]),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )
        .expect("a name is declared once");

    name
}

/// A struct reaches itself where its declaration names it — by a field of its own, in a parameter of another struct, or through another struct's fields, and whether the name is spelled as the type former's normal form or as the global that unfolds to it. One that names no struct does not, and neither does one that names only a struct that reaches itself: the question is the diagonal, and what conversion asks it for is a struct met again.
///
/// Mutation-checked: read for its own name alone, a struct that reaches itself through another's fields is missed; read for its free variables alone, one spelled as a normal form is; and read for its constructions alone, one spelled as a global is.
#[test]
fn a_struct_reaches_itself_where_its_declaration_names_it_through_whatever_structs() {
    let mut kernel = kernel();
    let named = |path: &str| Global::Authored(Qualifier::from([path]));
    let at = |path: &str, param: Term| {
        Term::from(Subterm::StructType(StructType {
            name: named(path),
            universes: Universes::default(),
            params: vec![param],
        }))
    };
    let zero = || Term::intrinsic(Intrinsic::Nat(Nat::new(0u64)));
    let spelled = |path: &str| Term::apply(Term::free_var(&Free::Global(named(path))), [zero()]);

    declare_struct_over_a_number(&mut kernel, "Plain", vec![Term::tuple_type_unit()]);
    declare_struct_over_a_number(&mut kernel, "Own", vec![at("Own", zero())]);
    declare_struct_over_a_number(&mut kernel, "Spelled", vec![spelled("Spelled")]);
    declare_struct_over_a_number(
        &mut kernel,
        "Nested",
        vec![at("Plain", at("Nested", zero()))],
    );
    declare_struct_over_a_number(&mut kernel, "Left", vec![at("Right", zero())]);
    declare_struct_over_a_number(&mut kernel, "Right", vec![at("Left", zero())]);
    declare_struct_over_a_number(&mut kernel, "Beside", vec![at("Own", zero())]);

    assert_eq!(
        ["Own", "Spelled", "Nested", "Left", "Right"]
            .map(|path| struct_reaches_itself(&kernel, &named(path))),
        [true, true, true, true, true]
    );
    assert_eq!(
        ["Plain", "Beside", "Undeclared"].map(|path| struct_reaches_itself(&kernel, &named(path))),
        [false, false, false]
    );
}

/// A declaration's carried polarity vector is not evidence.
///
/// `InductDecl::polarities` is computed by `curios-elab` after elaboration and rides the prelude archive, so believing it would make this crate's positivity check a restatement of the other's. Here the vector *lies* — it claims the parameter is used strictly while the constructor stores a function out of it, which is the negative occurrence Cantor forbids — and the analysis must reach its own verdict from the telescopes regardless.
#[test]
fn a_carried_polarity_vector_is_recomputed_rather_than_believed() {
    let mut kernel = kernel();

    let false_name = Global::Authored(Qualifier::from(["False"]));
    let bad_name = Global::Authored(Qualifier::from(["Bad"]));
    let false_type = Term::induct_type(false_name, Vec::<Term>::new(), Vec::<Term>::new());
    let bad_type = Term::induct_type(bad_name, Vec::<Term>::new(), Vec::<Term>::new());

    let mut inducts = BTreeMap::new();
    inducts.insert(
        false_name,
        InductDecl {
            constructors: Vec::new().into(),
            ..single_payload(Term::type_ground(), Term::prop())
        },
    );
    inducts.insert(
        bad_name,
        InductDecl {
            // The lie: every parameter claimed strictly positive, while the payload below is a function *out of* the family.
            polarities: vec![Polarity::Strict],
            variances: Vec::new(),
            ..single_payload(
                Term::func_type([(Free::local(0, Some("f")), bad_type)], false_type),
                Term::type_ground(),
            )
        },
    );

    for (name, entry) in &inducts {
        kernel
            .declare_induct(name, entry)
            .expect("a name is declared once");
    }

    assert!(
        positivity_vectors(&mut kernel, Declarations::of(&inducts, &BTreeMap::new()),).is_err(),
        "the carried vector was believed instead of recomputed",
    );
}

/// The same claim at the branch that actually decides it, which the fixture above does not reach: there `Bad` is *inside* the analyzed set, so its polarity comes from the fixpoint and the carried vector is never consulted by any rule. A family *outside* the set answers from the entry the kernel holds, and the kernel holding no vector it did not compute is the whole of its not inheriting an elaborator-computed one.
///
/// So `Wrapper` is declared to the kernel and withheld from the analyzed map, and the vector its entry carries in lies — `Strict` in a parameter its constructor would use negatively. `Outer` stores a `Wrapper(Outer)`. The kernel keeps an entry it is handed without the vector, which is elaboration's, so the lookup answers `Mixed`, `Outer` reaches itself at `Mixed`, and the set is refused.
///
/// A kernel that kept the stamp with the entry would admit `Outer` on it, and pass every test that analyzes a family with the families it mentions, since those resolve from the computed map.
#[test]
fn a_vector_an_entry_is_handed_in_with_is_not_the_kernels() {
    let mut kernel = kernel();

    let wrapper_name = Global::Authored(Qualifier::from(["Wrapper"]));
    let outer_name = Global::Authored(Qualifier::from(["Outer"]));
    let outer_type = Term::induct_type(outer_name, Vec::<Term>::new(), Vec::<Term>::new());

    // Registered, never analyzed: the lie rides on `polarities` and only a registry lookup can read it.
    kernel
        .declare_induct(
            &wrapper_name,
            &InductDecl {
                arity: Telescope::build(
                    [(Free::local(2, Some("A")), Term::type_ground())],
                    Telescope::done(()),
                ),
                constructors: Vec::new().into(),
                polarities: vec![Polarity::Strict],
                variances: Vec::new(),
                ..single_payload(Term::type_ground(), Term::type_ground())
            },
        )
        .expect("a name is declared once");

    let mut inducts = BTreeMap::new();
    inducts.insert(
        outer_name,
        single_payload(
            Term::induct_type(wrapper_name, [outer_type], Vec::<Term>::new()),
            Term::type_ground(),
        ),
    );
    for (name, entry) in &inducts {
        kernel
            .declare_induct(name, entry)
            .expect("a name is declared once");
    }

    assert!(
        positivity_vectors(&mut kernel, Declarations::of(&inducts, &BTreeMap::new()),).is_err(),
        "a family the kernel has filed no vector for must read `Mixed`, not the vector its entry was handed in with",
    );
}
