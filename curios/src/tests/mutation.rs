//! Differential mutation of an elaborated module: the kernel must refuse a body it has no reason to accept.
//!
//! Every other soundness fixture states a shape someone thought to write down. This states a *property* instead — replace an item's body with a term of a manifestly different type, and the module must be refused — and applies it to every item a program has, including the ones nobody would think to attack.
//!
//! The oracle is what makes this defensible. `curios-elab` cannot be asked about a mutated *Core* module, so a mutant carries no second opinion to compare against; the property therefore has to be one whose answer is known by construction. A definition declared at `Nat` whose body is `true`, or one declared at anything else whose body is `0`, is ill-typed for a reason that needs no checker to establish — so a kernel that accepts it has admitted something false, and one that refuses it has done its job whatever rule it used.

use {
    curios_core::{Item, Module, Term},
    curios_pipeline::recheck,
    curios_text::{Entrypoint, RootSource},
};

/// Programs whose items sit at deliberately varied types — intrinsic, propositional, functional, indexed, and nominal — so the property is exercised against more than one shape of declaration. None declares anything at a sort, which is what makes [`foreign_body`] foreign to all of them.
const SUBJECTS: &[(&str, &str)] = &[
    (
        "values and a proof",
        r#"
        use /std/{Nat, Bool, Eq};

        let count : Nat = 3;
        let flag : Bool = true;
        let same : Eq(1, 1) = Eq/refl();
        let twice(n : Nat) -> Nat = n + n;

        /std/print(Nat/to_str(twice(count)))
        "#,
    ),
    (
        "an indexed family and its inhabitant",
        r#"
        use /std/{Nat, Vec};

        let pair : Vec(Nat, 2) = Vec/cons(1, Vec/cons(2, Vec/nil()));
        let length : Nat = 2;

        /std/print(Nat/to_str(length))
        "#,
    ),
    (
        "a structure and a concept witness",
        r#"
        use /std/{Nat, Str, Show};

        struct Point : pub Type { x : Nat, y : Nat }

        let origin : Point = Point { x = 0, y = 0 };
        let shown : Str = Show/show(origin.x);

        /std/print(shown)
        "#,
    ),
];

/// A term foreign to every *value* type: the sort itself.
///
/// Picking a literal would mean recognizing the declaration it belongs to, and a declared type is rarely the intrinsic it reduces to — `Nat` reaches the kernel as `/sys/Nat/Nat`, so a structural test for `Intrinsic::NatType` misses it and substitutes a `0` that is perfectly well typed. `Type` inhabits no value type at all, which needs no recognition to be sure of; the subject below accordingly declares nothing at a sort.
fn foreign_body() -> Term {
    Term::type_ground()
}

/// Where `module`'s `let` items sit.
///
/// Every item is the entry program's own: the prelude reaches a compilation as scope rather than as items copied in front of it, so there is nothing here to filter out.
fn lets(module: &Module) -> Vec<usize> {
    module
        .items
        .iter()
        .enumerate()
        .filter_map(|(index, item)| match item {
            Item::Let(_) => Some(index),
            Item::Rec(_) => None,
        })
        .collect()
}

/// Every mutant of every subject must be refused, and each unmutated subject accepted.
#[test]
fn every_body_replaced_by_a_foreign_term_is_refused() {
    let mut mutated = 0;

    for (description, source) in SUBJECTS {
        let entrypoint = source
            .parse::<Entrypoint>()
            .unwrap_or_else(|error| panic!("{description}: the subject parses: {error:?}"));
        let (module, obligations) = curios_pipeline::typecheck_reporting(
            crate::DEFAULT_STEP_BUDGET,
            &entrypoint,
            RootSource::none(),
        )
        .unwrap_or_else(|error| panic!("{description}: the subject type-checks:\n{error}"));
        assert!(
            obligations.is_empty(),
            "{description}: the subject carries an erasure obligation, so it is the wrong control",
        );
        assert!(
            recheck(&module, crate::DEFAULT_STEP_BUDGET).is_empty(),
            "{description}: the unmutated subject must be accepted, or every mutant passes for the wrong reason",
        );

        for index in lets(&module) {
            let Item::Let(definition) = &module.items[index] else {
                unreachable!("the index came from a `let`");
            };

            let mut mutant: Module = module.clone();
            let Item::Let(target) = &mut mutant.items[index] else {
                unreachable!("the item was a `let` a moment ago");
            };
            target.body = foreign_body();

            assert!(
                !recheck(&mutant, crate::DEFAULT_STEP_BUDGET).is_empty(),
                "{description}: the kernel accepted `{}` at `{}` with a body of another type entirely",
                definition.name,
                definition.type_,
            );
            mutated += 1;
        }
    }

    assert!(
        mutated >= 8,
        "the subjects stopped exercising the property: only {mutated} items were mutated",
    );
}

/// The same property from the other side: keep each body and give it another item's declared type.
///
/// Body substitution asks whether the kernel notices a term that cannot inhabit its type; this asks whether it notices a *type* nothing in place inhabits. The two fail differently — the first is caught where the body is checked, the second can also surface downstream, where a consumer of the retyped definition stops fitting — and a checker that tracked only one of them would pass the earlier test and fail this one.
///
/// Pairs whose declared types are structurally identical are skipped: swapping those is not a mutation at all.
#[test]
fn every_type_replaced_by_another_item_s_is_refused() {
    let mut mutated = 0;

    for (description, source) in SUBJECTS {
        let entrypoint = source
            .parse::<Entrypoint>()
            .unwrap_or_else(|error| panic!("{description}: the subject parses: {error:?}"));
        let (module, _) = curios_pipeline::typecheck_reporting(
            crate::DEFAULT_STEP_BUDGET,
            &entrypoint,
            RootSource::none(),
        )
        .unwrap_or_else(|error| panic!("{description}: the subject type-checks:\n{error}"));

        let declared = lets(&module)
            .into_iter()
            .map(|index| match &module.items[index] {
                Item::Let(definition) => (index, definition.type_.clone()),
                Item::Rec(_) => unreachable!("the index came from a `let`"),
            })
            .collect::<Vec<_>>();

        for (index, _) in &declared {
            for (other, replacement) in &declared {
                let Item::Let(definition) = &module.items[*index] else {
                    unreachable!("the index came from a `let`");
                };
                if other == index || definition.type_ == *replacement {
                    continue;
                }

                let mut mutant: Module = module.clone();
                let Item::Let(target) = &mut mutant.items[*index] else {
                    unreachable!("the item was a `let` a moment ago");
                };
                target.type_ = replacement.clone();

                assert!(
                    !recheck(&mutant, crate::DEFAULT_STEP_BUDGET).is_empty(),
                    "{description}: the kernel accepted `{}` redeclared at `{replacement}`",
                    definition.name,
                );
                mutated += 1;
            }
        }
    }

    assert!(
        mutated >= 12,
        "the subjects stopped exercising the property: only {mutated} pairs were swapped",
    );
}

/// Programs that differ only in an index, so a body grafted from one into the other is well formed and wrong in exactly one respect.
///
/// The two classes above substitute something obviously foreign — a sort, or an unrelated declaration's type. A checker could refuse both while tracking nothing finer than the shape of a term. These pairs remove that escape: the grafted body is a genuine, well-typed vector, built from the same constructors at the same element type, and the *only* thing wrong with it is its length. Refusing it requires the indices to be carried through the constructor telescope and compared, which is the whole of what a dependent checker is for.
const GRAFTS: &[(&str, &str, &str)] = &[
    (
        "a vector of the wrong length",
        r#"
        use /std/{Nat, Vec};
        let subject : Vec(Nat, 2) = Vec/cons(1, Vec/cons(2, Vec/nil()));
        /std/print("donor")
        "#,
        r#"
        use /std/{Nat, Vec};
        let subject : Vec(Nat, 3) = Vec/cons(1, Vec/cons(2, Vec/cons(3, Vec/nil())));
        /std/print("host")
        "#,
    ),
    (
        "an equation between the wrong pair",
        r#"
        use /std/{Nat, Eq};
        let subject : Eq(1, 1) = Eq/refl();
        /std/print("donor")
        "#,
        r#"
        use /std/{Nat, Eq};
        let subject : Eq(2, 2) = Eq/refl();
        /std/print("host")
        "#,
    ),
];

/// Elaborate `source` and return its module.
fn elaborated(description: &str, source: &str) -> Module {
    let entrypoint = source
        .parse::<Entrypoint>()
        .unwrap_or_else(|error| panic!("{description}: the subject parses: {error:?}"));
    let (module, _) = curios_pipeline::typecheck_reporting(
        crate::DEFAULT_STEP_BUDGET,
        &entrypoint,
        RootSource::none(),
    )
    .unwrap_or_else(|error| panic!("{description}: the subject type-checks:\n{error}"));

    module
}

/// A body grafted from a program that differs only in an index must be refused by the host.
#[test]
fn a_body_grafted_across_an_index_is_refused() {
    for (description, donor_source, host_source) in GRAFTS {
        let donor = elaborated(description, donor_source);
        let host = elaborated(description, host_source);

        let donated = donor
            .items
            .iter()
            .find_map(|item| match item {
                Item::Let(definition) if definition.name.to_string().ends_with("subject") => {
                    Some(definition.body.clone())
                }
                _ => None,
            })
            .unwrap_or_else(|| panic!("{description}: the donor declares `subject`"));

        assert!(
            recheck(&host, crate::DEFAULT_STEP_BUDGET).is_empty(),
            "{description}: the host must be accepted before anything is grafted into it",
        );

        let mut grafted: Module = host.clone();
        let target = grafted
            .items
            .iter_mut()
            .find_map(|item| match item {
                Item::Let(definition) if definition.name.to_string().ends_with("subject") => {
                    Some(definition)
                }
                _ => None,
            })
            .unwrap_or_else(|| panic!("{description}: the host declares `subject`"));
        target.body = donated;

        assert!(
            !recheck(&grafted, crate::DEFAULT_STEP_BUDGET).is_empty(),
            "{description}: the kernel accepted a body whose index disagrees with its declaration",
        );
    }
}
