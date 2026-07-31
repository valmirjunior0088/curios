//! Differential mutation of an elaborated module: the kernel must refuse a body it has no reason to accept.
//!
//! Every other soundness fixture states a shape someone thought to write down. This states a *property* instead — replace an item's body with a term of a manifestly different type, and the module must be refused — and applies it to every item a program has, including the ones nobody would think to attack.
//!
//! The oracle is what makes this defensible. `curios-elab` cannot be asked about a mutated *Core* module, so a mutant carries no second opinion to compare against; the property therefore has to be one whose answer is known by construction. A definition declared at `Nat` whose body is `true`, or one declared at anything else whose body is `0`, is ill-typed for a reason that needs no checker to establish — so a kernel that accepts it has admitted something false, and one that refuses it has done its job whatever rule it used.

use {
    curios_cert::recheck_module_suffix,
    curios_core::{Item, Module, Term},
    curios_text::{Entrypoint, RootSource},
};

/// Programs whose items sit at deliberately varied types — primitive, propositional, functional, indexed, and nominal — so the property is exercised against more than one shape of declaration. None declares anything at a sort, which is what makes [`foreign_body`] foreign to all of them.
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
/// Picking a literal would mean recognizing the declaration it belongs to, and a declared type is rarely the primitive it reduces to — `Nat` reaches the kernel as `/sys/Nat/Nat`, so a structural test for `Prim::NatType` misses it and substitutes a `0` that is perfectly well typed. `Type` inhabits no value type at all, which needs no recognition to be sure of; the subject below accordingly declares nothing at a sort.
fn foreign_body() -> Term {
    Term::type_ground()
}

/// Every mutant of every subject must be refused, and each unmutated subject accepted.
#[test]
fn every_body_replaced_by_a_foreign_term_is_refused() {
    let mut mutated = 0;

    for (description, source) in SUBJECTS {
        let entrypoint = source
            .parse::<Entrypoint>()
            .unwrap_or_else(|error| panic!("{description}: the subject parses: {error:?}"));
        let (module, checked_from, obligations) = curios_pipeline::typecheck_reporting(
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
            recheck_module_suffix(&module, crate::DEFAULT_STEP_BUDGET, checked_from).is_empty(),
            "{description}: the unmutated subject must be accepted, or every mutant passes for the wrong reason",
        );

        for index in checked_from..module.items.len() {
            let Item::Let(definition) = &module.items[index] else {
                continue;
            };

            let mut mutant: Module = module.clone();
            let Item::Let(target) = &mut mutant.items[index] else {
                unreachable!("the item was a `let` a moment ago");
            };
            target.body = foreign_body();

            assert!(
                !recheck_module_suffix(&mutant, crate::DEFAULT_STEP_BUDGET, checked_from)
                    .is_empty(),
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
