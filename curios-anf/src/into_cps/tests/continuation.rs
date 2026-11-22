//! The roles and lineages a lowered continuation carries: the role each fills, a function's continuations of its lineage, and the shared carry of none.

use {
    super::test_support::*,
    crate::*,
    curios_common::{Lineage, Qualifier},
};

/// The lowering stamps each continuation with the role it fills, which is what lets a dump name a block instead of numbering it. Nothing below reads these — `curios-emit` takes function names only — so they are display and no more; contification adds the contified function's own name on top of them.
#[test]
fn every_lowered_continuation_carries_the_role_it_fills() {
    let mut builder = Builder::new();
    let identity = builder.reserve_function();
    let subject = builder.value(Some("subject".into()));
    builder.open_block();
    let identity_body = builder.seal_block(Terminator::Return(Atom::Value(subject)));
    builder.define_function(
        identity,
        Some("identity".into()),
        vec![subject],
        identity_body,
    );
    builder.item_functions(vec![identity]);

    let family = builder.family(Some("Opt".into()));
    let none = builder.constructor(family, Some("none".into()), vec![]);
    let some = builder.constructor(
        family,
        Some("some".into()),
        vec![Field::opaque(Some("x".into()))],
    );

    builder.open_block();
    let six = nat(&mut builder, 6);
    let value = builder.let_value(
        None,
        Rhs::Construct {
            constructor: some,
            fields: vec![six],
        },
    );
    builder.open_block();
    let zero = nat(&mut builder, 0);
    let none_arm = builder.seal_block(Terminator::Return(zero));
    builder.open_block();
    let x = builder.value(Some("x".into()));
    let some_arm = builder.seal_block(Terminator::Return(Atom::Value(x)));
    let matched = builder.let_value(
        None,
        Rhs::MatchVariant {
            family,
            scrutinee: Atom::Value(value),
            arms: vec![
                VariantArm {
                    constructor: none,
                    bindings: vec![],
                    block: none_arm,
                },
                VariantArm {
                    constructor: some,
                    bindings: vec![x],
                    block: some_arm,
                },
            ],
            default: None,
        },
    );
    // A call on the match's result, and a statement after the call: the first keeps the match's result flowing into a join rather than straight out of the function, the second keeps the call's result flowing into a continuation rather than taking `split`'s tail bypass.
    let called = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(identity),
            arguments: vec![Atom::Value(matched)],
        },
    );
    let one = nat(&mut builder, 1);
    let bumped = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(called), one],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(bumped)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");

    let cps = lowered(&module);
    let mut roles: Vec<&str> = cps
        .continuations()
        .iter()
        .flatten()
        .map(|continuation| {
            continuation
                .debug_name
                .as_deref()
                .expect("every lowered continuation names its role")
        })
        .collect();
    roles.sort_unstable();
    assert_eq!(
        roles,
        ["apply/resume", "arm/none", "arm/some", "join"],
        "{cps}"
    );
}

/// The continuations a function's body lowers to are its code, so each takes the function's lineage; those of the entry, of no declaration's text, take none.
#[test]
fn a_functions_continuations_are_of_its_lineage() {
    let mut builder = Builder::new();
    let family = builder.family(Some("Opt".into()));
    let none = builder.constructor(family, Some("none".into()), vec![]);
    let some = builder.constructor(
        family,
        Some("some".into()),
        vec![Field::opaque(Some("x".into()))],
    );

    let before = builder.declaring(Declaring::of(Some(Qualifier::from(["pick"]))));
    let pick = builder.reserve_function();
    let subject = builder.value(Some("subject".into()));
    builder.open_block();
    builder.open_block();
    let zero = nat(&mut builder, 0);
    let none_arm = builder.seal_block(Terminator::Return(zero));
    builder.open_block();
    let x = builder.value(Some("x".into()));
    let some_arm = builder.seal_block(Terminator::Return(Atom::Value(x)));
    let matched = builder.let_value(
        None,
        Rhs::MatchVariant {
            family,
            scrutinee: Atom::Value(subject),
            arms: vec![
                VariantArm {
                    constructor: none,
                    bindings: vec![],
                    block: none_arm,
                },
                VariantArm {
                    constructor: some,
                    bindings: vec![x],
                    block: some_arm,
                },
            ],
            default: None,
        },
    );
    // A statement after the match keeps its result flowing into a join rather than straight out of the function.
    let one = nat(&mut builder, 1);
    let bumped = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(matched), one],
        },
    );
    let pick_body = builder.seal_block(Terminator::Return(Atom::Value(bumped)));
    builder.define_function(pick, Some("pick".into()), vec![subject], pick_body);
    builder.declaring(before);
    builder.item_functions(vec![pick]);

    builder.open_block();
    let six = nat(&mut builder, 6);
    let value = builder.let_value(
        None,
        Rhs::Construct {
            constructor: some,
            fields: vec![six],
        },
    );
    let picked = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(pick),
            arguments: vec![Atom::Value(value)],
        },
    );
    let two = nat(&mut builder, 2);
    let doubled = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatMul,
            operands: vec![Atom::Value(picked), two],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(doubled)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");

    let cps = lowered(&module);
    let lineage = Some(Lineage {
        declaration: Qualifier::from(["pick"]),
        function: 0,
    });
    let mut continuations = cps
        .continuations()
        .iter()
        .flatten()
        .map(|continuation| (continuation.debug_name.as_deref(), continuation.lineage))
        .collect::<Vec<_>>();
    continuations.sort_unstable();
    assert_eq!(
        continuations,
        [
            (Some("apply/resume"), None),
            (Some("arm/none"), lineage),
            (Some("arm/some"), lineage),
            (Some("join"), lineage),
        ],
        "{cps}"
    );
}

/// The step every bind's suspension arm calls is made once, on first use, and shared by every bind: it is of no declaration's text, though the first to ask for it is a function of one.
#[test]
fn the_shared_carry_is_of_no_lineage() {
    let mut builder = Builder::new();
    let before = builder.declaring(Declaring::of(Some(Qualifier::from(["bind"]))));
    let bind = builder.reserve_function();
    let suspension = builder.value(Some("suspension".into()));
    let rest = builder.value(Some("rest".into()));
    builder.open_block();
    let carried = builder.let_value(
        None,
        Rhs::Suspension {
            operation: SuspensionOperation::Carry,
            operands: vec![Atom::Value(suspension), Atom::Value(rest)],
        },
    );
    let body = builder.seal_block(Terminator::Return(Atom::Value(carried)));
    builder.define_function(bind, Some("bind".into()), vec![suspension, rest], body);
    builder.declaring(before);
    builder.item_functions(vec![bind]);
    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Function(bind)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");

    let cps = lowered(&module);
    let lineage = |hint: &str| {
        cps.function_ids()
            .filter_map(|id| cps.function(id))
            .find(|function| function.debug_name.as_deref() == Some(hint))
            .unwrap_or_else(|| panic!("{hint} is lowered: {cps}"))
            .lineage
    };
    assert_eq!(
        lineage("bind"),
        Some(Lineage {
            declaration: Qualifier::from(["bind"]),
            function: 0,
        })
    );
    assert_eq!(lineage("io/carry"), None, "{cps}");
}
