//! Identities a walk is handed: a free local is refused before the kernel mints a binder that could alias it, and a bound variable no scope binds before a rule reads it as one.

use {
    super::test_support::*,
    crate::*,
    curios_analysis::test_support::*,
    curios_common::Qualifier,
    curios_core::{
        Definitions, Entrypoint, Free, Global, Intrinsic, Let, LetBinding, Level, Many, Nat,
        Program, Scope, Subterm, Term, UniverseParam, Var,
    },
};

/// The indices a stray local is tried at: every one the kernel mints first, so a walk that let the local through would open a binder carrying the same identity at one of them.
const COLLIDING: std::ops::Range<u64> = 0..4;

/// `(x : Nat) => stray`, the function a capture turns into the identity.
fn constant_over(stray: &Free) -> Term {
    Term::func(
        [(Free::local(9_000, Some("x")), nat())],
        Term::free_var(stray),
    )
}

fn held() -> Global {
    Global::Authored(Qualifier::from(["held"]))
}

/// `0`, the value a binding is given where the binding is not what is under test.
fn zero() -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)))
}

/// A module holding one definition that states `stated` as the sort its value inhabits: `held : Type = Nat`, whose value sort is the position under test.
fn sorted(stated: Term) -> Definitions {
    let item = match authored(&held(), Term::type_ground(), nat()) {
        curios_core::Item::Let(definition) => {
            curios_core::Item::Let(Box::new(curios_core::Definition {
                value_sort: Some(stated),
                ..*definition
            }))
        }
        item => item,
    };

    holding(vec![item])
}

/// A module holding `items` and nothing else.
fn holding(items: Vec<curios_core::Item>) -> Definitions {
    Definitions {
        items: items.into(),
    }
}

/// A definition mentioning a local no binder of its opens is refused, at every index the kernel could mint for the binder it opens.
///
/// The kernel's counter starts at zero, so the local is one it can hand out again: opening `x` at the stray's index turns `(x : Nat) => stray` into the identity, which inhabits `(x : Nat) -> Nat`, and the judgment — which then sees a bound variable — accepts. The refusal is taken at the boundary for that reason. Mutation-checked: dropping it certifies the definition at the colliding index.
#[test]
fn a_definition_mentioning_a_free_local_is_refused() {
    for index in COLLIDING {
        let stray = Free::local(index, Some("stray"));
        let function = Term::func_type([(Free::local(9_001, Some("x")), nat())], nat());
        let definitions = holding(vec![authored(&held(), function, constant_over(&stray))]);

        let verdicts = fixture_verdicts(&definitions, 1_000_000, &Globals::default(), SYNTAX);
        assert!(
            verdicts.iter().any(|verdict| verdict.name == Some(held())
                && matches!(&verdict.error, Error::Unbound(found) if *found == stray)),
            "a definition mentioning free local {index} was not refused for it: {verdicts:?}",
        );
    }
}

/// The sort a definition states its value inhabits is a position of the definition, held to the boundary its type and body are: a stray local there is refused, at every index the kernel could mint.
#[test]
fn a_stated_value_sort_mentioning_a_free_local_is_refused() {
    for index in COLLIDING {
        let stray = Free::local(index, Some("stray"));
        let stated = Term::apply(
            Term::func(
                [(Free::local(9_002, Some("y")), nat())],
                Term::type_ground(),
            ),
            [Term::free_var(&stray)],
        );
        let definitions = sorted(stated);

        let verdicts = fixture_verdicts(&definitions, 1_000_000, &Globals::default(), SYNTAX);
        assert!(
            verdicts.iter().any(|verdict| verdict.name == Some(held())
                && matches!(&verdict.error, Error::Unbound(found) if *found == stray)),
            "a value sort mentioning free local {index} was not refused for it: {verdicts:?}",
        );
    }
}

/// What a module states of a definition's value sort is read by nothing: the walk states the sort the value inhabits itself (`kernel::stating`). A statement holding a metavariable reduction discards, one carrying a universe parameter past the scheme, and one naming the wrong sort are each admitted, and `held : Type = Nat` is stated `Type 0` whatever it said.
#[test]
fn a_stated_value_sort_is_read_by_nothing() {
    let discarding = |argument: Term| {
        Term::apply(
            Term::func(
                [(Free::local(9_002, Some("y")), Term::type_ground())],
                Term::type_ground(),
            ),
            [argument],
        )
    };
    let escaping = discarding(Term::type_at(Level::param(UniverseParam(1))));
    let with_a_hole = Term::apply(
        Term::func(
            [(Free::local(9_003, Some("y")), nat())],
            Term::type_ground(),
        ),
        [Term::hole(7_usize)],
    );

    for (said, stated) in [
        ("a metavariable", with_a_hole),
        ("an escaping level", escaping),
        ("the wrong sort", Term::prop()),
    ] {
        let rechecked = fixture_certified(&sorted(stated), 1_000_000, &Globals::default(), SYNTAX);
        assert_eq!(rechecked.verdicts, Vec::new(), "{said}");
        let curios_core::Item::Let(definition) = &rechecked.definitions.items[0] else {
            panic!("the module holds one definition");
        };
        assert_eq!(definition.value_sort, Some(Term::type_ground()), "{said}");
    }
}

/// The entry is held to the same boundary as the module's items, since it is judged in the same walk by the same counter. Mutation-checked as above.
#[test]
fn an_entry_mentioning_a_free_local_is_refused() {
    for index in COLLIDING {
        let stray = Free::local(index, Some("stray"));
        let program = Program {
            definitions: holding(Vec::new()),
            entrypoint: Entrypoint {
                body: Term::apply(
                    constant_over(&stray),
                    [Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)))],
                ),
                type_: Some(nat()),
            },
        };

        let verdicts = fixture_verdicts(&program, 1_000_000, &Globals::default(), SYNTAX);
        assert!(
            verdicts.iter().any(|verdict| verdict.name.is_none()
                && matches!(&verdict.error, Error::Unbound(found) if *found == stray)),
            "an entry mentioning free local {index} was not refused for it: {verdicts:?}",
        );
    }
}

/// Every position of a definition is held closed: a bound variable no scope in it binds is refused rather than aborted on.
///
/// `Var::bound` is public, so a module built by hand carries an occurrence no binder of its own opens. No boundary saw one, because every one asks `Mentions`, which is built from `var.as_free()` and answers nothing for a bound occurrence — and a position no rule types, a stated value sort, is met by nothing at all, where [the board](../../../documentation/design/soundness/the-soundness-board.md) requires a refusal. The last case is the one no arity check sees either: the index is loose *past* a binder, so the scope it sits under is a legitimate one and only its depth is wrong. Mutation-checked: with the boundary dropped, the stated value sort is admitted, and a body and a type are refused by `infer`'s own `Var` rule instead.
#[test]
fn a_definition_position_holding_a_loose_bound_variable_is_refused() {
    let loose = || Term::var(Var::bound(0));
    // `let x : Nat = 0; <index 1>`: the block binds one binder and its tail reaches past it, which is why the tail's own arity is no answer to this.
    let past_a_binder = Term::from(Subterm::Let(Let {
        bindings: vec![LetBinding::new(nat(), zero())],
        tail: Scope::constant(Many(1), Term::var(Var::bound(1))),
    }));

    for (position, definitions) in [
        ("a body", holding(vec![authored(&held(), nat(), loose())])),
        ("a type", holding(vec![authored(&held(), loose(), nat())])),
        ("a stated value sort", sorted(loose())),
        (
            "a body past a binder",
            holding(vec![authored(&held(), nat(), past_a_binder)]),
        ),
    ] {
        let verdicts = fixture_verdicts(&definitions, 1_000_000, &Globals::default(), SYNTAX);
        assert!(
            verdicts.iter().any(|verdict| verdict.name == Some(held())
                && matches!(&verdict.error, Error::NotClosed { .. })),
            "{position} holding a loose bound variable was not refused for it: {verdicts:?}",
        );
    }
}

/// The entry is held closed as the module's items are, since it is judged in the same walk by the same rule.
#[test]
fn an_entry_holding_a_loose_bound_variable_is_refused() {
    let program = Program {
        definitions: holding(Vec::new()),
        entrypoint: Entrypoint {
            body: Term::var(Var::bound(0)),
            type_: Some(nat()),
        },
    };

    let verdicts = fixture_verdicts(&program, 1_000_000, &Globals::default(), SYNTAX);
    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name.is_none()
                && matches!(&verdict.error, Error::NotClosed { .. })),
        "an entry holding a loose bound variable was not refused for it: {verdicts:?}",
    );
}
