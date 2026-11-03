//! Identities a walk is handed: a free local is refused before the kernel mints a binder that could alias it, and a bound variable no scope binds before a rule reads it as one.

use {
    super::test_support::*,
    crate::*,
    curios_analysis::test_support::*,
    curios_common::Qualifier,
    curios_core::{
        Entrypoint, Free, Global, Intrinsic, Let, LetBinding, Level, Many, Module, Nat, Program,
        Scope, Subterm, Term, UniverseContext, UniverseParam, Var,
    },
    std::collections::BTreeSet,
};

/// The indices a stray local is tried at: every one the kernel mints first, so a walk that let the local through would open a binder carrying the same identity at one of them.
const COLLIDING: std::ops::Range<u64> = 0..4;

fn nat() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

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
fn sorted(stated: Term) -> Module {
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
fn holding(items: Vec<curios_core::Item>) -> Module {
    Module {
        items: items.into(),
        mounts: Vec::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
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
        let module = holding(vec![authored(&held(), function, constant_over(&stray))]);

        let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX);
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
        let module = sorted(stated);

        let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX);
        assert!(
            verdicts.iter().any(|verdict| verdict.name == Some(held())
                && matches!(&verdict.error, Error::Unbound(found) if *found == stray)),
            "a value sort mentioning free local {index} was not refused for it: {verdicts:?}",
        );
    }
}

/// The stated value sort is held to the boundary that refuses elaboration's residue in a type or a body: an unsolved metavariable there is refused, even where reducing the sort discards it.
#[test]
fn a_stated_value_sort_holding_a_metavariable_is_refused() {
    let stated = Term::apply(
        Term::func(
            [(Free::local(9_002, Some("y")), nat())],
            Term::type_ground(),
        ),
        [Term::hole(7_usize)],
    );
    let module = sorted(stated);

    let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX);
    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::NotCore(_))),
        "a value sort holding a metavariable was not refused for it: {verdicts:?}",
    );
}

/// A stray local that would decide the stated sort where a reader meets it is refused where the sort is held: the walk holds the statement to the value's sort in a scope that opens no binder, where the local decides nothing, so a sort that reads `Prop` under a binder the kernel later mints at the stray's index is no sort the value reaches.
#[test]
fn a_stated_value_sort_a_stray_local_decides_is_refused_where_it_is_held() {
    for index in COLLIDING {
        let stray = Free::local(index, Some("stray"));
        let stated = Term::bool_match(
            Term::free_var(&stray),
            None,
            Term::type_ground(),
            Term::prop(),
            Term::type_ground(),
        );
        let module = sorted(stated);

        let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX);
        assert!(
            verdicts.iter().any(|verdict| verdict.name == Some(held())
                && matches!(&verdict.error, Error::ValueSort { .. })),
            "a value sort decided by free local {index} was not refused where it is held: {verdicts:?}",
        );
    }
}

/// A universe parameter past the scheme, carried in a stated value sort where reduction discards it, is refused at the boundary — the escape the instance route (`value_sort_at`, then `instantiate_universes`) would carry to the use site as a capture.
///
/// The level sits in the domain of a redex whose body drops its binder, so the stated sort reduces to `Type 0`, which is the value's own sort: `hold_value_sort`'s conversion passes, exactly as it passes a free local reduction discards. The boundary's `universe_escape` over the unreduced sort is the sole guard, as `free_locals_of` is for the free local. Mutation-checked: dropping the `universe_escape` over `value_sort` certifies the definition.
#[test]
fn a_universe_escape_a_stated_value_sort_discards_is_refused_where_it_is_held() {
    let mut universe_context = UniverseContext::empty();
    universe_context.parameter_count = 1;
    // `((Y : Type(param 1)) => Type 0)(Prop)`: reduces to `Type 0`, dropping the domain that carries `param 1`, which the scheme does not bind.
    let escaping = Term::apply(
        Term::func(
            [(
                Free::local(9_002, Some("Y")),
                Term::type_at(Level::param(UniverseParam(1))),
            )],
            Term::type_ground(),
        ),
        [Term::prop()],
    );

    let item = match authored(&held(), Term::type_ground(), nat()) {
        curios_core::Item::Let(definition) => {
            curios_core::Item::Let(Box::new(curios_core::Definition {
                universe_context,
                value_sort: Some(escaping),
                ..*definition
            }))
        }
        item => item,
    };
    let module = holding(vec![item]);

    let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX);
    assert!(
        verdicts.iter().any(|verdict| verdict.name == Some(held())
            && matches!(&verdict.error, Error::UnclosedUniverses)),
        "a universe escape a stated value sort discards was not refused where it is held: {verdicts:?}",
    );
}

/// The entry is held to the same boundary as the module's items, since it is judged in the same walk by the same counter. Mutation-checked as above.
#[test]
fn an_entry_mentioning_a_free_local_is_refused() {
    for index in COLLIDING {
        let stray = Free::local(index, Some("stray"));
        let program = Program {
            module: holding(Vec::new()),
            entry: Entrypoint {
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
/// `Var::bound` is public, so a module built by hand carries an occurrence no binder of its own opens. No boundary saw one, because every one asks `Mentions`, which is built from `var.as_free()` and answers nothing for a bound occurrence — and `infer`'s `Var` rule reads such a variable first, through `Var::unwrap`'s `expect("a free occurrence")`, which kills the process where [the board](../../../documentation/design/soundness/the-soundness-board.md) requires a refusal. The last case is the one no arity check sees either: the index is loose *past* a binder, so the scope it sits under is a legitimate one and only its depth is wrong. Mutation-checked, and in two places. With the boundary dropped, a body aborts in `Var::unwrap` from `infer`'s own `Var` rule. With the boundary and that rule's refusal both standing but `accelerable` not reading closedness, the stated value sort aborts in the closed machine instead: it is the one position here that conversion reduces without typing it — `hold_value_sort` compares the statement to the sort the value reaches — so no rule-level refusal stands in front of it, which is what the machine's gate is for.
#[test]
fn a_definition_position_holding_a_loose_bound_variable_is_refused() {
    let loose = || Term::var(Var::bound(0));
    // `let x : Nat = 0; <index 1>`: the block binds one binder and its tail reaches past it, which is why the tail's own arity is no answer to this.
    let past_a_binder = Term::from(Subterm::Let(Let {
        bindings: vec![LetBinding::new(nat(), zero())],
        tail: Scope::constant(Many(1), Term::var(Var::bound(1))),
    }));

    for (position, module) in [
        ("a body", holding(vec![authored(&held(), nat(), loose())])),
        ("a type", holding(vec![authored(&held(), loose(), nat())])),
        ("a stated value sort", sorted(loose())),
        (
            "a body past a binder",
            holding(vec![authored(&held(), nat(), past_a_binder)]),
        ),
    ] {
        let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX);
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
        module: holding(Vec::new()),
        entry: Entrypoint {
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
