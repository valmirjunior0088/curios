//! Identities a walk is handed: a free local is refused before the kernel mints a binder that could alias it.

use {
    super::test_support::*,
    crate::{Error, Globals},
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Entrypoint, Free, Global, Intrinsic, Level, Module, Nat, Program, Term, UniverseContext,
        UniverseParam,
    },
    curios_utilities::Qualifier,
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
        let item = match authored(&held(), Term::type_ground(), nat()) {
            curios_core::Item::Let(definition) => {
                curios_core::Item::Let(Box::new(curios_core::Definition {
                    value_sort: Some(stated),
                    ..*definition
                }))
            }
            item => item,
        };
        let module = holding(vec![item]);

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
    let item = match authored(&held(), Term::type_ground(), nat()) {
        curios_core::Item::Let(definition) => {
            curios_core::Item::Let(Box::new(curios_core::Definition {
                value_sort: Some(stated),
                ..*definition
            }))
        }
        item => item,
    };
    let module = holding(vec![item]);

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
        let item = match authored(&held(), Term::type_ground(), nat()) {
            curios_core::Item::Let(definition) => {
                curios_core::Item::Let(Box::new(curios_core::Definition {
                    value_sort: Some(stated),
                    ..*definition
                }))
            }
            item => item,
        };
        let module = holding(vec![item]);

        let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX);
        assert!(
            verdicts.iter().any(|verdict| verdict.name == Some(held())
                && matches!(&verdict.error, Error::ValueSort { .. })),
            "a value sort decided by free local {index} was not refused where it is held: {verdicts:?}",
        );
    }
}

/// A universe parameter past the scheme, carried in a stated value sort where reduction discards it, is refused at the boundary — the escape the instance route (`value_sort_at`, then `instantiate_universes_scoped`) would renumber into a capture.
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
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(held())
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
