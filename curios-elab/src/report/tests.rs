use {
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_core::{Free, Intrinsic, Nat, Subterm, Term},
};

fn nat(value: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(value)))
}

/// A function type whose result is `result`.
fn returning(result: Term) -> Term {
    let n = Free::local(0, Some("n"));

    Term::func_type([(n, Term::intrinsic(Intrinsic::NatType))], result)
}

/// A function type whose result is a sum: a report shows it normalized, which reduces the sum and opens the binder, spending the budget and minting an identity.
fn under_a_binder() -> Term {
    returning(Term::intrinsic(Intrinsic::nat_add(nat(2), nat(3))))
}

/// Each level sums the one below with itself, sixty over, around a hole at the base: a tree past anything a walk per path finishes, and a graph of sixty-one nodes.
fn doubled_around_a_hole() -> Term {
    let mut term = Term::hole(0);
    for _ in 0..60 {
        term = Term::intrinsic(Intrinsic::nat_add(term.clone(), term));
    }
    term
}

/// Whether both operands of a sum at the root are one node — what a walk that kept the graph hands back.
fn root_operands_shared(term: &Term) -> bool {
    let Subterm::Intrinsic(Intrinsic::NatAdd(left, right)) = &**term else {
        panic!("the walk changed the root: {term}");
    };
    std::ptr::eq::<Subterm>(&**left, &**right)
}

/// What `context` will mint next, minted.
fn next_identity(context: &mut Context) -> Option<u64> {
    context.fresh(None).local_index()
}

#[test]
fn a_mismatch_thrown_away_spends_and_mints_nothing() {
    let mut context = Context::with_default_budget(SYNTAX);
    let side = under_a_binder();
    let before = next_identity(&mut context);
    let spent = context.consumed().units();

    let raised = mismatch(
        &mut context,
        &side,
        &side,
        &Term::intrinsic(Intrinsic::BoolType),
        None,
    );
    drop(raised);

    assert_eq!(context.consumed().units(), spent);
    assert_eq!(next_identity(&mut context), before.map(|index| index + 1));
}

#[test]
fn a_mismatch_shown_builds_its_report_where_it_is_shown() {
    let mut context = Context::with_default_budget(SYNTAX);
    let side = under_a_binder();
    let raised = mismatch(
        &mut context,
        &side,
        &side,
        &Term::intrinsic(Intrinsic::BoolType),
        None,
    );
    let before = next_identity(&mut context);
    let spent = context.consumed().units();

    let Error::TypeMismatch { inferred, .. } = raised.shown(&mut context) else {
        panic!("a mismatch between a function type and Bool is reported as one");
    };

    assert!(context.consumed().units() > spent);
    assert!(next_identity(&mut context) > before.map(|index| index + 1));
    assert_eq!(*inferred, returning(nat(5)));
}

/// Blanking a stranded region's metavariables for its report costs the type's graph, not its tree, and keeps the graph.
#[test]
fn a_required_region_type_blanks_once_per_node() {
    let mut context = Context::new(100_000, SYNTAX);
    // The type's own hole is the first the context mints, so the one hole that stands for it afterwards is another.
    let written = context.mint_metavar();

    let blanked = super::required_region_type(&mut context, &doubled_around_a_hole())
        .expect("the monad is not itself a hole");

    let holes = blanked.metavars();
    assert_eq!(holes.len(), 1);
    assert!(!holes.contains(&written));
    assert!(root_operands_shared(&blanked));
}
