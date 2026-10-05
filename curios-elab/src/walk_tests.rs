//! Every walk this crate runs over whole terms, over one doubling term: sixty levels that each sum the one below with itself around a local's application to a metavariable, a tree no walk per path finishes and a graph of sixty-one nodes — and, for the sort of a type, a record of two fields at the level below. A walk the crate adds joins the table here, so one a change makes per-path again stalls its row rather than waiting for a profile; a walk private to its module keeps its fixture beside it — `denoise`'s, `typing`'s and `convert::occurrence`'s — since reaching it from here would widen it for a test.

use {
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_core::{Free, Intrinsic, Level, MetavarId, Nat, Subterm, Term},
};

fn doubled(base: Term) -> Term {
    let mut term = base;
    for _ in 0..60 {
        term = Term::intrinsic(Intrinsic::nat_add(term.clone(), term));
    }
    term
}

/// A record of two fields at `base`, sixty levels deep.
fn doubled_record(base: Term) -> Term {
    (0..60u32).fold(base, |type_, level| {
        let first = Free::local(2 * level + 100, None);
        let second = Free::local(2 * level + 101, None);

        Term::tuple_type([(first, type_.clone()), (second, type_)])
    })
}

/// Whether both operands of the sum at the root are one node — what a walk that kept the graph hands back.
fn root_operands_shared(term: &Term) -> bool {
    let Subterm::Intrinsic(Intrinsic::NatAdd(left, right)) = &**term else {
        panic!("the walk changed the root: {term}");
    };
    std::ptr::eq::<Subterm>(&**left, &**right)
}

#[test]
fn every_walk_answers_a_doubling_term_in_its_own_size() {
    let mut context = Context::with_default_budget(SYNTAX);
    context.birth_metavar(
        MetavarId(0),
        Vec::new(),
        Term::intrinsic(Intrinsic::NatType),
    );
    context.solve_metavar(
        MetavarId(0),
        Term::intrinsic(Intrinsic::Nat(Nat::new(7usize))),
    );
    // Under a local's application, so no walk that evaluates folds the sums away.
    let f = Free::local(0, Some("f"));
    let solved = doubled(Term::apply(Term::free_var(&f), [Term::hole(0)]));
    let unsolved = doubled(Term::apply(Term::free_var(&f), [Term::hole(1)]));

    let rows: Vec<(&str, bool)> = vec![
        (
            "the strict zonk",
            root_operands_shared(&zonk(&context, &solved).expect("the hole is solved")),
        ),
        (
            "the zonk of solved metavariables",
            root_operands_shared(&zonk_solved_term_metas(&context, &solved)),
        ),
        (
            "reading metavariable origins",
            metavar_origins(&[&unsolved])
                .keys()
                .copied()
                .collect::<Vec<_>>()
                == [MetavarId(1)],
        ),
        // A report reduces what it shows, so the sums fold into one product: that it answered at all is the row.
        (
            "the display rendering",
            resolved_for_display(&mut context, &solved).mentions_free(&f),
        ),
        // And a type that reduces to nothing smaller is normalized position by position, each of its sixty-one nodes once.
        (
            "the display rendering of a type",
            matches!(
                &*resolved_for_display(
                    &mut context,
                    &doubled_record(Term::intrinsic(Intrinsic::NatType)),
                ),
                Subterm::TupleType(_),
            ),
        ),
        (
            "the sort of a type",
            Sort::of(
                &mut context,
                &doubled_record(Term::intrinsic(Intrinsic::NatType)),
            )
            .is_ok_and(|sort| sort == Sort::Type(Level::zero())),
        ),
    ];

    for (walk, answered) in rows {
        assert!(answered, "{walk} over a doubling term lost its graph");
    }
}
