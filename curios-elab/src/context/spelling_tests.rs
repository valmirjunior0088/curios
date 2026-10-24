//! Spellings: a term read as the kernel is handed it under the arms in force ([`Context::kernel_spelling`]), its local definitions by value and its refined variables as their values, outside every metavariable occurrence.
//!
//! An unsolved metavariable's occurrence stands for the scope it was born in, which no spelling changes: it is kept as it stands, spine and all, as conversion keeps it where it reads a block's locals by value ([`read_outside_spines`]), and nothing is gathered from it.

use {
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_core::{Free, Intrinsic, MetavarId, MetavarOrigin, Nat, Term},
};

fn context() -> Context<'static> {
    Context::with_default_budget(SYNTAX)
}

fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

fn nat(value: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(value)))
}

/// A family over two `Nat`s assumed at the top level, for terms that hold a local beside an occurrence.
fn family(context: &mut Context) -> Free {
    let family = context.fresh(Some("F"));
    let first = context.fresh(Some("i"));
    let second = context.fresh(Some("j"));
    context.assume(
        &family,
        &Term::func_type(
            [(first, nat_type()), (second, nat_type())],
            Term::type_ground(),
        ),
    );
    family
}

/// `F(n, ?0[d, n])` under `n = 2` reads `F(2, ?0[d, n])`, and `?1[F(n, d)]` stands as it is: a local definition is read by value where the term names it and nowhere inside an occurrence, an entry its spine holds whole included.
#[test]
fn a_local_definition_is_read_by_value_outside_a_metavariables_occurrence_alone() {
    let mut context = context();
    let family = family(&mut context);
    let d = context.fresh(Some("d"));
    let n = context.fresh(Some("n"));

    context.with_frame(|context| {
        context.assume(&d, &nat_type());
        context.define_assuming(&n, &nat_type(), &nat(2), None);
        let (telescope, spine) = context.identity_snapshot();
        context.birth_metavar(MetavarId(0), telescope, nat_type());
        let born = Term::metavar_birthed(0, MetavarOrigin::Hole, spine);
        let named =
            |first: Term, second: Term| Term::apply(Term::free_var(&family), [first, second]);

        assert_eq!(
            context.by_value(&named(Term::free_var(&n), born.clone())),
            named(nat(2), born)
        );

        let entry = named(Term::free_var(&n), Term::free_var(&d));
        let rewritten = Term::metavar_birthed(1, MetavarOrigin::Hole, vec![entry]);
        assert_eq!(context.by_value(&rewritten), rewritten);
    });
}

/// `F(x, ?0[x])` under an arm that refines `x` to `0` spells `F(0, ?0[x])`, and `?1[F(x, x)]` stands as it is: a refined variable is spelled as its value where the term names it and nowhere inside an occurrence, and a term naming it only there is handed back.
#[test]
fn a_refined_variable_is_spelled_as_its_value_outside_a_metavariables_occurrence_alone() {
    let mut context = context();
    let family = family(&mut context);
    let x = context.fresh(Some("x"));

    context.with_frame(|context| {
        context.assume(&x, &nat_type());
        let (telescope, spine) = context.identity_snapshot();
        context.birth_metavar(MetavarId(0), telescope, nat_type());
        let born = Term::metavar_birthed(0, MetavarOrigin::Hole, spine);
        let named =
            |first: Term, second: Term| Term::apply(Term::free_var(&family), [first, second]);

        context.with_frame(|context| {
            context.refine(&x, &nat(0));

            assert_eq!(context.spelled_as_refined(&born), born);
            assert_eq!(
                context.spelled_as_refined(&named(Term::free_var(&x), born.clone())),
                named(nat(0), born)
            );

            let entry = named(Term::free_var(&x), Term::free_var(&x));
            let rewritten = Term::metavar_birthed(1, MetavarOrigin::Hole, vec![entry]);
            assert_eq!(context.spelled_as_refined(&rewritten), rewritten);
        });
    });
}
