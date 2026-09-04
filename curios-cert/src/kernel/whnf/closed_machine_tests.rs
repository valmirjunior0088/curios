//! The closed machine, checked against the strategy it accelerates.

use {
    super::unfold_rec,
    crate::Kernel,
    curios_core::{Free, Global, Intrinsic, Many, Reducer, Scope, Subterm, Term},
    curios_utilities::{Grain, PackedBin, Qualifier},
};

use super::test_support::*;

/// A global name handed to a closed function stays a name in what the machine hands back, exactly as it does under the strategy: `twice(g)` at a plain demand is `(x) => g(g(x))` with `g` *named*, not `g`'s body substituted twice. The machine evaluated every beta argument and substituted its value, which on a function-valued global inlined the definition once per occurrence — and a web of definitions each naming the one before it twice came back as a graph whose tree was `2^n`, retained by the unfold memo and opened as a tree by the strategy's own beta. The strategy substitutes the argument as written, so the two reducts were never identical here, and this fixture is the one that sees it.
#[test]
fn the_closed_machine_keeps_a_global_argument_as_a_name() {
    let g = Free::global(Qualifier::from(["g"]));
    let twice = Free::global(Qualifier::from(["twice"]));
    let (x, f) = (binder(0, "x"), binder(1, "f"));
    let unary = Term::func_type([(x.clone(), nat_type())], nat_type());

    let define = |kernel: &mut Kernel| {
        kernel.define(
            &g,
            &unary,
            &Term::func(
                [(x.clone(), nat_type())],
                Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), nat(1))),
            ),
            &monomorphic(),
        );
        kernel.define(
            &twice,
            &Term::func_type([(f.clone(), unary.clone())], unary.clone()),
            &Term::func(
                [(f.clone(), unary.clone())],
                Term::func(
                    [(x.clone(), nat_type())],
                    Term::apply(
                        Term::free_var(&f),
                        [Term::apply(Term::free_var(&f), [Term::free_var(&x)])],
                    ),
                ),
            ),
            &monomorphic(),
        );
    };

    let term = Term::apply(Term::free_var(&twice), [Term::free_var(&g)]);

    let mut machined = kernel();
    define(&mut machined);
    let mut strategy = strategy_kernel();
    define(&mut strategy);

    let reduct = machined.reduce(term.clone()).expect("the machine reduces");
    assert_eq!(
        Some(reduct.clone()),
        strategy.reduce(term).ok(),
        "the machine and the strategy agree on the function handed back"
    );
    assert!(
        reduct.mentions_free(&g),
        "the argument survives as the name it was passed as:\n{reduct}"
    );
}

/// **The differential fixture the machine's perimeter entry names.** The same closed terms are put to a kernel with the closed machine and to one without it — the recursive strategy — and the reducts must be identical, term for term, **at both demands**. The battery covers each rule the machine implements on its own: beta over eagerly-evaluated arguments, zeta's left-to-right release, all four match families, projection, recursive unfolding to a value, and the two fold recursion encodings over a packed carrier. Both evaluators determine these completely — a first-order value at the forced demand, and at the plain one either that or the folded spelling the demand stops at — so equality here is syntactic rather than up-to-anything.
///
/// It asked `reduce_forced` alone until the plain demand was found to be where the machine and the strategy could disagree, on `forced_then_plain` below. Every recursive term in the battery before it is a `rec` block that both evaluators leave unopened at a plain demand, so the comparison ran but reached nothing.
#[test]
fn the_closed_machine_agrees_with_the_strategy() {
    let bin_type = Term::intrinsic(Intrinsic::BinType(Grain::X));
    let bytes =
        |data: Vec<u8>| Term::intrinsic(Intrinsic::Bin(Grain::X, PackedBin::from_bytes(data)));
    let motive = || {
        let m = binder(100, "m");
        Scope::close(Many(1), &[&m], nat_type())
    };

    let ih_fold = {
        let (h, t, ih) = (binder(0, "h"), binder(1, "t"), binder(2, "ih"));
        Term::bin_match_scoped(
            Grain::X,
            bytes(vec![7; 40]),
            motive(),
            nat(0),
            &h,
            &t,
            &ih,
            Term::intrinsic(Intrinsic::nat_add(Term::free_var(&ih), nat(1))),
        )
    };

    let tail_fold = {
        let (go, acc, b) = (binder(0, "go"), binder(1, "acc"), binder(2, "b"));
        let (h, t, ih) = (binder(3, "h"), binder(4, "t"), binder(5, "ih"));
        let body = Term::func(
            [(acc.clone(), nat_type()), (b.clone(), bin_type.clone())],
            Term::bin_match_scoped(
                Grain::X,
                Term::free_var(&b),
                motive(),
                Term::free_var(&acc),
                &h,
                &t,
                &ih,
                Term::apply(
                    Term::free_var(&go),
                    [
                        Term::intrinsic(Intrinsic::nat_add(
                            Term::free_var(&acc),
                            Term::intrinsic(Intrinsic::ByteToNat(Term::free_var(&h))),
                        )),
                        Term::free_var(&t),
                    ],
                ),
            ),
        );
        Term::rec(
            [(
                go.clone(),
                Term::func_type(
                    [(acc.clone(), nat_type()), (b.clone(), bin_type.clone())],
                    nat_type(),
                ),
                body,
            )],
            Term::apply(Term::free_var(&go), [nat(0), bytes(vec![3; 40])]),
        )
    };

    let countdown = {
        let (n, motive_b) = (binder(0, "n"), binder(1, "m"));
        let (pred, hypothesis, member) = (binder(2, "pred"), binder(3, "ih"), binder(4, "member"));
        let body = Term::func(
            [(n.clone(), nat_type())],
            Term::nat_match(
                Term::free_var(&n),
                Some(&motive_b),
                nat_type(),
                nat(0),
                &pred,
                &hypothesis,
                Term::apply(Term::free_var(&member), [Term::free_var(&pred)]),
            ),
        );
        Term::rec(
            [(
                member.clone(),
                Term::func_type([(n.clone(), nat_type())], nat_type()),
                body,
            )],
            Term::apply(Term::free_var(&member), [nat(9)]),
        )
    };

    let beta_zeta = {
        let (x, y) = (binder(0, "x"), binder(1, "y"));
        Term::apply(
            Term::func(
                [(x.clone(), nat_type()), (y.clone(), nat_type())],
                Term::let_(
                    &y,
                    nat_type(),
                    Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), nat(3))),
                    Term::free_var(&y),
                ),
            ),
            [nat(2), nat(0)],
        )
    };

    let switch = {
        let m = binder(0, "m");
        Term::switch(
            nat(2),
            Some(&m),
            nat_type(),
            [(1u32, nat(10)), (2, nat(20))],
            nat(99),
        )
    };

    let projection = Term::proj(Term::tuple([nat(10), nat(20), nat(30)]), 2);

    let induct = {
        let (m, payload) = (binder(0, "m"), binder(1, "a"));
        Term::induct_match(
            Term::variant(
                Global::Authored(Qualifier::from(["E"])),
                Vec::<Term>::new(),
                "some",
                [Term::intrinsic(Intrinsic::nat_add(nat(40), nat(2)))],
            ),
            Some(&m),
            nat_type(),
            [
                ("none", Vec::<Free>::new(), nat(0)),
                ("some", vec![payload.clone()], Term::free_var(&payload)),
            ],
        )
    };

    // A run that forces a *bare* member selection and then asks a plain demand for a call on the same member. Both demands are exercised in one term because the machine's value memo is run-scoped: the `let` value is an intrinsic operand, which is forced, and its tail is an ordinary application, which must come back folded. The memo is keyed on the term alone, so a projection recorded at the forced demand was answered to the plain probe, and the machine ran the whole fold where the strategy stops at the folded spelling.
    let forced_then_plain = {
        let (go, b, x) = (binder(0, "go"), binder(1, "b"), binder(6, "x"));
        let (h, t, ih) = (binder(2, "h"), binder(3, "t"), binder(4, "ih"));
        let body = Term::func(
            [(b.clone(), bin_type.clone())],
            Term::bin_match_scoped(
                Grain::X,
                Term::free_var(&b),
                motive(),
                nat(0),
                &h,
                &t,
                &ih,
                Term::apply(Term::free_var(&go), [Term::free_var(&t)]),
            ),
        );
        let Subterm::Rec(rec) = Term::unwrap_or_clone(Term::rec(
            [(
                go.clone(),
                Term::func_type([(b.clone(), bin_type.clone())], nat_type()),
                body,
            )],
            Term::let_(
                &x,
                nat_type(),
                Term::intrinsic(Intrinsic::nat_add(Term::free_var(&go), nat(1))),
                Term::apply(Term::free_var(&go), [bytes(vec![1, 2, 3])]),
            ),
        )) else {
            unreachable!("built as a rec")
        };

        unfold_rec(rec)
    };

    // A group whose one member never mentions itself: opening its tail reduces past the projection to
    // the member's own value, so what the head exposes is a `Func` and the application is ordinary
    // beta. The machine takes it from the apply arm; the strategy reaches it through the branch that
    // used to demand a projection and decline everything else.
    let dissolved_group = {
        let (n, unused) = (binder(0, "n"), binder(1, "unused"));
        let identity = Term::func([(n.clone(), nat_type())], Term::free_var(&n));

        Term::apply(
            Term::rec(
                [(
                    unused,
                    Term::func_type([(n, nat_type())], nat_type()),
                    identity.clone(),
                )],
                identity,
            ),
            [nat(2)],
        )
    };

    for term in [
        chain(64),
        dissolved_group,
        ih_fold,
        tail_fold,
        countdown,
        beta_zeta,
        switch,
        projection,
        induct,
        forced_then_plain,
    ] {
        let mut machined = kernel();
        let mut strategy = strategy_kernel();

        assert_eq!(
            machined.reduce_forced(term.clone()),
            strategy.reduce_forced(term.clone()),
            "the machine and the strategy disagreed on {term}",
        );

        // The plain demand is a separate contract, not a weaker reading of the one above: it is where a folded recursive spelling is the answer rather than a step on the way to one, so a machine that unfolds here computes a value the strategy never offers. Asking only the forced demand left that whole half of the machine uncompared.
        let mut machined = kernel();
        let mut strategy = strategy_kernel();

        assert_eq!(
            machined.reduce(term.clone()),
            strategy.reduce(term.clone()),
            "the machine and the strategy disagreed at a plain demand on {term}",
        );
    }
}
