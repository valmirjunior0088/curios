//! The closed machine, checked against the strategy it accelerates.

use {
    super::{test_support::*, unfold_rec},
    crate::Kernel,
    curios_core::{
        Atom, Free, Global, InductDecl, InductParam, Intrinsic, Level, Many, Reducer, Scope,
        Subterm, Telescope, Term, UniverseContext,
    },
    curios_num::Binary,
    curios_utilities::{Plicity, Qualifier},
};

/// A global name handed to a closed function stays a name in what the machine hands back, exactly as it does under the strategy: `twice(g)` at a plain demand is `(x) => g(g(x))` with `g` *named*, not `g`'s body substituted twice. A machine that evaluates every beta argument and substitutes its value would inline a function-valued global's definition once per occurrence — and a web of definitions each naming the one before it twice would come back as a graph whose tree is `2^n`. The strategy substitutes the argument as written, so the two reducts would differ here, and this fixture is the one that sees it.
#[test]
fn the_closed_machine_keeps_a_global_argument_as_a_name() {
    let g = Free::global(Qualifier::from(["g"]));
    let twice = Free::global(Qualifier::from(["twice"]));
    let (x, f) = (binder(0, "x"), binder(1, "f"));
    let unary = Term::func_type([(x, nat_type())], nat_type());

    let define = |kernel: &mut Kernel| {
        kernel
            .define(
                &g,
                &unary,
                &Term::func(
                    [(x, nat_type())],
                    Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), nat(1))),
                ),
                &monomorphic(),
            )
            .expect("a name is declared once");
        kernel
            .define(
                &twice,
                &Term::func_type([(f, unary.clone())], unary.clone()),
                &Term::func(
                    [(f, unary.clone())],
                    Term::func(
                        [(x, nat_type())],
                        Term::apply(
                            Term::free_var(&f),
                            [Term::apply(Term::free_var(&f), [Term::free_var(&x)])],
                        ),
                    ),
                ),
                &monomorphic(),
            )
            .expect("a name is declared once");
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

/// A witness named at a universe instance stays that name where it is passed as one: `twice(use g.{0})` hands back `g` at its instance, as the strategy does, where a machine that evaluated it put the witness's dictionary wherever an alias over the witness unfolded, in a reported type included. Passed as an ordinary argument the same instance is evaluated, which is what tells two instances of a family apart in a level from one: the function handed back holds `g`'s body and no name.
#[test]
fn the_closed_machine_keeps_a_witness_named_at_an_instance_as_written() {
    let g = Free::global(Qualifier::from(["g"]));
    let twice = Free::global(Qualifier::from(["twice"]));
    let (x, f) = (binder(0, "x"), binder(1, "f"));
    let unary = Term::func_type([(x, nat_type())], nat_type());

    let define = |kernel: &mut Kernel| {
        kernel
            .define(
                &g,
                &unary,
                &Term::func(
                    [(x, nat_type())],
                    Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), nat(1))),
                ),
                &polymorphic(),
            )
            .expect("a name is declared once");
        kernel
            .define(
                &twice,
                &Term::func_type([(f, unary.clone())], unary.clone()),
                &Term::func(
                    [(f, unary.clone())],
                    Term::func(
                        [(x, nat_type())],
                        Term::apply(
                            Term::free_var(&f),
                            [Term::apply(Term::free_var(&f), [Term::free_var(&x)])],
                        ),
                    ),
                ),
                &monomorphic(),
            )
            .expect("a name is declared once");
    };

    let passed = |plicity: Plicity| {
        Term::apply_marked(
            Term::free_var(&twice),
            [(plicity, Term::instance_of(&g, vec![Level::zero()]))],
        )
    };

    let mut machined = kernel();
    define(&mut machined);
    let mut strategy = strategy_kernel();
    define(&mut strategy);

    let witness = machined
        .reduce(passed(Plicity::Witness))
        .expect("the machine reduces");
    assert_eq!(
        Some(witness.clone()),
        strategy.reduce(passed(Plicity::Witness)).ok(),
        "the machine and the strategy agree on the function handed back"
    );
    assert!(
        witness.mentions_free(&g),
        "the witness survives as the instance it was passed as:\n{witness}"
    );

    let ordinary = machined
        .reduce(passed(Plicity::Explicit))
        .expect("the machine reduces");
    assert!(
        !ordinary.mentions_free(&g),
        "an instance passed as an ordinary argument is evaluated:\n{ordinary}"
    );
}

/// **The differential fixture that holds the machine to the strategy.** The same closed terms are put to a kernel with the closed machine and to one without it — the recursive strategy — and the reducts must be identical, term for term, **at both demands**. The battery covers each rule the machine implements on its own: beta over eagerly-evaluated arguments, zeta's left-to-right release, all four match families, projection, recursive unfolding to a value, and the two fold recursion encodings over a packed carrier. Both evaluators determine these completely — a first-order value at the forced demand, and at the plain one either that or the folded spelling the demand stops at — so equality here is syntactic rather than up-to-anything.
///
/// Both demands are asked because the plain one is where the machine and the strategy can disagree, on `forced_then_plain` below: a `rec` block both evaluators leave unopened at a plain demand is compared there but reaches nothing.
#[test]
fn the_closed_machine_agrees_with_the_strategy() {
    let bytes_type = Term::intrinsic(Intrinsic::BytesType);
    let bytes = |data: Vec<u8>| Term::intrinsic(Intrinsic::Bytes(Binary::from_bytes(data)));
    let motive = || {
        let m = binder(100, "m");
        Scope::close(Many(1), &[&m], nat_type())
    };

    let ih_fold = {
        let (h, t, ih) = (binder(0, "h"), binder(1, "t"), binder(2, "ih"));
        Term::bytes_match_scoped(
            bytes(vec![7; 40]),
            motive(),
            nat(0),
            &[&h, &t, &ih],
            Term::intrinsic(Intrinsic::nat_add(Term::free_var(&ih), nat(1))),
        )
    };

    let tail_fold = {
        let (go, acc, b) = (binder(0, "go"), binder(1, "acc"), binder(2, "b"));
        let (h, t, ih) = (binder(3, "h"), binder(4, "t"), binder(5, "ih"));
        let body = Term::func(
            [(acc, nat_type()), (b, bytes_type.clone())],
            Term::bytes_match_scoped(
                Term::free_var(&b),
                motive(),
                Term::free_var(&acc),
                &[&h, &t, &ih],
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
                go,
                Term::func_type([(acc, nat_type()), (b, bytes_type.clone())], nat_type()),
                body,
            )],
            Term::apply(Term::free_var(&go), [nat(0), bytes(vec![3; 40])]),
        )
    };

    let countdown = {
        let (n, motive_b) = (binder(0, "n"), binder(1, "m"));
        let (pred, hypothesis, member) = (binder(2, "pred"), binder(3, "ih"), binder(4, "member"));
        let body = Term::func(
            [(n, nat_type())],
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
            [(member, Term::func_type([(n, nat_type())], nat_type()), body)],
            Term::apply(Term::free_var(&member), [nat(9)]),
        )
    };

    let beta_zeta = {
        let (x, y) = (binder(0, "x"), binder(1, "y"));
        Term::apply(
            Term::func(
                [(x, nat_type()), (y, nat_type())],
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
                ("some", vec![payload], Term::free_var(&payload)),
            ],
        )
    };

    // A run that forces a *bare* member selection and then asks a plain demand for a call on the same member. Both demands are exercised in one term, so that one run records at the first and is probed at the second: the `let` value is an intrinsic operand, which is forced, and its tail is an ordinary application, which must come back folded. Keyed on the term alone, the memo would answer a projection recorded at the forced demand to the plain probe, and the machine would run the whole fold where the strategy stops at the folded spelling.
    let forced_then_plain = {
        let (go, b, x) = (binder(0, "go"), binder(1, "b"), binder(6, "x"));
        let (h, t, ih) = (binder(2, "h"), binder(3, "t"), binder(4, "ih"));
        let body = Term::func(
            [(b, bytes_type.clone())],
            Term::bytes_match_scoped(
                Term::free_var(&b),
                motive(),
                nat(0),
                &[&h, &t, &ih],
                Term::apply(Term::free_var(&go), [Term::free_var(&t)]),
            ),
        );
        let Subterm::Rec(rec) = Term::unwrap_or_clone(Term::rec(
            [(
                go,
                Term::func_type([(b, bytes_type.clone())], nat_type()),
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

    // The same order through a call and through a match rather than a bare selection: the `let` value's operand is forced, so the call — or the match whose arm is the call — is remembered with the value the fold computes, and the tail asks the same term plainly, where the folded call is the answer. Keyed on the term alone, the memo would serve the forced value; mutation-checked by reading the forced table at a plain demand.
    let forced_then_plain_through = |through_a_match: bool| {
        let (go, b, x) = (binder(0, "go"), binder(1, "b"), binder(6, "x"));
        let (h, t, ih) = (binder(2, "h"), binder(3, "t"), binder(4, "ih"));
        let motive_b = binder(7, "m");
        let body = Term::func(
            [(b, bytes_type.clone())],
            Term::bytes_match_scoped(
                Term::free_var(&b),
                motive(),
                nat(0),
                &[&h, &t, &ih],
                Term::apply(Term::free_var(&go), [Term::free_var(&t)]),
            ),
        );
        let call = Term::apply(Term::free_var(&go), [bytes(vec![1, 2, 3])]);
        let subject = match through_a_match {
            true => Term::bool_match(
                Term::intrinsic(Intrinsic::Bool(true)),
                Some(&motive_b),
                nat_type(),
                nat(0),
                call,
            ),
            false => call,
        };
        let Subterm::Rec(rec) = Term::unwrap_or_clone(Term::rec(
            [(
                go,
                Term::func_type([(b, bytes_type.clone())], nat_type()),
                body,
            )],
            Term::let_(
                &x,
                nat_type(),
                Term::intrinsic(Intrinsic::nat_add(subject.clone(), nat(1))),
                subject,
            ),
        )) else {
            unreachable!("built as a rec")
        };

        unfold_rec(rec)
    };

    // A group whose one member never mentions itself: opening its tail reduces past the projection to the member's own value, so what the head exposes is a `Func` and the application is ordinary beta. The machine takes it from the apply arm, the strategy through `expose_rec_tail`.
    let dissolved_group = {
        let (n, unused) = (binder(0, "n"), binder(1, "unused"));
        let identity = Term::func([(n, nat_type())], Term::free_var(&n));

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

    // A member whose result is a function, called past its own parameters: `f(2)(3)` is the call `f(2)` applied to `3`. The plain demand stops at the folded spine, and the forced one reaches through it to the call — which a head frame evaluating its head at the plain demand would hand back unforced while the strategy unfolds it.
    let past_parameters = {
        let (n, motive_b, x) = (binder(0, "n"), binder(1, "m"), binder(5, "x"));
        let (pred, hypothesis, member) = (binder(2, "pred"), binder(3, "ih"), binder(4, "member"));
        let arrow = Term::func_type([(x, nat_type())], nat_type());
        let body = Term::func(
            [(n, nat_type())],
            Term::nat_match(
                Term::free_var(&n),
                Some(&motive_b),
                arrow.clone(),
                Term::func([(x, nat_type())], Term::free_var(&x)),
                &pred,
                &hypothesis,
                Term::func(
                    [(x, nat_type())],
                    Term::apply(
                        Term::apply(Term::free_var(&member), [Term::free_var(&pred)]),
                        [Term::free_var(&x)],
                    ),
                ),
            ),
        );
        let Subterm::Rec(rec) = Term::unwrap_or_clone(Term::rec(
            [(member, Term::func_type([(n, nat_type())], arrow), body)],
            Term::apply(Term::apply(Term::free_var(&member), [nat(2)]), [nat(3)]),
        )) else {
            unreachable!("built as a rec")
        };

        unfold_rec(rec)
    };

    // The same call demanded plainly and then forced, in one run: the `let` value is released at the plain demand, where the folded spine is the answer, and the tail's operand is forced. A forced probe reads the plain table as well, so the memo's guard must recognize the spine as folded and decline to record it — read one application deep, it would record the spine and serve it to the forced probe, and the sum would stick on it.
    let plain_then_forced = {
        let call = past_parameters.clone();
        let released = binder(6, "released");

        Term::let_(
            &released,
            nat_type(),
            call.clone(),
            Term::intrinsic(Intrinsic::nat_add(call, nat(0))),
        )
    };

    // Two closed forwardings under a hidden mark, which the machine contracts through `eta_head` and the strategy through its own statement of the rule: one whose argument stands under its binder's mark contracts, `(@n) => ((@m) => m)(@n)` to `(@m) => m`, and one whose argument does not stays as written.
    let forwarded_under_its_mark = {
        let (n, m) = (binder(0, "n"), binder(1, "m"));
        Term::func_marked(
            [(Plicity::Implicit, n, nat_type())],
            Term::apply_marked(
                Term::func_marked([(Plicity::Implicit, m, nat_type())], Term::free_var(&m)),
                [(Plicity::Implicit, Term::free_var(&n))],
            ),
        )
    };
    let forwarded_across_a_mark = {
        let (n, x) = (binder(0, "n"), binder(1, "x"));
        Term::func_marked(
            [(Plicity::Implicit, n, nat_type())],
            Term::apply(
                Term::func([(x, nat_type())], Term::free_var(&x)),
                [Term::free_var(&n)],
            ),
        )
    };

    for term in [
        chain(64),
        dissolved_group,
        forwarded_under_its_mark,
        forwarded_across_a_mark,
        past_parameters,
        plain_then_forced,
        ih_fold,
        tail_fold,
        countdown,
        beta_zeta,
        switch,
        projection,
        induct,
        forced_then_plain,
        forced_then_plain_through(false),
        forced_then_plain_through(true),
    ] {
        let mut machined = kernel();
        let mut strategy = strategy_kernel();

        assert_eq!(
            machined.reduce_forced(term.clone()),
            strategy.reduce_forced(term.clone()),
            "the machine and the strategy disagreed on {term}",
        );

        // The plain demand is a separate contract, not a weaker reading of the one above: it is where a folded recursive spelling is the answer rather than a step on the way to one, so a machine that unfolds here computes a value the strategy never offers. Asking only the forced demand would leave that whole half of the machine uncompared.
        let mut machined = kernel();
        let mut strategy = strategy_kernel();

        assert_eq!(
            machined.reduce(term.clone()),
            strategy.reduce(term.clone()),
            "the machine and the strategy disagreed at a plain demand on {term}",
        );
    }
}

/// A closed one-arm match on a proof of a proposition whose index pins its payload fires on the *stated* instance with the proof unevaluated — `ClosedHost::stated_payload` — and the machine and the strategy bind the same payload and reach the same value. The arm reads its payload, so the rule's second clause runs rather than the `unread` one: `match mk(5) at P(5) : (i, q) => Nat | mk(x) => x end` is `5` under both. A machine that read the payload off the scrutinee while the strategy read it off the stated instance (or the reverse) would answer a closed computation in a type with a value reduction of the same term would not.
#[test]
fn the_closed_machine_reads_a_stated_proof_payload_as_the_strategy_does() {
    let family = Global::Authored(Qualifier::from(["P"]));
    let index = binder(70, "i");
    let payload = binder(71, "x");
    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build([(index, nat_type())], ())),
        constructors: vec![(
            Atom::from("mk"),
            InductParam::new(
                Telescope::build([(payload, nat_type())], vec![Term::free_var(&payload)]),
                vec![Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: Term::prop(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let scrutinee = Term::variant(family, Vec::<Term>::new(), "mk", [nat(5)]);
    let stated = Term::induct_type(family, Vec::<Term>::new(), [nat(5)]);
    let arm_payload = binder(72, "x");
    let term = Term::induct_match_scoped_marked(
        scrutinee,
        Scope::constant(Many(2), nat_type()),
        [(
            "mk",
            vec![(Plicity::Explicit, arm_payload)],
            Term::free_var(&arm_payload),
        )],
        None,
    )
    .stating(stated);

    let mut machined = kernel();
    machined
        .declare_induct(&family, &declaration)
        .expect("declared once");
    let mut strategy = strategy_kernel();
    strategy
        .declare_induct(&family, &declaration)
        .expect("declared once");

    assert_eq!(
        machined.reduce_forced(term.clone()),
        Ok(nat(5)),
        "the stated payload is read off the pinned index",
    );
    assert_eq!(
        machined.reduce_forced(term.clone()),
        strategy.reduce_forced(term.clone()),
        "the machine and the strategy read one payload for {term}",
    );
}

/// The differential's case of a recursive step that applies a head outside the group to the folded call: `member(3)` with `member(n) = match n | 0 => 0 | pred => g(member(pred))` and `g` opaque. The strategy's `force` keeps the folded spelling wherever the reduct still names the member, and the machine's guard reads the stuck application as neutral the same way, so both hand back `member(3)` rather than one of them `g(member(2))`. Asked apart from the loop above because `g` needs a declaration in both kernels.
#[test]
fn the_closed_machine_folds_a_call_under_an_opaque_head_as_the_strategy_does() {
    let g = Free::global(Qualifier::from(["g"]));
    let (n, motive_b, x) = (binder(0, "n"), binder(1, "m"), binder(5, "x"));
    let (pred, hypothesis, member) = (binder(2, "pred"), binder(3, "ih"), binder(4, "member"));
    let unary = Term::func_type([(x, nat_type())], nat_type());
    let body = Term::func(
        [(n, nat_type())],
        Term::nat_match(
            Term::free_var(&n),
            Some(&motive_b),
            nat_type(),
            nat(0),
            &pred,
            &hypothesis,
            Term::apply(
                Term::free_var(&g),
                [Term::apply(
                    Term::free_var(&member),
                    [Term::free_var(&pred)],
                )],
            ),
        ),
    );
    let Subterm::Rec(rec) = Term::unwrap_or_clone(Term::rec(
        [(member, Term::func_type([(n, nat_type())], nat_type()), body)],
        Term::apply(Term::free_var(&member), [nat(3)]),
    )) else {
        unreachable!("built as a rec")
    };
    let term = unfold_rec(rec);

    let mut machined = kernel();
    machined
        .declare(&g, &unary, &monomorphic())
        .expect("declared once");
    let mut strategy = strategy_kernel();
    strategy
        .declare(&g, &unary, &monomorphic())
        .expect("declared once");

    assert_eq!(
        machined.reduce_forced(term.clone()),
        strategy.reduce_forced(term.clone()),
        "the machine and the strategy disagreed on {term}",
    );
}
