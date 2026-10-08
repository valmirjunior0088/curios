use {
    super::{ClosedHost, Demand, reduce_closed, unfold_rec},
    crate::{
        Cases, Category, Cost, Exhaustion, Free, Func, InductArm, Intrinsic, Let, Many, Match,
        ReduceError, Reducer, Scope, Subterm, Term,
    },
    curios_num::{Binary, Grain},
    std::collections::BTreeMap,
};

/// The minimal host: a definition store and a budget. Its own reduction *is* the machine, which is exactly the position the real hosts put it in for closed terms — anything the intrinsic folds hand back through the [`Reducer`] seam lands here and re-enters the machine.
struct Host {
    definitions: BTreeMap<Free, Term>,
    budget: u64,
    by_category: BTreeMap<Category, u64>,
    minted: u32,
}

impl Host {
    fn new(budget: u64) -> Self {
        Self {
            definitions: BTreeMap::new(),
            budget,
            by_category: BTreeMap::new(),
            minted: 0,
        }
    }
}

impl Reducer for Host {
    fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
        reduce_closed(self, term, Demand::Whnf)
    }

    fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
        reduce_closed(self, term, Demand::Forced)
    }

    fn spend(&mut self, cost: Cost) -> Result<(), ReduceError> {
        if cost.is_refused() || cost.get() > self.budget {
            return Err(ReduceError::exhausted(self.budget, cost));
        }

        *self.by_category.entry(cost.category()).or_default() += cost.get();
        self.budget -= cost.get();
        Ok(())
    }

    fn fresh_binder(&mut self, _hint: Option<&str>) -> Free {
        self.minted += 1;
        Free::local(1_000_000 + self.minted, Some("fresh"))
    }
}

impl ClosedHost for Host {
    fn closed_body(&self, name: &Free) -> Option<&Term> {
        self.definitions.get(name)
    }

    fn closed_body_at(&self, name: &Free) -> Option<&Term> {
        self.definitions.get(name)
    }
}

fn binder(index: u32, hint: &str) -> Free {
    Free::local(index, Some(hint))
}

fn nat(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(crate::Nat::new(n)))
}

fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

fn bin_type() -> Term {
    Term::intrinsic(Intrinsic::BinType(Grain::X))
}

fn bytes(data: Vec<u8>) -> Term {
    Term::intrinsic(Intrinsic::Bin(Grain::X, Binary::from_bytes(data)))
}

fn motive() -> Scope<Many> {
    let m = binder(100, "m");
    Scope::close(Many(1), &[&m], nat_type())
}

/// A stand-in for a discharged bound; reduction never inspects one.
fn qed() -> Term {
    nat(0)
}

/// The induction-hypothesis form: a structural fold whose cons arm combines the recursive result, `count(x[]) = 0`, `count(x[h, ..t]) = count(t) + 1`. Its depth is the hypothesis nesting — the shape only an explicit stack can walk without a native frame per element — so the budget this passes under is the claim: about two hundred units per element where the recursive strategy's frame row alone is [`Cost::FRAME`].
#[test]
fn an_ih_fold_costs_transitions_rather_than_frames() {
    let n = 2_000usize;
    let mut host = Host::new(200 * n as u64);
    let (h, t, ih) = (binder(0, "h"), binder(1, "t"), binder(2, "ih"));

    let count = Term::bin_match_scoped(
        Grain::X,
        bytes(vec![7; n]),
        motive(),
        nat(0),
        &h,
        &t,
        &ih,
        Term::intrinsic(Intrinsic::nat_add(Term::free_var(&ih), nat(1))),
    );

    assert_eq!(reduce_closed(&mut host, count, Demand::Forced), Ok(nat(n)));
}

/// The arm that binds its hypothesis and then returns it — `let (taken, stopped) = ih; match stopped | true => ih | …`, an induction carrying a flag past the element it stopped at — spelled as `count(x[]) = 5`, `count(x[h, ..t]) = let c = count(t); count(t)`. The `let` evaluates the hypothesis and the tail demands the same term again; recorded, the second demand is a probe and the fold is linear under the budget the single-projection test passes under. Unrecorded, each level runs the level below twice, so the fold doubles per element and runs out of steps a few dozen bytes in.
#[test]
fn a_hypothesis_demanded_twice_is_forced_once() {
    let n = 2_000usize;
    let mut host = Host::new(200 * n as u64);
    let (h, t, ih, c) = (
        binder(0, "h"),
        binder(1, "t"),
        binder(2, "ih"),
        binder(3, "c"),
    );

    let count = Term::bin_match_scoped(
        Grain::X,
        bytes(vec![7; n]),
        motive(),
        nat(5),
        &h,
        &t,
        &ih,
        Term::let_(&c, nat_type(), Term::free_var(&ih), Term::free_var(&ih)),
    );

    assert_eq!(reduce_closed(&mut host, count, Demand::Forced), Ok(nat(5)));
}

/// The tail-accumulator fold `go(acc, x[h, ..t]) = go(acc + to_nat(h), t)`, built by `tail_fold`. Eager argument evaluation is what keeps the accumulator a literal at every step instead of a chain of unevaluated additions, and the per-element budget is the same claim as the induction-hypothesis test's.
#[test]
fn a_tail_accumulator_fold_carries_values_not_chains() {
    let n = 2_000usize;
    let mut host = Host::new(200 * n as u64);

    let term = tail_fold(vec![3; n], |acc, h| {
        Term::intrinsic(Intrinsic::nat_add(
            acc,
            Term::intrinsic(Intrinsic::ByteToNat(h)),
        ))
    });

    assert_eq!(
        reduce_closed(&mut host, term, Demand::Forced),
        Ok(nat(3 * n))
    );
}

/// The same fold with a combining operation nothing folds — `Nat/and(acc + 1, h)` at the byte head, which no rule reads: the left is never the absorbing `0` and never identical to the right, and a successor floor over a stuck `and` is neutral — so the accumulator becomes a genuinely neutral chain one link longer per element, exactly as it would under the hosts. The machine's run-scoped value memo is what keeps re-encountering that chain linear; without it this fold is quadratic in transitions and prices *worse* than the recursive strategy. (`acc + h` builds no chain: the sum normal form merges like terms, so two thousand `h`s become `2000 · h`.)
#[test]
fn a_closed_neutral_accumulator_stays_linear() {
    let n = 2_000usize;
    let mut host = Host::new(400 * n as u64);

    let term = tail_fold(vec![3; n], |acc, h| {
        let bumped = Term::intrinsic(Intrinsic::nat_add(acc, nat(1)));
        Term::intrinsic(Intrinsic::NatAnd(bumped, h))
    });

    let result = reduce_closed(&mut host, term, Demand::Forced).expect("stays within the budget");
    assert!(matches!(
        &*result,
        Subterm::Intrinsic(Intrinsic::NatAnd(..))
    ));
}

/// `go(acc, x[]) = acc`, `go(acc, x[h, ..t]) = go(combine(acc, h), t)` over `data`, applied to an accumulator of zero.
fn tail_fold(data: Vec<u8>, combine: impl Fn(Term, Term) -> Term) -> Term {
    let (go, acc, b) = (binder(0, "go"), binder(1, "acc"), binder(2, "b"));
    let (h, t, ih) = (binder(3, "h"), binder(4, "t"), binder(5, "ih"));

    let body = Term::func(
        [(acc, nat_type()), (b, bin_type())],
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
                    combine(Term::free_var(&acc), Term::free_var(&h)),
                    Term::free_var(&t),
                ],
            ),
        ),
    );

    Term::rec(
        [(
            go,
            Term::func_type([(acc, nat_type()), (b, bin_type())], nat_type()),
            body,
        )],
        Term::apply(Term::free_var(&go), [nat(0), bytes(data)]),
    )
}

/// The plain-reduction contract: a recursive application is its own weak-head normal form, and only a forced demand unfolds it.
#[test]
fn whnf_keeps_a_recursive_application_folded() {
    let mut host = Host::new(100_000);
    let (go, b) = (binder(0, "go"), binder(1, "b"));
    let (h, t, ih) = (binder(2, "h"), binder(3, "t"), binder(4, "ih"));

    let body = Term::func(
        [(b, bin_type())],
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
    let group = [(go, Term::func_type([(b, bin_type())], nat_type()), body)];

    let Subterm::Rec(rec) = Term::unwrap_or_clone(Term::rec(
        group.clone(),
        Term::apply(Term::free_var(&go), [bytes(vec![1, 2, 3])]),
    )) else {
        unreachable!("built as a rec")
    };
    let folded = reduce_closed(&mut host, unfold_rec(rec), Demand::Whnf)
        .expect("weak-head reduction succeeds");
    assert!(matches!(
        &*folded,
        Subterm::Apply(apply) if apply.head.as_rec_proj().is_some()
    ));

    let forced = reduce_closed(
        &mut host,
        Term::rec(
            group,
            Term::apply(Term::free_var(&go), [bytes(vec![1, 2, 3])]),
        ),
        Demand::Forced,
    );
    assert_eq!(forced, Ok(nat(0)));
}

/// A forced *bare* member selection must not answer a later plain probe for the same selection: the run-scoped value memo is keyed on the term alone, so a value recorded under a `rec` projection was served back whatever demand asked for it, and at [`Demand::Whnf`] a projection's value is the folded spelling itself. [`Frame::Memo`]'s guard does not reach this — it tests the recorded *value* for a folded spelling, and here it is the *key* that is one.
///
/// The two probes are in one run because the memo is run-scoped. The `let` value is an intrinsic operand, which the machine evaluates at [`Demand::Forced`], so the bare selection takes the member-body path that records; the tail is an ordinary application at the run's plain demand, which must come back folded.
///
/// Served, it would return `0` — the machine taking the recorded member body for the projection's plain value, beta-reducing the recursive call and running the whole fold — where the recursive strategy returns `go(x[0x01, 0x02, 0x03])` unopened. `curios-cert`'s `the_closed_machine_agrees_with_the_strategy` carries the same term and asks both demands of the strategy itself.
///
/// The forced probe on the same term is the control: answering a projection from its shape at *every* demand, or declining the machine for `rec` altogether, would pass the probe above and fail here, because the member's body still has to be stepped into when a value is what was asked for.
#[test]
fn a_forced_bare_selection_does_not_answer_a_plain_probe() {
    let mut host = Host::new(1_000_000);
    let (go, b, x) = (binder(0, "go"), binder(1, "b"), binder(6, "x"));
    let (h, t, ih) = (binder(2, "h"), binder(3, "t"), binder(4, "ih"));

    let body = Term::func(
        [(b, bin_type())],
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
    let group = [(go, Term::func_type([(b, bin_type())], nat_type()), body)];

    let Subterm::Rec(rec) = Term::unwrap_or_clone(Term::rec(
        group,
        Term::let_(
            &x,
            nat_type(),
            Term::intrinsic(Intrinsic::nat_add(Term::free_var(&go), nat(1))),
            Term::apply(Term::free_var(&go), [bytes(vec![1, 2, 3])]),
        ),
    )) else {
        unreachable!("built as a rec")
    };

    let folded = reduce_closed(&mut host, unfold_rec(rec.clone()), Demand::Whnf)
        .expect("weak-head reduction succeeds");

    assert!(
        matches!(&*folded, Subterm::Apply(apply) if apply.head.as_rec_proj().is_some()),
        "a plain probe was served a forced value: {folded}"
    );

    // The same term at a demand for its value still computes one: the machine withholds the member's body from a plain probe, not from the eliminator.
    assert_eq!(
        reduce_closed(&mut host, unfold_rec(rec), Demand::Forced),
        Ok(nat(0))
    );

    // The control, on the branch the memo guard touches rather than on a call that reaches the member through an application: a *bare* selection is the folded spelling at a plain demand and the member's value at a forced one. Answering a projection from its shape at every demand would pass the probe above and fail the second of these.
    let v = binder(7, "v");
    let Subterm::Rec(value_rec) =
        Term::unwrap_or_clone(Term::rec([(v, nat_type(), nat(5))], Term::free_var(&v)))
    else {
        unreachable!("built as a rec")
    };
    let selection = unfold_rec(value_rec);
    assert!(
        selection.as_rec_proj().is_some(),
        "the tail is the selection"
    );

    assert_eq!(
        reduce_closed(&mut host, selection.clone(), Demand::Whnf),
        Ok(selection.clone())
    );
    assert_eq!(
        reduce_closed(&mut host, selection, Demand::Forced),
        Ok(nat(5))
    );
}

/// The substitution positions catch: an argument whose evaluation errors falls back to its unreduced spelling, so a dead erroring argument costs nothing observable — exactly the deferral the hosts get by substituting unreduced. The demanded twin below is the control: the same error at a demanded position surfaces.
#[test]
fn a_dead_erroring_argument_defers_and_a_demanded_one_surfaces() {
    let mut host = Host::new(100_000);
    let (x, y) = (binder(0, "x"), binder(1, "y"));

    let first = Term::apply(
        Term::func([(x, nat_type()), (y, nat_type())], Term::free_var(&x)),
        [
            nat(7),
            Term::intrinsic(Intrinsic::NatDiv {
                dividend: nat(1),
                divisor: nat(0),
                non_zero: qed(),
            }),
        ],
    );
    assert_eq!(reduce_closed(&mut host, first, Demand::Forced), Ok(nat(7)));

    let demanded = Term::intrinsic(Intrinsic::NatDiv {
        dividend: nat(1),
        divisor: nat(0),
        non_zero: qed(),
    });
    assert!(matches!(
        reduce_closed(&mut host, demanded, Demand::Forced),
        Err(ReduceError::DivisionByZero { .. })
    ));
}

/// A group that consumes nothing spins until the budget answers, exactly as it does under the recursive strategy.
#[test]
fn a_non_productive_recursion_exhausts_the_budget() {
    let mut host = Host::new(1_000);
    let (loop_, n) = (binder(0, "loop"), binder(1, "n"));

    let body = Term::func(
        [(n, nat_type())],
        Term::apply(Term::free_var(&loop_), [Term::free_var(&n)]),
    );
    let term = Term::rec(
        [(loop_, Term::func_type([(n, nat_type())], nat_type()), body)],
        Term::apply(Term::free_var(&loop_), [nat(1)]),
    );

    assert!(
        reduce_closed(&mut host, term, Demand::Forced).is_err_and(|spent| spent.is_exhausted())
    );
}

/// Zeta binds left to right, each value seeing the ones before it — the machine's eager spelling of the kernel's substitution.
#[test]
fn let_bindings_bind_left_to_right() {
    let mut host = Host::new(100_000);
    let (x, y) = (binder(0, "x"), binder(1, "y"));

    let term = Term::let_(
        &x,
        nat_type(),
        nat(2),
        Term::let_(
            &y,
            nat_type(),
            Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), nat(3))),
            Term::free_var(&y),
        ),
    );

    assert_eq!(reduce_closed(&mut host, term, Demand::Forced), Ok(nat(5)));
}

/// Delta through the host seam: a defined name unfolds to its body's value.
#[test]
fn a_definition_unfolds_through_the_host() {
    let mut host = Host::new(100_000);
    let f = binder(0, "f");
    host.definitions
        .insert(f, Term::intrinsic(Intrinsic::nat_add(nat(1), nat(1))));

    assert_eq!(
        reduce_closed(&mut host, Term::free_var(&f), Demand::Forced),
        Ok(nat(2))
    );

    let undefined = binder(1, "g");
    let occurrence = Term::free_var(&undefined);
    assert_eq!(
        reduce_closed(&mut host, occurrence.clone(), Demand::Forced),
        Ok(occurrence)
    );
}

/// A term whose value no budget reaches: a group that calls itself on what it was handed, applied. It stands for a certificate that costs, as a lemma proved by induction does at a large number.
fn spinning() -> Term {
    let (loop_, n) = (binder(90, "loop"), binder(91, "n"));
    let body = Term::func(
        [(n, nat_type())],
        Term::apply(Term::free_var(&loop_), [Term::free_var(&n)]),
    );

    Term::rec(
        [(loop_, Term::func_type([(n, nat_type())], nat_type()), body)],
        Term::apply(Term::free_var(&loop_), [nat(1)]),
    )
}

/// The proof of a bound is not run: no fold reads it, so a quotient is its operands' whatever its certificate would cost. Forced beside them, as every operand of an operation once was, the certificate below spends the budget.
#[test]
fn the_proof_of_a_bound_is_not_run() {
    let mut host = Host::new(100_000);
    let quotient = Term::intrinsic(Intrinsic::NatDiv {
        dividend: nat(6),
        divisor: nat(3),
        non_zero: spinning(),
    });

    assert_eq!(
        reduce_closed(&mut host, quotient, Demand::Forced),
        Ok(nat(2))
    );
}

/// An operation that does not fold keeps the proof of its bound as it was written: the three folds that rebuild their node over a proof — a pointwise operation, a regrouping and a channel's creation — hand it on unreduced.
#[test]
fn an_operation_that_does_not_fold_keeps_its_proof_as_written() {
    let mut host = Host::new(100_000);
    let (left, right) = (binder(92, "left"), binder(93, "right"));
    let stuck = [
        Intrinsic::bin_reinterp(Grain::X, Term::free_var(&left), spinning()),
        Intrinsic::BinAnd {
            grain: Grain::X,
            left: Term::free_var(&left),
            right: Term::free_var(&right),
            same_length: spinning(),
        },
        Intrinsic::Channel {
            element: nat_type(),
            capacity: Term::free_var(&left),
            positive: spinning(),
        },
    ];

    for operation in stuck {
        let written = Term::intrinsic(operation);

        assert_eq!(
            reduce_closed(&mut host, written.clone(), Demand::Forced),
            Ok(written)
        );
    }
}

/// An argument no budget this file's hosts hold could reduce: a tower of powers, which a position that evaluates it is refused on.
fn unaffordable() -> Term {
    Term::intrinsic(Intrinsic::NatPow(
        nat(10),
        Term::intrinsic(Intrinsic::NatPow(nat(10), nat(10))),
    ))
}

/// `function`, a function literal, with each binder stating what `proofs` says of whether it binds a proof.
fn stating(function: Term, proofs: &[bool]) -> Term {
    let Subterm::Func(Func { telescope }) = Term::unwrap_or_clone(function) else {
        panic!("a function literal");
    };

    Term::from(Subterm::Func(Func {
        telescope: telescope.with_proofs(proofs),
    }))
}

/// An argument at a binder that states it binds a proof is bound as written: the function below never reads its second argument, and applied to one no budget reduces it answers its first. The same function with the binder unmarked evaluates the argument before it binds it, and is refused.
#[test]
fn an_argument_at_a_proof_binder_is_bound_as_written() {
    let mut host = Host::new(100_000);
    let (n, h) = (binder(0, "n"), binder(1, "h"));
    let function = Term::func([(n, nat_type()), (h, nat_type())], Term::free_var(&n));

    assert_eq!(
        reduce_closed(
            &mut host,
            Term::apply(
                stating(function.clone(), &[false, true]),
                [nat(7), unaffordable()]
            ),
            Demand::Forced
        ),
        Ok(nat(7))
    );
    assert!(
        reduce_closed(
            &mut host,
            Term::apply(function, [nat(7), unaffordable()]),
            Demand::Forced
        )
        .is_err_and(|refusal| refusal.is_exhausted())
    );
}

/// A body that eliminates what a proof binder binds still demands it: a mark is the machine's leave to bind a term as written and changes no value, so the sum below is its operands' whichever binder says what.
#[test]
fn a_body_that_reads_a_proof_binder_demands_it() {
    let mut host = Host::new(100_000);
    let h = binder(0, "h");
    let function = stating(
        Term::func(
            [(h, nat_type())],
            Term::intrinsic(Intrinsic::nat_add(Term::free_var(&h), nat(1))),
        ),
        &[true],
    );
    let argument = Term::intrinsic(Intrinsic::nat_add(nat(2), nat(3)));

    assert_eq!(
        reduce_closed(&mut host, Term::apply(function, [argument]), Demand::Forced),
        Ok(nat(6))
    );
}

/// A `let` binds a value at a proof binder as written, and an arm a payload at one: neither tail below reads what it binds, and each answers over a term no budget reduces. Unmarked, each evaluates it first and is refused.
#[test]
fn a_let_and_an_arm_bind_a_proof_as_written() {
    let x = binder(0, "x");
    let block = Term::let_(&x, nat_type(), unaffordable(), nat(5));
    let name = crate::Global::Authored(curios_utilities::Qualifier::from(["Box"]));
    let matched = Term::induct_match(
        Term::variant(name, Vec::<Term>::new(), "box", [unaffordable()]),
        None,
        nat_type(),
        [("box", vec![x], nat(5))],
    );

    let marked = |term: &Term| match Term::unwrap_or_clone(term.clone()) {
        Subterm::Let(Let { bindings, tail }) => Term::from(Subterm::Let(Let {
            bindings,
            tail: tail.with_proofs(&[true]),
        })),
        Subterm::Match(Match {
            head,
            result,
            cases: Cases::Induct { cases, default },
        }) => Term::from(Subterm::Match(Match {
            head,
            result,
            cases: Cases::Induct {
                cases: cases
                    .into_iter()
                    .map(|(tag, arm)| {
                        let plicities = arm.plicities().to_vec();
                        (
                            tag,
                            InductArm::new(arm.body.with_proofs(&[true]), plicities),
                        )
                    })
                    .collect(),
                default,
            },
        })),
        other => panic!("neither a `let` nor a match: {other:?}"),
    };

    for term in [block, matched] {
        assert_eq!(
            reduce_closed(&mut Host::new(100_000), marked(&term), Demand::Forced),
            Ok(nat(5))
        );
        assert!(
            reduce_closed(&mut Host::new(100_000), term, Demand::Forced)
                .is_err_and(|refusal| refusal.is_exhausted())
        );
    }
}

/// What the three fold shapes spend per element, printed rather than asserted — the assertions live in the budget bounds of the ordinary tests above. Set `CURIOS_MACHINE_TRACE` to shrink the ladder and print every dispatch.
#[test]
#[ignore = "measurement: reports what the machine spends per element rather than asserting"]
fn spend_probe() {
    let sizes: Vec<usize> = match std::env::var_os("CURIOS_MACHINE_TRACE") {
        Some(_) => vec![3],
        None => vec![250, 500, 1000, 2000],
    };
    for n in sizes {
        let budget = 1_000_000_000u64;
        let mut host = Host::new(budget);
        let (h, t, ih) = (binder(0, "h"), binder(1, "t"), binder(2, "ih"));
        let count = Term::bin_match_scoped(
            Grain::X,
            bytes(vec![7; n]),
            motive(),
            nat(0),
            &h,
            &t,
            &ih,
            Term::intrinsic(Intrinsic::nat_add(Term::free_var(&ih), nat(1))),
        );
        reduce_closed(&mut host, count, Demand::Forced).unwrap();
        let ih_spent = budget - host.budget;

        let mut host = Host::new(budget);
        let term = tail_fold(vec![3; n], |acc, h| {
            Term::intrinsic(Intrinsic::nat_add(
                acc,
                Term::intrinsic(Intrinsic::ByteToNat(h)),
            ))
        });
        reduce_closed(&mut host, term, Demand::Forced).unwrap();
        let tail_spent = budget - host.budget;

        let mut host = Host::new(budget);
        let term = tail_fold(vec![3; n], |acc, h| {
            Term::intrinsic(Intrinsic::nat_add(acc, h))
        });
        reduce_closed(&mut host, term, Demand::Forced).unwrap();
        println!(
            "n={n}: ih={ih_spent} ({}/elem), tail={tail_spent} ({}/elem), neutral={} ({}/elem), neutral categories {:?}",
            ih_spent / n as u64,
            tail_spent / n as u64,
            budget - host.budget,
            (budget - host.budget) / n as u64,
            host.by_category,
        );
    }
}
