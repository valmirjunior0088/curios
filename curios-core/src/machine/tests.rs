use {
    super::{ClosedHost, Demand, reduce_closed, unfold_rec},
    crate::{Category, Cost, Free, Intrinsic, Many, ReduceError, Reducer, Scope, Subterm, Term},
    curios_utilities::{Grain, PackedBin},
    std::collections::BTreeMap,
};

/// The minimal host: a definition store and a budget. Its own reduction *is* the machine, which is exactly the position the real hosts put it in for closed terms — anything the intrinsic folds hand back through the [`Reducer`] seam lands here and re-enters the machine.
struct Host {
    definitions: BTreeMap<Free, Term>,
    budget: u64,
    by_category: BTreeMap<Category, u64>,
}

impl Host {
    fn new(budget: u64) -> Self {
        Self {
            definitions: BTreeMap::new(),
            budget,
            by_category: BTreeMap::new(),
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
    Term::intrinsic(Intrinsic::Bin(Grain::X, PackedBin::from_bytes(data)))
}

fn motive() -> Scope<Many> {
    let m = binder(100, "m");
    Scope::close(Many(1), &[&m], nat_type())
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

/// The same fold with a combining operation nothing folds — `acc + h` at the byte head, which no rule reads — so the accumulator becomes a genuinely neutral chain one link longer per element, exactly as it would under the hosts. The machine's run-scoped value memo is what keeps re-encountering that chain linear; without it this fold is quadratic in transitions and prices *worse* than the strategy it replaces.
#[test]
fn a_closed_neutral_accumulator_stays_linear() {
    let n = 2_000usize;
    let mut host = Host::new(400 * n as u64);

    let term = tail_fold(vec![3; n], |acc, h| {
        Term::intrinsic(Intrinsic::nat_add(acc, h))
    });

    let result = reduce_closed(&mut host, term, Demand::Forced).expect("stays within the budget");
    assert!(matches!(
        &*result,
        Subterm::Intrinsic(Intrinsic::NatAdd(..))
    ));
}

/// `go(acc, x[]) = acc`, `go(acc, x[h, ..t]) = go(combine(acc, h), t)` over `data`, applied to an accumulator of zero.
fn tail_fold(data: Vec<u8>, combine: impl Fn(Term, Term) -> Term) -> Term {
    let (go, acc, b) = (binder(0, "go"), binder(1, "acc"), binder(2, "b"));
    let (h, t, ih) = (binder(3, "h"), binder(4, "t"), binder(5, "ih"));

    let body = Term::func(
        [(acc.clone(), nat_type()), (b.clone(), bin_type())],
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
            go.clone(),
            Term::func_type(
                [(acc.clone(), nat_type()), (b.clone(), bin_type())],
                nat_type(),
            ),
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
        [(b.clone(), bin_type())],
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
    let group = [(
        go.clone(),
        Term::func_type([(b.clone(), bin_type())], nat_type()),
        body,
    )];

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

/// The substitution positions catch: an argument whose evaluation errors falls back to its unreduced spelling, so a dead erroring argument costs nothing observable — exactly the deferral the hosts get by substituting unreduced. The demanded twin below is the control: the same error at a demanded position surfaces.
#[test]
fn a_dead_erroring_argument_defers_and_a_demanded_one_surfaces() {
    let mut host = Host::new(100_000);
    let (x, y) = (binder(0, "x"), binder(1, "y"));

    let first = Term::apply(
        Term::func(
            [(x.clone(), nat_type()), (y.clone(), nat_type())],
            Term::free_var(&x),
        ),
        [nat(7), Term::intrinsic(Intrinsic::NatDiv(nat(1), nat(0)))],
    );
    assert_eq!(reduce_closed(&mut host, first, Demand::Forced), Ok(nat(7)));

    let demanded = Term::intrinsic(Intrinsic::NatDiv(nat(1), nat(0)));
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
        [(n.clone(), nat_type())],
        Term::apply(Term::free_var(&loop_), [Term::free_var(&n)]),
    );
    let term = Term::rec(
        [(
            loop_.clone(),
            Term::func_type([(n.clone(), nat_type())], nat_type()),
            body,
        )],
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
    host.definitions.insert(
        f.clone(),
        Term::intrinsic(Intrinsic::nat_add(nat(1), nat(1))),
    );

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
