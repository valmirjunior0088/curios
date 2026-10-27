//! What the kernel's reduction asks of its conversion: the guards and questions it answers through the shared analysis, asking nothing itself, and the reducts and verdicts it remembers by the reduction that reached them.

use {
    super::test_support::*,
    crate::{Kernel, convert, whnf},
    curios_analysis::{Env, test_support::SYNTAX},
    curios_core::{Free, Intrinsic, Reducer, Term},
};

/// A guard answers a term the carriers' readers hold equal to it: its sum commuted, and its dual with the sum commuted, neither a spelling the key is held under.
///
/// Conversion holds `a + b < 10` and `b + a < 10` equal wherever no arm stands, by the comparisons' linear views. Inside the guard's arm the written spelling is `true` by its equation, so while nothing answered the respelling, conversion refused inside the arm a pair it held outside it. `curios_analysis::answers` is the rule, the elaborator's reducer asks it at the same point, and `curios-elab`'s `reduce::question_tests` holds this proposition under this name.
///
/// Mutation-checked: with `answers` holding only a term identical to its key, both respellings stay stuck.
#[test]
fn a_guard_answers_a_term_the_readers_hold_equal_to_it() {
    let a = binder(1, "a");
    let b = binder(2, "b");
    let sum = |left: &Free, right: &Free| {
        Term::intrinsic(Intrinsic::nat_add(
            Term::free_var(left),
            Term::free_var(right),
        ))
    };
    let guard = Term::intrinsic(Intrinsic::nat_lt(sum(&a, &b), nat(10)));
    let commuted = Term::intrinsic(Intrinsic::nat_lt(sum(&b, &a), nat(10)));
    let dual = Term::intrinsic(Intrinsic::NatLe(nat(10), sum(&b, &a)));

    let mut kernel = kernel();
    kernel.assume(&a, &nat_type());
    kernel.assume(&b, &nat_type());

    kernel.scoped(|kernel| {
        kernel
            .refine(guard.clone(), Term::intrinsic(Intrinsic::Bool(true)))
            .expect("the equation records");

        assert_eq!(
            whnf(kernel, commuted.clone()).expect("reduces").as_bool(),
            Some(true)
        );
        assert_eq!(
            whnf(kernel, dual.clone()).expect("reduces").as_bool(),
            Some(false)
        );
    });

    assert_eq!(
        whnf(&mut kernel, commuted).expect("reduces").as_bool(),
        None,
        "outside the arm the respelling is the stuck comparison it is"
    );
}

/// The binders and terms the tests of what reduction asks conversion share: `a`, `b`, `c` at `Nat`, `f: (Nat) -> Nat` and `h: (Bool) -> Nat`.
struct Asked {
    a: Free,
    b: Free,
    c: Free,
    f: Free,
    h: Free,
}

impl Asked {
    fn over(kernel: &mut Kernel) -> Self {
        let asked = Asked {
            a: binder(1, "a"),
            b: binder(2, "b"),
            c: binder(3, "c"),
            f: binder(4, "f"),
            h: binder(5, "h"),
        };
        for binder in [&asked.a, &asked.b, &asked.c] {
            kernel.assume(binder, &nat_type());
        }
        kernel.assume(
            &asked.f,
            &Term::func_type([(binder(6, "x"), nat_type())], nat_type()),
        );
        let bool_type = Term::intrinsic(Intrinsic::BoolType);
        kernel.assume(
            &asked.h,
            &Term::func_type([(binder(7, "flag"), bool_type)], nat_type()),
        );
        asked
    }

    /// `h(flag) < 5`.
    fn over_flag(&self, flag: Term) -> Term {
        Term::intrinsic(Intrinsic::nat_lt(
            Term::apply(Term::free_var(&self.h), [flag]),
            nat(5),
        ))
    }

    /// `f(left + right)`.
    fn call(&self, left: &Free, right: &Free) -> Term {
        Term::apply(
            Term::free_var(&self.f),
            [Term::intrinsic(Intrinsic::nat_add(
                Term::free_var(left),
                Term::free_var(right),
            ))],
        )
    }

    /// `f(a + b) < 10`, the guard, and `f(b + a) < 10`, which conversion holds equal to it and no reader of the carriers' algebra does: its one atom is another term.
    fn guard(&self) -> Term {
        Term::intrinsic(Intrinsic::nat_lt(self.call(&self.a, &self.b), nat(10)))
    }

    fn respelled(&self) -> Term {
        Term::intrinsic(Intrinsic::nat_lt(self.call(&self.b, &self.a), nat(10)))
    }

    /// Assume the guard holds, for the arm `inside` runs in.
    fn under<T>(&self, kernel: &mut Kernel, inside: impl FnOnce(&mut Kernel) -> T) -> T {
        kernel.scoped(|kernel| {
            kernel
                .refine(self.guard(), Term::intrinsic(Intrinsic::Bool(true)))
                .expect("the equation records");
            inside(kernel)
        })
    }
}

/// A guard answers a term the kernel's own conversion holds equal to its scrutinee: a call respelled in its argument, the guard's dual over that call, and — where the scrutinee is no operation of the algebra — the call itself. The control is a call on another argument.
///
/// The shared rule hands the question back and the kernel asks its conversion; `curios-elab`'s `reduce::question_tests` holds this proposition under this name. Mutation-checked: asked of the whole pair with the key's own equation left in force, the key reduces to its case value and the call is never the scrutinee.
#[test]
fn a_guard_answers_a_term_conversion_holds_equal_to_it() {
    let mut kernel = kernel();
    let asked = Asked::over(&mut kernel);

    asked.under(&mut kernel, |kernel| {
        assert_eq!(
            whnf(kernel, asked.respelled()).expect("reduces").as_bool(),
            Some(true)
        );
        let dual = Term::intrinsic(Intrinsic::NatLe(nat(10), asked.call(&asked.b, &asked.a)));
        assert_eq!(whnf(kernel, dual).expect("reduces").as_bool(), Some(false));
        let other = Term::intrinsic(Intrinsic::nat_lt(asked.call(&asked.a, &asked.c), nat(10)));
        assert_eq!(whnf(kernel, other).expect("reduces").as_bool(), None);
    });

    kernel.scoped(|kernel| {
        kernel
            .refine(asked.call(&asked.a, &asked.b), nat(0))
            .expect("the equation records");

        assert_eq!(whnf(kernel, asked.call(&asked.b, &asked.a)), Ok(nat(0)));
        let other = asked.call(&asked.a, &asked.c);
        assert_eq!(whnf(kernel, other.clone()), Ok(other));
    });
}

/// A shared analysis reads a term by plain reduction, which asks the kernel's conversion nothing: under a guard, the respelling a judgment's reduction answers by asking stays stuck through `Env::force`. Totality and positivity read terms this way, and conversion's proof irrelevance is sound because acceptance waits on totality, so what totality reads may rest on no verdict of conversion's.
///
/// Mutation-checked: with `Env::force` reducing as a judgment does, the respelling is `true` to the analysis.
#[test]
fn a_shared_analysis_reads_a_term_by_reduction_that_asks_nothing() {
    let mut kernel = kernel();
    let asked = Asked::over(&mut kernel);

    asked.under(&mut kernel, |kernel| {
        let read = Env::force(kernel, &asked.respelled()).expect("reduces");
        assert_eq!(read.as_bool(), None);
        assert_eq!(
            whnf(kernel, asked.respelled()).expect("reduces").as_bool(),
            Some(true),
            "the judgment's own reduction answers it, or the reading above proves nothing"
        );
    });
}

/// A reduct is remembered by the reduction that took it: what plain reduction made of a term does not answer for it where a judgment reduces it, nor the other way, whichever came first. Both orders are run, each in an arm of its own so neither meets the other's reducts, on a kernel with its memos on and on one with them off, which must agree.
///
/// Mutation-checked both ways at each entry point's table: a hit read from the judgment's table whichever reduction asks hands plain reduction the judgment's `true` in the first order, and a store filed there whichever reduction took it hands the judgment plain reduction's stuck form in the second.
#[test]
fn a_reduct_is_remembered_by_the_reduction_that_took_it() {
    let orders = |kernel: &mut Kernel| {
        let asked = Asked::over(kernel);
        // Through the forced entry point on both sides, which reads and writes the forced table and, beneath it, the unforced one.
        let judged = |kernel: &mut Kernel| {
            kernel
                .reduce_forced(asked.respelled())
                .expect("reduces")
                .as_bool()
        };
        let read = |kernel: &mut Kernel| {
            Env::force(kernel, &asked.respelled())
                .expect("reduces")
                .as_bool()
        };

        let judged_first = asked.under(kernel, |kernel| [judged(kernel), read(kernel)]);
        let read_first = asked.under(kernel, |kernel| [read(kernel), judged(kernel)]);
        [judged_first, read_first]
    };

    let cached = orders(&mut kernel());
    assert_eq!(cached, [[Some(true), None], [None, Some(true)]]);
    assert_eq!(
        orders(&mut Kernel::uncached(1_000_000, SYNTAX)),
        cached,
        "the memos changed what one reduction or the other makes of the term"
    );
}

/// A question reduction puts to conversion is answered by plain reduction, so an answer that needs a question answered inside a question is refused. Under the guards `f(a + b) < 10` and `h(f(a + b) < 10) < 5`, the term `h(f(b + a) < 10) < 5` is the second guard's scrutinee only to a conversion that asks the first guard about the respelled argument, and it stays stuck. The second guard names every binder the term names, so the filter in front of it passes the term and nothing but the question inside the question stands in the way. That is what ends the regress of reduction asking conversion asking reduction, and it leaves a question one conversion the budget bounds.
///
/// Mutation-checked: with the lookup asking under plain reduction too, the question decides the argument and the term is `true`.
#[test]
fn a_question_is_answered_by_reduction_that_asks_nothing() {
    let mut kernel = kernel();
    let asked = Asked::over(&mut kernel);

    asked.under(&mut kernel, |kernel| {
        kernel
            .refine(
                asked.over_flag(asked.guard()),
                Term::intrinsic(Intrinsic::Bool(true)),
            )
            .expect("the equation records");

        assert_eq!(
            whnf(kernel, asked.respelled()).expect("reduces").as_bool(),
            Some(true),
            "one question deep the argument is decided, or the refusal below proves nothing"
        );
        let nested = asked.over_flag(asked.respelled());
        assert_eq!(whnf(kernel, nested).expect("reduces").as_bool(), None);
    });
}

/// The binders the tests of a call under two proofs share: `a` at `Nat`, a proposition `bound`, two proofs of it, and `w: (n: Nat, at: bound) -> Nat`.
struct Proved {
    a: Free,
    w: Free,
    p1: Free,
    p2: Free,
}

impl Proved {
    fn over(kernel: &mut Kernel) -> Self {
        let proved = Proved {
            a: binder(1, "a"),
            w: binder(2, "w"),
            p1: binder(3, "p1"),
            p2: binder(4, "p2"),
        };
        let bound = binder(5, "bound");
        let proposition = Term::free_var(&bound);
        kernel.assume(&proved.a, &nat_type());
        kernel.assume(&bound, &Term::prop());
        kernel.assume(
            &proved.w,
            &Term::func_type(
                [
                    (binder(6, "n"), nat_type()),
                    (binder(7, "at"), proposition.clone()),
                ],
                nat_type(),
            ),
        );
        kernel.assume(&proved.p1, &proposition);
        kernel.assume(&proved.p2, &proposition);
        proved
    }

    /// `w(a, proof)`.
    fn call(&self, proof: &Free) -> Term {
        Term::apply(
            Term::free_var(&self.w),
            [Term::free_var(&self.a), Term::free_var(proof)],
        )
    }

    /// `w(a, proof) < 10`.
    fn below(&self, proof: &Free) -> Term {
        Term::intrinsic(Intrinsic::nat_lt(self.call(proof), nat(10)))
    }
}

/// A guard answers a call under another proof of its bound, and so does a match on the call itself. `w(a, p2) < 10` is the scrutinee `w(a, p1) < 10` to the kernel's conversion, which reads no proof, and it names a proof the key does not: so it is no reduct of the key. The filter that keeps settlement rare (`curios_analysis::could_reduce_to`) counts the binders a form names that the key does not, and passes over one that is itself a proof.
///
/// `curios-elab`'s `reduce::question_tests` holds this proposition under this name, and `curios`'s `tests::respelling` puts the programs through both checkers. Mutation-checked: with a proof counted as any other binder, both forms stay stuck.
#[test]
fn a_guard_answers_a_call_under_another_proof_of_its_bound() {
    let mut kernel = kernel();
    let proved = Proved::over(&mut kernel);

    kernel.scoped(|kernel| {
        kernel
            .refine(
                proved.below(&proved.p1),
                Term::intrinsic(Intrinsic::Bool(true)),
            )
            .expect("the equation records");
        assert_eq!(
            whnf(kernel, proved.below(&proved.p2))
                .expect("reduces")
                .as_bool(),
            Some(true)
        );
    });
    kernel.scoped(|kernel| {
        kernel
            .refine(proved.call(&proved.p1), nat(0))
            .expect("the equation records");
        assert_eq!(whnf(kernel, proved.call(&proved.p2)), Ok(nat(0)));
    });

    assert_eq!(
        whnf(&mut kernel, proved.below(&proved.p2))
            .expect("reduces")
            .as_bool(),
        None,
        "with no guard the form is the stuck comparison it is"
    );
}

/// A form naming a binder the key does not, outside a proof, is put to no equation: `f(b + 0 * c) < 10` converts with the guard `f(b) < 10` and names `c`, which is no proof, so the filter that keeps settlement rare counts it and the form stays stuck. That is what the filter still gives up, stated so that moving it is seen; the control is the conversion itself, which holds the two equal with no arm standing.
///
/// `curios-elab`'s `reduce::question_tests` holds this proposition under this name. Mutation-checked: with the filter passing every binder, the form is asked about and is `true`.
#[test]
fn a_form_naming_another_binder_outside_a_proof_is_put_to_no_equation() {
    let mut kernel = kernel();
    let asked = Asked::over(&mut kernel);
    let below = |argument: Term| {
        let call = Term::apply(Term::free_var(&asked.f), [argument]);
        Term::intrinsic(Intrinsic::nat_lt(call, nat(10)))
    };
    let erased = Intrinsic::NatMul(nat(0), Term::free_var(&asked.c));
    let guard = below(Term::free_var(&asked.b));
    let respelled = below(Term::intrinsic(Intrinsic::nat_add(
        Term::free_var(&asked.b),
        Term::intrinsic(erased),
    )));

    assert_eq!(
        convert(
            &mut kernel,
            &Term::intrinsic(Intrinsic::BoolType),
            &guard,
            &respelled
        ),
        Ok(true),
        "the two convert, or the miss below is no limit of the filter"
    );
    kernel.scoped(|kernel| {
        kernel
            .refine(guard, Term::intrinsic(Intrinsic::Bool(true)))
            .expect("the equation records");
        assert_eq!(whnf(kernel, respelled).expect("reduces").as_bool(), None);
    });
}

/// A comparison's verdict is remembered by the reduction it was reached under: under a guard, the respelling a judgment's reduction answers by asking converts with `true` for a judgment and not for plain reduction, whichever compared first. Both orders are run, each in an arm of its own so neither meets the other's verdicts, and the uncached kernel agrees.
///
/// Mutation-checked both ways: a verdict read from the judgment's table whichever reduction asks hands plain reduction the judgment's `true` in the first order, and one filed there whichever reduction reached it hands the judgment plain reduction's `false` in the second.
#[test]
fn a_verdict_is_remembered_by_the_reduction_that_reached_it() {
    let orders = |kernel: &mut Kernel| {
        let asked = Asked::over(kernel);
        let bool_type = Term::intrinsic(Intrinsic::BoolType);
        let truth = Term::intrinsic(Intrinsic::Bool(true));
        let judged = |kernel: &mut Kernel| {
            convert(kernel, &bool_type, &asked.respelled(), &truth).expect("compares")
        };
        let read = |kernel: &mut Kernel| {
            kernel
                .plainly(|kernel| convert(kernel, &bool_type, &asked.respelled(), &truth))
                .expect("compares")
        };

        let judged_first = asked.under(kernel, |kernel| [judged(kernel), read(kernel)]);
        let read_first = asked.under(kernel, |kernel| [read(kernel), judged(kernel)]);
        [judged_first, read_first]
    };

    let cached = orders(&mut kernel());
    assert_eq!(cached, [[true, false], [false, true]]);
    assert_eq!(
        orders(&mut Kernel::uncached(1_000_000, SYNTAX)),
        cached,
        "the memos changed what one reduction or the other makes of the comparison"
    );
}

/// An equation's reduced spelling is settled by the reduction that asks for it, once for each. Under the guard `f(a + b) < 10`, the inner guard `f(b + a) < 10 && c < 3` reduces to `c < 3` for a judgment, which asks its conversion about the left operand, and stands as written for plain reduction; so `c < 3` is `true` to a judgment and stuck to an analysis, whichever asked first. `curios-elab`'s `reduce::question_tests` holds this proposition under this name.
///
/// Mutation-checked both ways: with the spelling read as a judgment's whichever reduction asks, plain reduction is handed `true` in the first order, and with it filed there whichever reduction settled it, the judgment meets plain reduction's spelling in the second and answers nothing.
#[test]
fn a_reduced_spelling_is_settled_by_the_reduction_that_asks_for_it() {
    let orders = |kernel: &mut Kernel| {
        let asked = Asked::over(kernel);
        let probe = Term::intrinsic(Intrinsic::nat_lt(Term::free_var(&asked.c), nat(3)));
        let inner = Term::intrinsic(Intrinsic::BoolAnd(asked.respelled(), probe.clone()));
        let judged = |kernel: &mut Kernel| {
            kernel
                .reduce_forced(probe.clone())
                .expect("reduces")
                .as_bool()
        };
        let read = |kernel: &mut Kernel| Env::force(kernel, &probe).expect("reduces").as_bool();
        let under = |kernel: &mut Kernel, inside: &dyn Fn(&mut Kernel) -> [Option<bool>; 2]| {
            asked.under(kernel, |kernel| {
                kernel
                    .refine(inner.clone(), Term::intrinsic(Intrinsic::Bool(true)))
                    .expect("the equation records");
                inside(kernel)
            })
        };

        [
            under(kernel, &|kernel| [judged(kernel), read(kernel)]),
            under(kernel, &|kernel| [read(kernel), judged(kernel)]),
        ]
    };

    let cached = orders(&mut kernel());
    assert_eq!(cached, [[Some(true), None], [None, Some(true)]]);
    assert_eq!(
        orders(&mut Kernel::uncached(1_000_000, SYNTAX)),
        cached,
        "the memos changed what one reduction or the other makes of the term"
    );
}

/// A stuck fold is taken again over the atoms the kernel's own conversion holds one: an equality, an ordering and a truncated difference over `f(a + b)` and `f(b + a)` are decided, where a fold that pairs atoms by identity left each stuck and conversion was no congruence under it. The control is a call on another argument, and a fold that decides no more is left as plain reduction spells it, with nothing respelled written back.
///
/// Plain reduction takes no fold again, which the reading through `Env::force` holds after the judgment's own reduct is remembered. `curios-elab`'s `reduce::question_tests` holds this proposition under this name.
#[test]
fn a_stuck_fold_is_taken_again_over_atoms_conversion_holds_one() {
    let mut kernel = kernel();
    let asked = Asked::over(&mut kernel);
    let left = asked.call(&asked.a, &asked.b);
    let right = asked.call(&asked.b, &asked.a);

    let equal = Term::intrinsic(Intrinsic::nat_eql(left.clone(), right.clone()));
    assert_eq!(
        whnf(&mut kernel, equal.clone()).expect("reduces").as_bool(),
        Some(true)
    );
    let at_most = Term::intrinsic(Intrinsic::nat_lte(left.clone(), right.clone()));
    assert_eq!(
        whnf(&mut kernel, at_most).expect("reduces").as_bool(),
        Some(true)
    );
    let difference = Term::intrinsic(Intrinsic::nat_sub(left.clone(), right.clone()));
    assert_eq!(whnf(&mut kernel, difference), Ok(nat(0)));

    let other = Term::intrinsic(Intrinsic::nat_eql(
        left.clone(),
        asked.call(&asked.a, &asked.c),
    ));
    assert_eq!(whnf(&mut kernel, other).expect("reduces").as_bool(), None);

    assert_eq!(
        Env::force(&mut kernel, &equal).expect("reduces").as_bool(),
        None,
        "plain reduction takes no fold again"
    );

    let undecided = Term::intrinsic(Intrinsic::nat_lt(
        left,
        Term::intrinsic(Intrinsic::NatMul(right, Term::free_var(&asked.c))),
    ));
    let plain = Env::force(&mut kernel, &undecided).expect("reduces");
    assert_eq!(whnf(&mut kernel, undecided), Ok(plain));
}

/// A fold inside a question is not taken again, a question being answered by plain reduction (`a_question_is_answered_by_reduction_that_asks_nothing` holds the same of a guard's lookup): `h(f(a + b) == f(b + a)) == h(true)` stays stuck, its two calls being one only to a conversion that takes the fold in the first one's argument again.
///
/// Mutation-checked: with the fold taken again under plain reduction too, the question decides the argument and the comparison is `true`.
#[test]
fn a_fold_inside_a_question_is_not_taken_again() {
    let mut kernel = kernel();
    let asked = Asked::over(&mut kernel);
    let h = &asked.h;
    let equal = Term::intrinsic(Intrinsic::nat_eql(
        asked.call(&asked.a, &asked.b),
        asked.call(&asked.b, &asked.a),
    ));
    let nested = Term::intrinsic(Intrinsic::nat_eql(
        Term::apply(Term::free_var(h), [equal.clone()]),
        Term::apply(Term::free_var(h), [Term::intrinsic(Intrinsic::Bool(true))]),
    ));

    assert_eq!(
        whnf(&mut kernel, equal).expect("reduces").as_bool(),
        Some(true),
        "one question deep the fold is decided, or the refusal below proves nothing"
    );
    assert_eq!(whnf(&mut kernel, nested).expect("reduces").as_bool(), None);
}

/// A stuck elimination takes the arm the carriers' algebra reads its scrutinee as, in both reductions: a `Bool` match on `(a < b && c < b) && ¬(a < b)`, a contradiction across connectives no fold sees, is its false arm, and a structural `Nat` match on `(a + b + 1) * (b + a + 1)`, whose distribution has a floor, is its successor arm. Plain reduction takes both, the theory asking no judgment. The controls are a connective the decision diagram leaves open and a product with no floor, whose matches stand. `curios-elab`'s `reduce::question_tests` holds this proposition under this name.
#[test]
fn a_stuck_elimination_takes_the_arm_the_algebra_reads() {
    let mut kernel = kernel();
    let asked = Asked::over(&mut kernel);
    let [a, b, c] = [&asked.a, &asked.b, &asked.c].map(Term::free_var);
    let less =
        |left: &Term, right: &Term| Term::intrinsic(Intrinsic::nat_lt(left.clone(), right.clone()));
    let and = |left: Term, right: Term| Term::intrinsic(Intrinsic::BoolAnd(left, right));
    let not = |term: Term| {
        Term::intrinsic(Intrinsic::BoolXor(
            term,
            Term::intrinsic(Intrinsic::Bool(true)),
        ))
    };
    let add = |left: &Term, right: &Term| {
        Term::intrinsic(Intrinsic::nat_add(left.clone(), right.clone()))
    };
    let product = |floor: usize| {
        Term::intrinsic(Intrinsic::nat_mul(
            Term::intrinsic(Intrinsic::nat_add(add(&a, &b), nat(floor))),
            Term::intrinsic(Intrinsic::nat_add(add(&b, &a), nat(floor))),
        ))
    };
    let on_bool = |scrutinee: Term| Term::bool_match(scrutinee, None, nat_type(), nat(2), nat(1));
    let on_nat = |scrutinee: Term| {
        Term::nat_match(
            scrutinee,
            None,
            nat_type(),
            nat(10),
            &binder(20, "pred"),
            &binder(21, "hypothesis"),
            nat(20),
        )
    };

    let contradiction = on_bool(and(and(less(&a, &b), less(&c, &b)), not(less(&a, &b))));
    let successor = on_nat(product(1));
    for decided in [&contradiction, &successor] {
        let arm = whnf(&mut kernel, decided.clone()).expect("reduces");
        assert!(arm.as_nat().is_some(), "{decided} stands as {arm}");
        assert_eq!(
            Env::force(&mut kernel, decided),
            Ok(arm),
            "plain reduction takes the arm too"
        );
    }
    assert_eq!(whnf(&mut kernel, contradiction), Ok(nat(2)));
    assert_eq!(whnf(&mut kernel, successor), Ok(nat(20)));

    for open in [on_bool(and(less(&a, &b), less(&c, &b))), on_nat(product(0))] {
        let stuck = whnf(&mut kernel, open.clone()).expect("reduces");
        assert!(stuck.as_nat().is_none(), "{open} takes an arm: {stuck}");
    }
}

/// A stuck elimination classes its scrutinee's atoms in a judgment's reduction alone: a `Bool` match on `(f(a + b) < c && b < c) && ¬(f(b + a) < c)`, a contradiction once the kernel's conversion holds the two calls one, is its false arm, and plain reduction, which asks nothing, leaves it standing. No fold law reads the two comparisons together, so the classing at the dispatch is what decides it. The control is the same connective over a call on another argument, which no classing makes a contradiction. `curios-elab`'s `reduce::question_tests` holds this proposition under this name.
#[test]
fn a_stuck_elimination_classes_its_scrutinees_atoms_in_a_judgments_reduction_alone() {
    let mut kernel = kernel();
    let asked = Asked::over(&mut kernel);
    let [b, c] = [&asked.b, &asked.c].map(Term::free_var);
    let less =
        |left: &Term, right: &Term| Term::intrinsic(Intrinsic::nat_lt(left.clone(), right.clone()));
    let and = |left: Term, right: Term| Term::intrinsic(Intrinsic::BoolAnd(left, right));
    let not = |term: Term| {
        Term::intrinsic(Intrinsic::BoolXor(
            term,
            Term::intrinsic(Intrinsic::Bool(true)),
        ))
    };
    let on = |other: Term| {
        let scrutinee = and(
            and(less(&asked.call(&asked.a, &asked.b), &c), less(&b, &c)),
            not(less(&other, &c)),
        );
        Term::bool_match(scrutinee, None, nat_type(), nat(2), nat(1))
    };

    let classed = on(asked.call(&asked.b, &asked.a));
    assert_eq!(whnf(&mut kernel, classed.clone()), Ok(nat(2)));
    let plain = Env::force(&mut kernel, &classed).expect("reduces");
    assert!(
        plain.as_nat().is_none(),
        "plain reduction classed {classed}"
    );

    let other = whnf(&mut kernel, on(asked.call(&asked.a, &asked.c))).expect("reduces");
    assert!(
        other.as_nat().is_none(),
        "a call on another argument classed: {other}"
    );
}
