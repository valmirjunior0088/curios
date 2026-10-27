//! What a judgment's reduction asks of conversion: the guards and questions it answers through the shared analysis, asking nothing itself, the case equations and reducts it answers them by, and a question declined for a commit.

use {
    super::test_support::{context, nat, nominal},
    crate::*,
    curios_core::{
        Free, InductDecl, Intrinsic, Level, Telescope, Term, UniverseConstraintKind,
        UniverseConstraintOrigin, UniverseContext, UniverseMetaId, UniverseParam, UniverseRole,
        Variance,
    },
    curios_utilities::Qualifier,
};

/// [`scrutinee_refinement_does_not_fire_at_another_ground_universe_instance`] with the level on a nominal node. Where the node's family is irrelevant in the level, the key still keeps the two spellings apart — its guard reads no family's variance, the kernel's key being the scrutinee compared with every level — and the equation answers all the same at the stuck form reduction leaves, where each checker goes by what its conversion holds one: `Wrap.{0}(Nat)` and `Wrap.{1}(Nat)` are one type with nothing required of the two levels, so a call respelled in another argument as well is the scrutinee to a question that commits nothing. Where the family is invariant the two are two types and the call stays stuck. `curios-cert`'s `kernel::convert::variance_tests` holds the answers under this name.
///
/// Mutation-checked three ways: against a key guard handed the context's registries, under which the key itself answers; against a guard past the key that reads no variance; and against a question that reads a weak equation as a commit. Under each of the last two the calls at `Wrap` stay stuck.
#[test]
fn a_case_equation_answers_at_another_instance_of_an_irrelevant_level() {
    let mut context = context();
    let asked = Asked::over(&mut context);
    let over = context.fresh(Some("over"));
    let (kind, count) = (context.fresh(Some("T")), context.fresh(Some("n")));
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    context.assume(
        &over,
        &Term::func_type(
            [
                (kind, Term::type_at(Level::constant(1))),
                (count, nat_type.clone()),
            ],
            nat_type.clone(),
        ),
    );
    let carrier = context.fresh(Some("A"));
    let sort = Term::type_at(Level::param(UniverseParam(0)));
    for (family, variance) in [("Wrap", Variance::Irrelevant), ("Box", Variance::Invariant)] {
        context
            .register_induct(
                &nominal(family),
                InductDecl {
                    universe_context: UniverseContext {
                        parameter_count: 1,
                        quality_count: 0,
                        constraints: Vec::new(),
                    },
                    arity: Telescope::build([(carrier, sort.clone())], Telescope::done(())),
                    constructors: Vec::new().into(),
                    result_sort: sort.clone(),
                    module: Qualifier::empty(),
                    rep_public: true,
                    polarities: Vec::new(),
                    variances: vec![variance],
                },
            )
            .unwrap();
    }
    let at = |family: &str, level: u32, left: &Free, right: &Free| {
        Term::apply(
            Term::free_var(&over),
            [
                Term::induct_type_at(
                    nominal(family),
                    [Level::constant(level)],
                    [nat_type.clone()],
                    Vec::<Term>::new(),
                ),
                Term::intrinsic(Intrinsic::nat_add(
                    Term::free_var(left),
                    Term::free_var(right),
                )),
            ],
        )
    };

    for (family, answered) in [("Wrap", true), ("Box", false)] {
        context.with_frame(|context| {
            let registered = at(family, 0, &asked.a, &asked.b);
            let key = shallow_scrutinee(context, &registered);
            context.refine_scrutinee_spellings(
                vec![(key.clone(), registered.clone(), false)],
                &nat(1),
            );
            assert_eq!(reduce(context, registered), Ok(nat(1)));

            let lifted = at(family, 1, &asked.a, &asked.b);
            assert_eq!(
                context.scrutinee_reduct(&key, &lifted),
                None,
                "the key reads no variance"
            );
            let levels = context.universes().state_token();
            for probe in [lifted, at(family, 1, &asked.b, &asked.a)] {
                let expected = match answered {
                    true => nat(1),
                    false => probe.clone(),
                };
                assert_eq!(reduce(context, probe), Ok(expected), "{family}");
            }
            assert_eq!(
                context.universes().state_token(),
                levels,
                "the question committed nothing"
            );
        });
    }
}

/// A spelling that is its own weak-head form keeps its binder names when the erased door answers for it: the stored entry belongs to another declaration's Π-type, α-equivalent and equal once universes are erased, and the answer is the asking spelling itself rather than that one. Served the stored spelling, `satisfy Show(Tree)`'s refusal would read `(T: Type) -> Type` for a `Tree` declared over `A`.
#[test]
fn a_self_entry_served_across_spellings_keeps_the_asking_binder_names() {
    let mut context = context();
    let stored = Term::func_type(
        [(Free::local(1, Some("T")), Term::type_at(Level::constant(0)))],
        Term::type_at(Level::constant(0)),
    );
    assert_eq!(reduce(&mut context, stored.clone()), Ok(stored));

    let asked = Term::func_type(
        [(
            Free::local(2, Some("A")),
            Term::type_at(Level::meta(UniverseMetaId(3))),
        )],
        Term::type_at(Level::meta(UniverseMetaId(4))),
    );
    let answer = reduce(&mut context, asked.clone()).expect("a Π-type is its own form");
    assert_eq!(answer, asked);
    assert!(
        answer.to_string().starts_with("(A: "),
        "the asking spelling's binder, not the stored one's: {answer}"
    );
}

/// A guard answers a term the carriers' readers hold equal to it: its sum commuted, and its dual with the sum commuted, neither a spelling the key is held under. The kernel's `whnf::question_tests` holds this proposition under this name, and `curios_analysis::answers` is the one rule both ask.
#[test]
fn a_guard_answers_a_term_the_readers_hold_equal_to_it() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    let sum = |left: &Free, right: &Free| {
        Term::intrinsic(Intrinsic::nat_add(
            Term::free_var(left),
            Term::free_var(right),
        ))
    };
    let guard = Term::intrinsic(Intrinsic::nat_lt(sum(&a, &b), nat(10)));
    let commuted = Term::intrinsic(Intrinsic::nat_lt(sum(&b, &a), nat(10)));
    let dual = Term::intrinsic(Intrinsic::NatLe(nat(10), sum(&b, &a)));

    assert_eq!(
        reduce(&mut context, commuted.clone()).map(|reduct| reduct.as_bool()),
        Ok(None),
        "with no guard the respelling is the stuck comparison it is"
    );

    let canonical = shallow_scrutinee(&context, &guard);
    context.refine_scrutinee_spellings(
        vec![(canonical, guard, false)],
        &Term::intrinsic(Intrinsic::Bool(true)),
    );

    assert_eq!(
        reduce(&mut context, commuted).map(|reduct| reduct.as_bool()),
        Ok(Some(true))
    );
    assert_eq!(
        reduce(&mut context, dual).map(|reduct| reduct.as_bool()),
        Ok(Some(false))
    );
}

/// The binders and terms the tests of what reduction asks conversion share, as the kernel's `whnf::question_tests` has them: `a`, `b`, `c` at `Nat`, `f: (Nat) -> Nat` and `h: (Bool) -> Nat`.
struct Asked {
    a: Free,
    b: Free,
    c: Free,
    f: Free,
    h: Free,
}

impl Asked {
    fn over(context: &mut Context) -> Self {
        let asked = Asked {
            a: context.fresh(Some("a")),
            b: context.fresh(Some("b")),
            c: context.fresh(Some("c")),
            f: context.fresh(Some("f")),
            h: context.fresh(Some("h")),
        };
        let x = context.fresh(Some("x"));
        let flag = context.fresh(Some("flag"));
        let nat_type = Term::intrinsic(Intrinsic::NatType);
        for binder in [&asked.a, &asked.b, &asked.c] {
            context.assume(binder, &nat_type);
        }
        context.assume(
            &asked.f,
            &Term::func_type([(x, nat_type.clone())], nat_type.clone()),
        );
        context.assume(
            &asked.h,
            &Term::func_type([(flag, Term::intrinsic(Intrinsic::BoolType))], nat_type),
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

    fn call(&self, left: &Free, right: &Free) -> Term {
        Term::apply(
            Term::free_var(&self.f),
            [Term::intrinsic(Intrinsic::nat_add(
                Term::free_var(left),
                Term::free_var(right),
            ))],
        )
    }

    fn guard(&self) -> Term {
        Term::intrinsic(Intrinsic::nat_lt(self.call(&self.a, &self.b), nat(10)))
    }

    fn respelled(&self) -> Term {
        Term::intrinsic(Intrinsic::nat_lt(self.call(&self.b, &self.a), nat(10)))
    }

    /// Register the guard as holding, for the frame `inside` runs in.
    fn under<T>(&self, context: &mut Context, inside: impl FnOnce(&mut Context) -> T) -> T {
        context.with_frame(|context| {
            let guard = self.guard();
            let canonical = shallow_scrutinee(context, &guard);
            context.refine_scrutinee_spellings(
                vec![(canonical, guard, false)],
                &Term::intrinsic(Intrinsic::Bool(true)),
            );
            inside(context)
        })
    }
}

/// A guard answers a term the elaborator's own conversion holds equal to its scrutinee: a call respelled in its argument, the guard's dual over that call, and — where the scrutinee is no operation of the algebra — the call itself. The kernel's `whnf::question_tests` holds this proposition under this name.
#[test]
fn a_guard_answers_a_term_conversion_holds_equal_to_it() {
    let mut context = context();
    let asked = Asked::over(&mut context);
    let decided =
        |context: &mut Context, term: Term| reduce(context, term).map(|reduct| reduct.as_bool());

    asked.under(&mut context, |context| {
        assert_eq!(decided(context, asked.respelled()), Ok(Some(true)));
        let dual = Term::intrinsic(Intrinsic::NatLe(nat(10), asked.call(&asked.b, &asked.a)));
        assert_eq!(decided(context, dual), Ok(Some(false)));
        let other = Term::intrinsic(Intrinsic::nat_lt(asked.call(&asked.a, &asked.c), nat(10)));
        assert_eq!(decided(context, other), Ok(None));
    });

    context.with_frame(|context| {
        let scrutinee = asked.call(&asked.a, &asked.b);
        let canonical = shallow_scrutinee(context, &scrutinee);
        context.refine_scrutinee_spellings(vec![(canonical, scrutinee, false)], &nat(0));

        assert_eq!(reduce(context, asked.call(&asked.b, &asked.a)), Ok(nat(0)));
        let other = asked.call(&asked.a, &asked.c);
        assert_eq!(reduce(context, other.clone()), Ok(other));
    });
}

/// A shared analysis reads a term by plain reduction, which asks the elaborator's conversion nothing, in either order the two reductions are taken in: under a guard, the respelling a judgment's reduction answers by asking stays stuck through `Env::force`, so totality rests on no verdict of conversion's, and a reduct one of the two filed does not answer the other. The kernel's `whnf::question_tests` holds the first half under this name.
///
/// Mutation-checked: with `Env::force` reducing as a judgment does the respelling is `true` to the analysis, and with the judgment's reduction table read whichever reduction asks, the judgment's `true` answers the analysis in the first order.
#[test]
fn a_shared_analysis_reads_a_term_by_reduction_that_asks_nothing() {
    let mut context = context();
    let asked = Asked::over(&mut context);
    let judged =
        |context: &mut Context| reduce(context, asked.respelled()).map(|reduct| reduct.as_bool());
    let read = |context: &mut Context| {
        curios_analysis::Env::force(context, &asked.respelled())
            .ok()
            .map(|reduct| reduct.as_bool())
    };

    asked.under(&mut context, |context| {
        assert_eq!(judged(context), Ok(Some(true)));
        assert_eq!(read(context), Some(None));
    });
    asked.under(&mut context, |context| {
        assert_eq!(read(context), Some(None));
        assert_eq!(judged(context), Ok(Some(true)));
    });
}

/// A question reduction puts to conversion is answered by plain reduction, so an answer that needs a question answered inside a question is refused: under the guards `f(a + b) < 10` and `h(f(a + b) < 10) < 5`, the term `h(f(b + a) < 10) < 5` stays stuck, the second guard naming every binder it names. The kernel's `whnf::question_tests` holds this proposition under this name.
///
/// Mutation-checked: with the lookup asking under plain reduction too, the question decides the argument and the term is `true`.
#[test]
fn a_question_is_answered_by_reduction_that_asks_nothing() {
    let mut context = context();
    let asked = Asked::over(&mut context);

    asked.under(&mut context, |context| {
        let guard = asked.over_flag(asked.guard());
        let canonical = shallow_scrutinee(context, &guard);
        context.refine_scrutinee_spellings(
            vec![(canonical, guard, false)],
            &Term::intrinsic(Intrinsic::Bool(true)),
        );

        assert_eq!(
            reduce(context, asked.respelled()).map(|reduct| reduct.as_bool()),
            Ok(Some(true)),
            "one question deep the argument is decided, or the refusal below proves nothing"
        );
        let nested = asked.over_flag(asked.respelled());
        assert_eq!(
            reduce(context, nested).map(|reduct| reduct.as_bool()),
            Ok(None)
        );
    });
}

/// The binders the tests of a call under two proofs share, as the kernel's `whnf::question_tests` has them: `a` at `Nat`, a proposition `bound`, two proofs of it, and `w: (n: Nat, at: bound) -> Nat`.
struct Proved {
    a: Free,
    w: Free,
    p1: Free,
    p2: Free,
}

impl Proved {
    fn over(context: &mut Context) -> Self {
        let proved = Proved {
            a: context.fresh(Some("a")),
            w: context.fresh(Some("w")),
            p1: context.fresh(Some("p1")),
            p2: context.fresh(Some("p2")),
        };
        let bound = context.fresh(Some("bound"));
        let n = context.fresh(Some("n"));
        let at = context.fresh(Some("at"));
        let nat_type = Term::intrinsic(Intrinsic::NatType);
        let proposition = Term::free_var(&bound);
        context.assume(&proved.a, &nat_type);
        context.assume(&bound, &Term::prop());
        context.assume(
            &proved.w,
            &Term::func_type([(n, nat_type.clone()), (at, proposition.clone())], nat_type),
        );
        context.assume(&proved.p1, &proposition);
        context.assume(&proved.p2, &proposition);
        proved
    }

    fn call(&self, proof: &Free) -> Term {
        Term::apply(
            Term::free_var(&self.w),
            [Term::free_var(&self.a), Term::free_var(proof)],
        )
    }

    fn below(&self, proof: &Free) -> Term {
        Term::intrinsic(Intrinsic::nat_lt(self.call(proof), nat(10)))
    }
}

/// Register `scrutinee` as assumed to be `value`, for the frame `inside` runs in.
fn assumed<T>(
    context: &mut Context,
    scrutinee: Term,
    value: Term,
    inside: impl FnOnce(&mut Context) -> T,
) -> T {
    context.with_frame(|context| {
        let canonical = shallow_scrutinee(context, &scrutinee);
        context.refine_scrutinee_spellings(vec![(canonical, scrutinee, false)], &value);
        inside(context)
    })
}

/// A guard answers a call under another proof of its bound, and so does a match on the call itself: the form names a proof the scrutinee does not, so it is no reduct of it, and the filter in front of every entry passes over a binder that is itself a proof. The kernel's `whnf::question_tests` holds this proposition under this name.
///
/// Mutation-checked: with a proof counted as any other binder, both forms stay stuck.
#[test]
fn a_guard_answers_a_call_under_another_proof_of_its_bound() {
    let mut context = context();
    let proved = Proved::over(&mut context);
    let truth = Term::intrinsic(Intrinsic::Bool(true));

    assumed(&mut context, proved.below(&proved.p1), truth, |context| {
        assert_eq!(
            reduce(context, proved.below(&proved.p2)).map(|reduct| reduct.as_bool()),
            Ok(Some(true))
        );
    });
    assumed(&mut context, proved.call(&proved.p1), nat(0), |context| {
        assert_eq!(reduce(context, proved.call(&proved.p2)), Ok(nat(0)));
    });
}

/// A form naming a binder the scrutinee does not, outside a proof, is put to no entry: `f(b + 0 * c) < 10` converts with the guard `f(b) < 10` and names `c`, which is no proof, so the form stays stuck. The kernel's `whnf::question_tests` holds this proposition under this name.
///
/// Mutation-checked: with the filter passing every binder, the form is asked about and is `true`.
#[test]
fn a_form_naming_another_binder_outside_a_proof_is_put_to_no_equation() {
    let mut context = context();
    let asked = Asked::over(&mut context);
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
    let bool_type = Term::intrinsic(Intrinsic::BoolType);
    let truth = Term::intrinsic(Intrinsic::Bool(true));

    assert_eq!(
        convert(&mut context, &bool_type, &guard, &respelled),
        Ok(true),
        "the two convert, or the miss below is no limit of the filter"
    );
    assumed(&mut context, guard, truth, |context| {
        assert_eq!(
            reduce(context, respelled).map(|reduct| reduct.as_bool()),
            Ok(None)
        );
    });
}

/// An entry's reduced spelling is settled by the reduction that asks for it, once for each. Under the guard `f(a + b) < 10`, the inner guard `f(b + a) < 10 && c < 3`, registered in a frame of its own so the outer guard stands while it is settled, reduces to `c < 3` for a judgment, which asks its conversion about the left operand, and stands as written for plain reduction; so `c < 3` is `true` to a judgment and stuck to an analysis, whichever asked first. The kernel's `whnf::question_tests` holds this proposition under this name.
///
/// Mutation-checked both ways: with the spelling read as a judgment's whichever reduction asks, plain reduction is handed `true` in the first order, and with it filed there whichever reduction settled it, the judgment meets plain reduction's spelling in the second and answers nothing.
#[test]
fn a_reduced_spelling_is_settled_by_the_reduction_that_asks_for_it() {
    let mut context = context();
    let asked = Asked::over(&mut context);
    let probe = Term::intrinsic(Intrinsic::nat_lt(Term::free_var(&asked.c), nat(3)));
    let inner = Term::intrinsic(Intrinsic::BoolAnd(asked.respelled(), probe.clone()));
    let judged =
        |context: &mut Context| reduce(context, probe.clone()).map(|reduct| reduct.as_bool());
    let read = |context: &mut Context| {
        curios_analysis::Env::force(context, &probe)
            .ok()
            .map(|reduct| reduct.as_bool())
    };
    let under = |context: &mut Context, inside: &dyn Fn(&mut Context)| {
        asked.under(context, |context| {
            context.with_frame(|context| {
                let canonical = shallow_scrutinee(context, &inner);
                context.refine_scrutinee_spellings(
                    vec![(canonical, inner.clone(), false)],
                    &Term::intrinsic(Intrinsic::Bool(true)),
                );
                inside(context)
            })
        })
    };

    under(&mut context, &|context| {
        assert_eq!(judged(context), Ok(Some(true)));
        assert_eq!(read(context), Some(None));
    });
    under(&mut context, &|context| {
        assert_eq!(read(context), Some(None));
        assert_eq!(judged(context), Ok(Some(true)));
    });
}

/// A stuck fold is taken again over the atoms the elaborator's own conversion holds one, a fold that decides no more is left as plain reduction spells it, and plain reduction takes no fold again. The kernel's `whnf::question_tests` holds this proposition under this name.
#[test]
fn a_stuck_fold_is_taken_again_over_atoms_conversion_holds_one() {
    let mut context = context();
    let asked = Asked::over(&mut context);
    let left = asked.call(&asked.a, &asked.b);
    let right = asked.call(&asked.b, &asked.a);
    let decided =
        |context: &mut Context, term: Term| reduce(context, term).map(|reduct| reduct.as_bool());

    let equal = Term::intrinsic(Intrinsic::nat_eql(left.clone(), right.clone()));
    assert_eq!(decided(&mut context, equal.clone()), Ok(Some(true)));
    let at_most = Term::intrinsic(Intrinsic::nat_lte(left.clone(), right.clone()));
    assert_eq!(decided(&mut context, at_most), Ok(Some(true)));
    let difference = Term::intrinsic(Intrinsic::nat_sub(left.clone(), right.clone()));
    assert_eq!(reduce(&mut context, difference), Ok(nat(0)));

    let other = Term::intrinsic(Intrinsic::nat_eql(
        left.clone(),
        asked.call(&asked.a, &asked.c),
    ));
    assert_eq!(decided(&mut context, other), Ok(None));

    let read = curios_analysis::Env::force(&mut context, &equal).ok();
    assert_eq!(
        read.map(|reduct| reduct.as_bool()),
        Some(None),
        "plain reduction takes no fold again"
    );

    let undecided = Term::intrinsic(Intrinsic::nat_lt(
        left,
        Term::intrinsic(Intrinsic::NatMul(right, Term::free_var(&asked.c))),
    ));
    let plain = curios_analysis::Env::force(&mut context, &undecided).ok();
    assert_eq!(reduce(&mut context, undecided).ok(), plain);
    assert!(plain.is_some());
}

/// A fold inside a question is not taken again, a question being answered by plain reduction: `h(f(a + b) == f(b + a)) == h(true)` stays stuck. The kernel's `whnf::question_tests` holds this proposition under this name.
///
/// Mutation-checked: with the fold taken again under plain reduction too, the question decides the argument and the comparison is `true`.
#[test]
fn a_fold_inside_a_question_is_not_taken_again() {
    let mut context = context();
    let asked = Asked::over(&mut context);
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
        reduce(&mut context, equal).map(|reduct| reduct.as_bool()),
        Ok(Some(true)),
        "one question deep the fold is decided, or the refusal below proves nothing"
    );
    assert_eq!(
        reduce(&mut context, nested).map(|reduct| reduct.as_bool()),
        Ok(None)
    );
}

/// A stuck elimination takes the arm the carriers' algebra reads its scrutinee as, in both reductions: a `Bool` match on `(a < b && c < b) && ¬(a < b)` is its false arm and a structural `Nat` match on `(a + b + 1) * (b + a + 1)` its successor arm, and plain reduction takes both. The controls are a connective the decision diagram leaves open and a product with no floor, whose matches stand. The kernel's `whnf::question_tests` holds this proposition under this name.
#[test]
fn a_stuck_elimination_takes_the_arm_the_algebra_reads() {
    let mut context = context();
    let asked = Asked::over(&mut context);
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
    let nat_type = || Term::intrinsic(Intrinsic::NatType);
    let on_bool = |scrutinee: Term| Term::bool_match(scrutinee, None, nat_type(), nat(2), nat(1));
    let (pred, hypothesis) = (
        context.fresh(Some("pred")),
        context.fresh(Some("hypothesis")),
    );
    let on_nat = |scrutinee: Term| {
        Term::nat_match(
            scrutinee,
            None,
            nat_type(),
            nat(10),
            &pred,
            &hypothesis,
            nat(20),
        )
    };

    let contradiction = on_bool(and(and(less(&a, &b), less(&c, &b)), not(less(&a, &b))));
    let successor = on_nat(product(1));
    for decided in [&contradiction, &successor] {
        let arm = reduce(&mut context, decided.clone()).expect("reduces");
        assert!(arm.as_nat().is_some(), "{decided} stands as {arm}");
        assert_eq!(
            curios_analysis::Env::force(&mut context, decided).ok(),
            Some(arm),
            "plain reduction takes the arm too"
        );
    }
    assert_eq!(reduce(&mut context, contradiction), Ok(nat(2)));
    assert_eq!(reduce(&mut context, successor), Ok(nat(20)));

    for open in [on_bool(and(less(&a, &b), less(&c, &b))), on_nat(product(0))] {
        let stuck = reduce(&mut context, open.clone()).expect("reduces");
        assert!(stuck.as_nat().is_none(), "{open} takes an arm: {stuck}");
    }
}

/// A stuck elimination classes its scrutinee's atoms in a judgment's reduction alone: a `Bool` match on `(f(a + b) < c && b < c) && ¬(f(b + a) < c)`, a contradiction once the elaborator's conversion holds the two calls one, is its false arm, and plain reduction, which asks nothing, leaves it standing. The control is the same connective over a call on another argument. The kernel's `whnf::question_tests` holds this proposition under this name.
#[test]
fn a_stuck_elimination_classes_its_scrutinees_atoms_in_a_judgments_reduction_alone() {
    let mut context = context();
    let asked = Asked::over(&mut context);
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
        Term::bool_match(
            scrutinee,
            None,
            Term::intrinsic(Intrinsic::NatType),
            nat(2),
            nat(1),
        )
    };

    let classed = on(asked.call(&asked.b, &asked.a));
    assert_eq!(reduce(&mut context, classed.clone()), Ok(nat(2)));
    let plain = curios_analysis::Env::force(&mut context, &classed).ok();
    assert!(
        plain.as_ref().is_some_and(|plain| plain.as_nat().is_none()),
        "plain reduction classed {classed}"
    );

    let other = reduce(&mut context, on(asked.call(&asked.a, &asked.c))).expect("reduces");
    assert!(
        other.as_nat().is_none(),
        "a call on another argument classed: {other}"
    );
}

/// A guard over a local definition answers a term spelled over the definition's value: under `let n = a + 0` and the guard `f(n) < 5`, `f(a) < 5` is `true`. The kernel holds the guard by value, so the filter in front of its equation reads `a`; the elaborator's key names `n`, and its filter reads the scrutinee as the kernel spells it, or the form is put to no equation. `curios`'s `tests::matching` holds a program of this shape under this name, through both checkers.
///
/// Mutation-checked: with the filter reading the scrutinee as written, `f(a) < 5` stays stuck.
#[test]
fn a_guard_over_a_local_definition_answers_a_term_over_its_value() {
    let mut context = context();
    let asked = Asked::over(&mut context);
    let n = context.fresh(Some("n"));
    let over = |argument: Term| {
        Term::intrinsic(Intrinsic::nat_lt(
            Term::apply(Term::free_var(&asked.f), [argument]),
            nat(5),
        ))
    };
    let truth = Term::intrinsic(Intrinsic::Bool(true));

    context.with_frame(|context| {
        context.assume(&n, &Term::intrinsic(Intrinsic::NatType));
        context.define(
            &n,
            &Term::intrinsic(Intrinsic::nat_add(Term::free_var(&asked.a), nat(0))),
            None,
        );
        let guard = over(Term::free_var(&n));
        let key = shallow_scrutinee(context, &guard);
        context.refine_scrutinee_spellings(vec![(key, guard, false)], &truth);

        let by_value = over(Term::free_var(&asked.a));
        assert_eq!(
            reduce(context, by_value).map(|reduct| reduct.as_bool()),
            Ok(Some(true))
        );
        let other = over(Term::free_var(&asked.b));
        assert_eq!(
            reduce(context, other).map(|reduct| reduct.as_bool()),
            Ok(None)
        );
    });
}

/// One call at two level metavariables, and what holds them one: `w(g.{this}(a), a + b)` and `w(g.{that}(a), b + a)` are compared whole, the carriers' readers having no say over an application's first argument, so they are one term only to a conversion that holds `this = that`.
struct AtTwoLevels {
    asked: Asked,
    scrutinee: Term,
    respelled: Term,
    this: Level,
    that: Level,
}

impl AtTwoLevels {
    fn over(context: &mut Context) -> Self {
        let asked = Asked::over(context);
        let g = context.fresh(Some("g"));
        let w = context.fresh(Some("w"));
        let (x, y) = (context.fresh(Some("x")), context.fresh(Some("y")));
        let nat_type = Term::intrinsic(Intrinsic::NatType);
        context.assume(
            &w,
            &Term::func_type([(x, nat_type.clone()), (y, nat_type.clone())], nat_type),
        );
        let this = Level::meta(context.universes_mut().fresh(UniverseRole::Flexible, None));
        let that = Level::meta(context.universes_mut().fresh(UniverseRole::Flexible, None));
        let over = |level: &Level, left: &Free, right: &Free| {
            Term::apply(
                Term::free_var(&w),
                [
                    Term::apply(
                        Term::instance_of(&g, vec![level.clone()]),
                        [Term::free_var(&asked.a)],
                    ),
                    Term::intrinsic(Intrinsic::nat_add(
                        Term::free_var(left),
                        Term::free_var(right),
                    )),
                ],
            )
        };
        AtTwoLevels {
            scrutinee: over(&this, &asked.a, &asked.b),
            respelled: over(&that, &asked.b, &asked.a),
            asked,
            this,
            that,
        }
    }

    fn equate(&self, context: &mut Context) {
        context
            .universes_mut()
            .add_eq(
                self.this.clone(),
                self.that.clone(),
                UniverseConstraintOrigin::new(UniverseConstraintKind::Conversion),
            )
            .expect("two fresh levels may be one");
    }
}

/// A reduct taken across a question declined for a commit is taken again once the solver has moved. Under the equation of one call, its respelling at another level is the scrutinee only to a conversion that would commit a level constraint: the question is declined and the term stays stuck. Once the two levels are one it is the equation's value, which a stuck reduct remembered from the first reading would hide.
///
/// Mutation-checked both ways: with a reduct remembered whatever was declined while it was taken, and with a declined question never noted, the second reading is handed the first's.
#[test]
fn a_reduct_taken_across_a_declined_question_is_taken_again() {
    let mut context = context();
    let levels = AtTwoLevels::over(&mut context);
    let (scrutinee, respelled) = (levels.scrutinee.clone(), levels.respelled.clone());

    context.with_frame(|context| {
        let key = shallow_scrutinee(context, &scrutinee);
        context.refine_scrutinee_spellings(vec![(key, scrutinee.clone(), false)], &nat(0));

        assert_eq!(
            reduce(context, respelled.clone()),
            Ok(respelled.clone()),
            "the two calls are one only to a conversion that commits a level constraint"
        );
        levels.equate(context);
        assert_eq!(reduce(context, respelled.clone()), Ok(nat(0)));
    });
}

/// A reduct taken by a question answered on a level constraint goes with the scope that held the constraint. Inside a scope that holds the two levels one, the respelled call is the equation's scrutinee and reduces to its value; rolled back, the levels are apart again and the call is stuck, which a reduct remembered inside the scope would hide.
///
/// Mutation-checked: with no question noted where reduction asks whether a stuck form is an equation's scrutinee, the rollback keeps the reduct taken inside.
#[test]
fn a_reduct_that_rested_on_a_level_constraint_goes_with_the_scope_that_held_it() {
    let mut context = context();
    let levels = AtTwoLevels::over(&mut context);
    let (scrutinee, respelled) = (levels.scrutinee.clone(), levels.respelled.clone());

    context.with_frame(|context| {
        let key = shallow_scrutinee(context, &scrutinee);
        context.refine_scrutinee_spellings(vec![(key, scrutinee.clone(), false)], &nat(0));

        let mark = context.solution_mark();
        levels.equate(context);
        assert_eq!(reduce(context, respelled.clone()), Ok(nat(0)));
        context.rollback_solutions(mark);
        context.end_solutions(mark);

        assert_eq!(reduce(context, respelled.clone()), Ok(respelled.clone()));
    });
}

/// An equation's reduced spelling settled across a declined question is settled again once the solver has moved. The inner guard `w(g.{that}(a), b + a) + c < 5` reduces through the outer equation only where the two levels are one: settled while they are apart, its spelling keeps the call, and `c < 5` is no spelling of it. Once the levels are one the guard reduces to `c < 5`, which the spelling settled before would not show: the call and `c` are two atoms to any classing.
///
/// Mutation-checked both ways: with a settlement filed beside no solver state, and with one read whatever state it was filed beside, `c < 5` stays stuck after the levels are one.
#[test]
fn a_spelling_settled_across_a_declined_question_is_settled_again() {
    let mut context = context();
    let levels = AtTwoLevels::over(&mut context);
    let (scrutinee, respelled) = (levels.scrutinee.clone(), levels.respelled.clone());
    let c = Term::free_var(&levels.asked.c);
    let guard = Term::intrinsic(Intrinsic::nat_lt(
        Term::intrinsic(Intrinsic::nat_add(respelled, c.clone())),
        nat(5),
    ));
    let bound = Term::intrinsic(Intrinsic::nat_lt(c, nat(5)));
    let truth = Term::intrinsic(Intrinsic::Bool(true));
    let decided = |context: &mut Context, term: &Term| {
        reduce(context, term.clone()).map(|reduct| reduct.as_bool())
    };

    context.with_frame(|context| {
        let key = shallow_scrutinee(context, &scrutinee);
        context.refine_scrutinee_spellings(vec![(key, scrutinee.clone(), false)], &nat(0));

        context.with_frame(|context| {
            let key = shallow_scrutinee(context, &guard);
            context.refine_scrutinee_spellings(vec![(key, guard.clone(), false)], &truth);

            assert_eq!(
                decided(context, &bound),
                Ok(None),
                "the guard keeps its call while the levels are apart"
            );
            levels.equate(context);
            assert_eq!(decided(context, &bound), Ok(Some(true)));
        });
    });
}
