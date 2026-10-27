//! The carriers' classification and the case equations through a real checker: atoms compared in whichever order they rank, a connective leaf as written, and an equation answering a term its readers hold equal to its key.

use {
    super::test_support::kernel,
    curios_analysis::{
        Answered, Classes, Judge, ScrutineeRead, answers, answers_classed, group_totality,
        holds_no_type, one_up_to_instances, read_scrutinee, test_support::SYNTAX,
    },
    curios_cert::Kernel,
    curios_core::{
        Cases, Exhaustion, Free, Intrinsic, Level, Match, Nat, Rec, Reducer, Subterm, Term,
        Totality,
    },
    curios_num::{Integer, Natural, Rounding},
};

/// Reading an argument's size through what stands before it is a probe: where the budget cannot afford the reading, the engine answers with the refusal. Reading the argument as nothing would classify the group `Partial`, a verdict with no cause in the term that the drivers would report as non-termination. The control is the same group with the budget to read it.
#[test]
fn a_size_the_budget_cannot_read_refuses_the_classification() {
    let nat = || Term::intrinsic(Intrinsic::NatType);
    let f = Free::local(2, Some("f"));
    let n = Free::local(3, Some("n"));
    let v = Free::local(4, Some("v"));
    let identity = Term::func([(v, nat())], Term::free_var(&v));
    let rec = Term::rec(
        vec![(
            f,
            Term::func_type([(n, nat())], nat()),
            Term::func(
                [(n, nat())],
                Term::apply(
                    Term::free_var(&f),
                    [Term::apply(identity, [Term::free_var(&n)])],
                ),
            ),
        )],
        Term::free_var(&f),
    );
    let Subterm::Rec(Rec { group, .. }) = &*rec else {
        panic!("the fixture changed shape");
    };

    let mut starved = Kernel::new(0, SYNTAX);
    assert!(group_totality(&mut starved, group).is_err_and(|error| error.is_exhausted()));
    assert_eq!(group_totality(&mut kernel(), group), Ok(Totality::Partial));
}

/// The conversion chain's respellings are probes: a connective's leaf with no value at the type level is read as written, an atom like any other, rather than turning the comparison into the leaf's own failure. The two sides differ by a unit the decision diagram sees through once the leaf is an atom.
#[test]
fn a_connective_leaf_with_no_value_is_compared_as_written() {
    let mut kernel = kernel();
    let b = Free::local(900, Some("b"));
    let x = Free::local(901, Some("x"));
    let boolean = Term::intrinsic(Intrinsic::BoolType);
    kernel.assume(&b, &boolean);
    kernel.assume(&x, &Term::intrinsic(Intrinsic::NatType));
    let nat = |k: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(k)));
    let undefined = Term::intrinsic(Intrinsic::NatEql(
        Term::intrinsic(Intrinsic::NatDiv {
            dividend: nat(1),
            divisor: nat(0),
            non_zero: Term::free_var(&x),
        }),
        nat(0),
    ));
    let this = Term::intrinsic(Intrinsic::BoolAnd(Term::free_var(&b), undefined.clone()));
    let that = Term::intrinsic(Intrinsic::BoolAnd(
        Term::free_var(&b),
        Term::intrinsic(Intrinsic::BoolAnd(
            undefined,
            Term::intrinsic(Intrinsic::Bool(true)),
        )),
    ));

    assert_eq!(kernel.convert_at(&boolean, &this, &that), Ok(true));
}

/// Conversion's readers key atoms on spelling, so two sums of atoms that commute a sum inside their arguments would meet only where the positional congruence pairs them, and an `Int` sum keeps the order it was written in: `h(i + j) + e(k + l)` against `e(l + k) + h(j + i)` would pair `h` with `e` and be refused, whichever identities the heads hold. A pair the chain decides nothing about is read once more with every atom's arguments forced, and the cancellation then pairs them. The control keeps one argument genuinely different, and stays apart.
#[test]
fn atoms_equal_up_to_their_arguments_convert_in_whichever_order_their_sum_holds_them() {
    let int = || Term::intrinsic(Intrinsic::IntType);
    let [i, j, k, l] = [1, 2, 3, 4].map(|index| Free::local(index, None));
    let sum = |left: Term, right: Term| Term::intrinsic(Intrinsic::IntAdd(left, right));
    let of = |left: &Free, right: &Free| sum(Term::free_var(left), Term::free_var(right));

    for (h_index, e_index) in [(10, 11), (11, 10)] {
        let mut kernel = kernel();
        let h = Free::local(h_index, Some("h"));
        let e = Free::local(e_index, Some("e"));
        for name in [i, j, k, l] {
            kernel.assume(&name, &int());
        }
        let function = Term::func_type([(Free::local(99, None), int())], int());
        kernel.assume(&h, &function);
        kernel.assume(&e, &function);
        let at = |head: &Free, argument: Term| Term::apply(Term::free_var(head), [argument]);

        let this = sum(at(&h, of(&i, &j)), at(&e, of(&k, &l)));
        let that = sum(at(&e, of(&l, &k)), at(&h, of(&j, &i)));
        let apart = sum(at(&e, of(&l, &k)), at(&h, of(&j, &j)));

        assert_eq!(
            kernel.convert_at(&int(), &this, &that),
            Ok(true),
            "h at {h_index}, e at {e_index}"
        );
        assert_eq!(
            kernel.convert_at(&int(), &this, &apart),
            Ok(false),
            "h at {h_index}, e at {e_index}"
        );
    }
}

/// A magnitude orients its operand by the order of its atoms, and two spellings of one call of two shapes rank apart: `abs(h(i + j) - e(n))` leads with the call or with `e(n)` as their ranks fall, and `abs(h((j + i) + 0) - e(n))` as the other spelling's does, so wherever the two fall on two sides of `e(n)` the folds lead apart and the congruence compares a difference with its negation. Classed, the pair is read with the fold taken again over one spelling of the call, and both checkers' magnitudes are led alike; `sign` the same, its fold drawing a negation out. The literals `n` sweep the places `e(n)` ranks at. The control holds one argument genuinely different, and stays apart.
///
/// Mutation-checked: with no magnitude's fold taken again by the classification, the kernel refuses the two `abs`es at some `n` of the sweep.
#[test]
fn a_magnitude_over_two_spellings_of_its_atoms_converts_whichever_way_they_rank() {
    let int = || Term::intrinsic(Intrinsic::IntType);
    let literal = |n: i32| Term::intrinsic(Intrinsic::Int(Integer::from(n)));
    let [i, j] = [1, 2].map(|index| Free::local(index, None));
    let [h, e] = [(10, "h"), (11, "e")].map(|(index, hint)| Free::local(index, Some(hint)));
    let mut kernel = kernel();
    for name in [i, j] {
        kernel.assume(&name, &int());
    }
    let function = Term::func_type([(Free::local(99, None), int())], int());
    kernel.assume(&h, &function);
    kernel.assume(&e, &function);
    let sum = |left: Term, right: Term| Term::intrinsic(Intrinsic::IntAdd(left, right));
    let (i, j) = (Term::free_var(&i), Term::free_var(&j));
    let arguments = [
        sum(i.clone(), j.clone()),
        sum(sum(j, i.clone()), literal(0)),
        sum(i.clone(), i),
    ];

    for n in 0..30 {
        let [first, second, apart] = arguments.clone().map(|argument| {
            Term::intrinsic(Intrinsic::IntSub(
                Term::apply(Term::free_var(&h), [argument]),
                Term::apply(Term::free_var(&e), [literal(n)]),
            ))
        });

        let nat = Term::intrinsic(Intrinsic::NatType);
        let abs = |operand: &Term| Term::intrinsic(Intrinsic::IntAbs(operand.clone()));
        assert_eq!(
            kernel.convert_at(&nat, &abs(&first), &abs(&second)),
            Ok(true),
            "abs, e({n})"
        );
        assert_eq!(
            kernel.convert_at(&nat, &abs(&first), &abs(&apart)),
            Ok(false),
            "abs, e({n})"
        );
        let sign = |operand: &Term| Term::intrinsic(Intrinsic::IntSign(operand.clone()));
        assert_eq!(
            kernel.convert_at(&int(), &sign(&first), &sign(&second)),
            Ok(true),
            "sign, e({n})"
        );
    }
}

/// A fused multiply-add's two factors pair in either order, as a commutative operation's operands do, and its addends are compared in place. The factors here convert without being identical, so nothing pairs until a classing spells them alike; then one factor of each side is one term, and what is left is the other two factors and the two addends. An addend is no factor, and two rounding directions are two operations.
#[test]
fn a_fused_multiply_add_pairs_its_factors_and_keeps_its_addend_in_place() {
    let flt = || Term::intrinsic(Intrinsic::FltType);
    let nat = || Term::intrinsic(Intrinsic::NatType);
    let mut kernel = kernel();
    let [x, y] = [1, 2].map(|index| Free::local(index, None));
    let [b, c] = [3, 4].map(|index| Free::local(index, None));
    let h = Free::local(10, Some("h"));
    for name in [x, y] {
        kernel.assume(&name, &nat());
    }
    for name in [b, c] {
        kernel.assume(&name, &flt());
    }
    kernel.assume(
        &h,
        &Term::func_type([(Free::local(99, None), nat())], flt()),
    );
    let at = |left: &Free, right: &Free| {
        let sum = Term::intrinsic(Intrinsic::NatAdd(
            Term::free_var(left),
            Term::free_var(right),
        ));
        Term::apply(Term::free_var(&h), [sum])
    };
    let fused = |rounding: Rounding, left: Term, right: Term, addend: &Free| {
        Term::intrinsic(Intrinsic::FltFma(
            rounding,
            left,
            right,
            Term::free_var(addend),
        ))
    };
    let toward_zero = Rounding::TowardZero;

    let this = fused(toward_zero, at(&x, &y), Term::free_var(&b), &c);
    let swapped = fused(toward_zero, Term::free_var(&b), at(&y, &x), &c);
    assert_eq!(kernel.convert_at(&flt(), &this, &swapped), Ok(true));

    let addend_for_factor = fused(toward_zero, Term::free_var(&c), at(&y, &x), &b);
    assert_eq!(
        kernel.convert_at(&flt(), &this, &addend_for_factor),
        Ok(false)
    );
    let another_direction = fused(Rounding::TowardPositive, Term::free_var(&b), at(&y, &x), &c);
    assert_eq!(
        kernel.convert_at(&flt(), &this, &another_direction),
        Ok(false)
    );
}

/// One rule says which terms an arm's equation answers, and both reducers ask it: the key itself, a term the carriers' readers hold equal to the key, and — where the equation assumes a `Bool` — a term they hold equal to the key negated, the value negated on the way.
///
/// The controls are what the rule must not answer: a comparison over another atom, and a term of another kind than the key's.
#[test]
fn an_equation_answers_a_term_the_readers_hold_equal_to_its_key() {
    let mut kernel = kernel();
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let [a, b, c] = [1, 2, 3].map(|index| Free::local(index, None));
    for binder in [&a, &b, &c] {
        kernel.assume(binder, &nat_type);
    }
    let nat = |value: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(value)));
    let sum = |left: &Free, right: &Free| {
        Term::intrinsic(Intrinsic::nat_add(
            Term::free_var(left),
            Term::free_var(right),
        ))
    };
    let truth = |value: bool| Term::intrinsic(Intrinsic::Bool(value));

    let key = Term::intrinsic(Intrinsic::nat_lt(sum(&a, &b), nat(10)));
    let commuted = Term::intrinsic(Intrinsic::nat_lt(sum(&b, &a), nat(10)));
    let dual = Term::intrinsic(Intrinsic::nat_lte(nat(10), sum(&b, &a)));
    let other = Term::intrinsic(Intrinsic::nat_lt(sum(&a, &c), nat(10)));

    assert_eq!(
        answers(&mut kernel, &key, &key, &truth(true)),
        Ok(Answered::Value(truth(true)))
    );
    assert_eq!(
        answers(&mut kernel, &commuted, &key, &truth(true)),
        Ok(Answered::Value(truth(true))),
        "a sum commuted is the key's comparison"
    );
    assert_eq!(
        answers(&mut kernel, &dual, &key, &truth(true)),
        Ok(Answered::Value(truth(false))),
        "the dual of a guard that holds does not"
    );
    assert_eq!(
        answers(&mut kernel, &dual, &key, &truth(false)),
        Ok(Answered::Value(truth(true))),
        "and the dual of one that fails holds"
    );
    assert_eq!(
        answers(&mut kernel, &other, &key, &truth(true)),
        Ok(Answered::Silent),
        "a comparison over another atom is another comparison"
    );

    // A key that is a number answers a number the readers hold equal to it, and nothing of another kind.
    let total = sum(&a, &b);
    assert_eq!(
        answers(&mut kernel, &sum(&b, &a), &total, &nat(0)),
        Ok(Answered::Value(nat(0)))
    );
    assert_eq!(
        answers(&mut kernel, &commuted, &total, &nat(0)),
        Ok(Answered::Silent)
    );
}

/// One rule says what an elimination reads a stuck scrutinee as, and both reducers ask it: a `Bool` connective as the literal its decision diagram is at every assignment of its atoms — a contradiction across connectives `false`, a tautology `true` — and a structural `Nat` with no floor as the successor its products of two sums distribute to.
///
/// The controls are what the rule must not answer: a connective the diagram leaves open, a product whose distribution has no floor, the same product under a literal switch, and a connective under a match of another kind.
#[test]
fn an_elimination_reads_its_scrutinee_as_the_algebra_decides_it() {
    let mut kernel = kernel();
    let [b, c, x, y] = [1, 2, 3, 4].map(|index| Free::local(index, None));
    for binder in [&b, &c] {
        kernel.assume(binder, &Term::intrinsic(Intrinsic::BoolType));
    }
    for binder in [&x, &y] {
        kernel.assume(binder, &Term::intrinsic(Intrinsic::NatType));
    }
    let [b, c, x, y] = [b, c, x, y].map(|binder| Term::free_var(&binder));
    let truth = |value: bool| Term::intrinsic(Intrinsic::Bool(value));
    let nat = |value: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(value)));
    let and = |left: Term, right: Term| Term::intrinsic(Intrinsic::BoolAnd(left, right));
    let or = |left: Term, right: Term| Term::intrinsic(Intrinsic::BoolOr(left, right));
    let not = |term: Term| Term::intrinsic(Intrinsic::BoolXor(term, truth(true)));
    let add = |left: Term, right: Term| Term::intrinsic(Intrinsic::nat_add(left, right));
    let product = |floor: usize| {
        let sum = |first: &Term, second: &Term| add(add(first.clone(), second.clone()), nat(floor));
        Term::intrinsic(Intrinsic::nat_mul(sum(&x, &y), sum(&y, &x)))
    };
    let cases_of = |matched: Term| match &*matched {
        Subterm::Match(Match { cases, .. }) => cases.clone(),
        _ => unreachable!("a match"),
    };
    let nat_type = || Term::intrinsic(Intrinsic::NatType);
    let bools = cases_of(Term::bool_match(
        truth(true),
        None,
        nat_type(),
        nat(2),
        nat(1),
    ));
    let naturals = cases_of(Term::nat_match(
        nat(0),
        None,
        nat_type(),
        nat(10),
        &Free::local(5, Some("pred")),
        &Free::local(6, Some("hypothesis")),
        nat(20),
    ));
    let switch = Cases::Switch {
        cases: vec![(Natural::from(0usize), nat(10))],
        default: nat(20),
    };
    // The scrutinee as the kernel forces it, and what the rule reads it as.
    let mut read = |cases: &Cases, scrutinee: Term| {
        let forced = kernel
            .reduce_forced(scrutinee)
            .expect("the kernel forces it");
        match read_scrutinee(&mut kernel, cases, &forced).expect("the kernel reads it") {
            ScrutineeRead::Value(value) => Some(value),
            ScrutineeRead::Silent | ScrutineeRead::Atoms(_) => None,
        }
    };

    assert_eq!(
        read(&bools, and(and(b.clone(), c.clone()), not(b.clone()))),
        Some(truth(false)),
        "a contradiction across connectives"
    );
    assert_eq!(
        read(&bools, or(b.clone(), not(and(b.clone(), c.clone())))),
        Some(truth(true)),
        "a tautology across connectives"
    );
    assert!(
        read(&naturals, product(1)).is_some_and(|value| !Nat::decompose(&value).0.is_zero()),
        "a product of two sums with a floor is a successor"
    );
    assert!(
        read(&naturals, add(product(1), x.clone()))
            .is_some_and(|value| !Nat::decompose(&value).0.is_zero()),
        "and so is a sum that holds one"
    );

    assert_eq!(read(&bools, and(b.clone(), c.clone())), None);
    assert_eq!(read(&naturals, product(0)), None);
    assert_eq!(read(&switch, product(1)), None);
    assert_eq!(read(&naturals, and(and(b.clone(), c), not(b))), None);
}

/// A stuck call the readers decide nothing of is handed back whole, and where it and the key are two calls of one head apart in its universe instance alone, a checker asks about them by their arguments, by the statement a classing reads: an instance is no part of a truth value, so `f.{1}(a)` is the term of a key `f.{0}(a)` whose equation assumes `true`, and `f.{1}(b)` is not.
#[test]
fn a_call_apart_from_the_key_in_its_instance_alone_is_asked_about_by_its_arguments() {
    let mut kernel = kernel();
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let [a, b] = [1, 2].map(|index| Free::local(index, None));
    for binder in [&a, &b] {
        kernel.assume(binder, &nat_type);
    }
    let called = |level: u32, argument: &Free| {
        Term::apply(
            Term::instance_of(&Free::local(10, Some("f")), vec![Level::constant(level)]),
            [Term::free_var(argument)],
        )
    };
    let truth = Term::intrinsic(Intrinsic::Bool(true));
    let key = called(0, &a);
    assert!(holds_no_type(&truth));

    for (term, one) in [(called(1, &a), true), (called(1, &b), false)] {
        assert_eq!(
            answers(&mut kernel, &term, &key, &truth),
            Ok(Answered::Whole)
        );
        let asked = one_up_to_instances(&term, &key, &mut |this, that| {
            kernel.convert_at(&nat_type, this, that)
        })
        .expect("the kernel compares two locals");
        assert_eq!(asked, one);
    }
}

/// Past what the readers decide, the rule hands a question back and calls no judgment: the atoms of two operations of one kind, where some two may be one, and the whole pair where the two are no operations of the algebra and their heads say they may be one term. Asked again with the atoms classed, it reads the pair one spelling to a class, for the key and for the key negated. What cannot be one term is answered with silence, so a checker is asked nothing about it.
///
/// The judge here stands for a checker's conversion and says every pair it is asked about is one, so what is held is the rule's half: which pairs it asks about, and what it makes of the answer.
#[test]
fn an_equation_hands_back_what_its_checker_is_to_ask() {
    let mut kernel = kernel();
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let [a, b, f, g, x] = [1, 2, 3, 4, 5].map(|index| Free::local(index, None));
    for binder in [&a, &b] {
        kernel.assume(binder, &nat_type);
    }
    let unary = Term::func_type([(x, nat_type.clone())], nat_type);
    for binder in [&f, &g] {
        kernel.assume(binder, &unary);
    }
    let nat = |value: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(value)));
    let truth = |value: bool| Term::intrinsic(Intrinsic::Bool(value));
    let call = |head: &Free, left: &Free, right: &Free| {
        let sum = Intrinsic::nat_add(Term::free_var(left), Term::free_var(right));
        Term::apply(Term::free_var(head), [Term::intrinsic(sum)])
    };

    let key = Term::intrinsic(Intrinsic::nat_lt(call(&f, &a, &b), nat(10)));
    let respelled = Term::intrinsic(Intrinsic::nat_lt(call(&f, &b, &a), nat(10)));
    let dual = Term::intrinsic(Intrinsic::nat_lte(nat(10), call(&f, &b, &a)));
    let other = Term::intrinsic(Intrinsic::nat_lt(call(&g, &b, &a), nat(10)));

    let Ok(Answered::Atoms(atoms)) = answers(&mut kernel, &respelled, &key, &truth(true)) else {
        panic!("two calls of one head may be one, and which is the checker's to say");
    };
    assert!(atoms.contains(&call(&f, &b, &a)) && atoms.contains(&call(&f, &a, &b)));
    assert_eq!(
        answers(&mut kernel, &other, &key, &truth(true)),
        Ok(Answered::Silent),
        "two heads are two atoms to every checker"
    );

    let Ok(mut one) = Classes::of(&atoms, |_, _| Ok::<_, std::convert::Infallible>(true));
    assert_eq!(
        answers_classed(&mut kernel, &respelled, &key, &truth(true), &mut one),
        Ok(Some(truth(true)))
    );
    assert_eq!(
        answers_classed(&mut kernel, &dual, &key, &truth(true), &mut one),
        Ok(Some(truth(false))),
        "the dual over the classed atom is the key negated"
    );
    let Ok(mut apart) = Classes::of(&atoms, |_, _| Ok::<_, std::convert::Infallible>(false));
    assert_eq!(
        answers_classed(&mut kernel, &respelled, &key, &truth(true), &mut apart),
        Ok(None),
        "atoms a checker holds apart leave the pair as it was"
    );

    // No operation of the algebra on either side: the whole pair is the question, or there is none.
    assert_eq!(
        answers(&mut kernel, &call(&f, &b, &a), &call(&f, &a, &b), &nat(0)),
        Ok(Answered::Whole)
    );
    assert_eq!(
        answers(&mut kernel, &call(&g, &b, &a), &call(&f, &a, &b), &nat(0)),
        Ok(Answered::Silent)
    );
    assert_eq!(
        answers(&mut kernel, &Term::free_var(&a), &call(&f, &a, &b), &nat(0)),
        Ok(Answered::Silent)
    );
    // Nor two formers: a spelling still to be unfolded is put to nothing until it is reduced, or every stuck form beside it would be a question.
    let projected = Term::proj(Term::free_var(&a), 0);
    assert_eq!(
        answers(&mut kernel, &projected, &call(&f, &a, &b), &nat(0)),
        Ok(Answered::Silent)
    );
}

/// A long sum of stuck operations, its later summands each a commuted spelling of an earlier one, reduces to one reduct with the kernel's memos and without: what each stuck fold's classification files for the fold above it to extend is what a walk of the whole operation would take (`curios_analysis::Classification`).
#[test]
fn a_spine_of_stuck_folds_reduces_alike_with_its_classifications_kept_and_without() {
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let locals = (0..24)
        .map(|at| Free::local(500 + at, Some("x")))
        .collect::<Vec<_>>();
    let and = |left: &Free, right: &Free| {
        Term::intrinsic(Intrinsic::NatAnd(
            Term::free_var(left),
            Term::free_var(right),
        ))
    };
    let summands = locals
        .chunks(2)
        .map(|pair| and(&pair[0], &pair[1]))
        .chain(locals.chunks(2).rev().map(|pair| and(&pair[1], &pair[0])));
    let sum = summands
        .reduce(|left, right| Term::intrinsic(Intrinsic::nat_add(left, right)))
        .expect("a summand");
    let difference = Term::intrinsic(Intrinsic::nat_sub(Term::free_var(&locals[0]), sum.clone()));

    let reduced = |mut kernel: Kernel, term: &Term| {
        for local in &locals {
            kernel.assume(local, &nat_type);
        }
        kernel.reduce_forced(term.clone())
    };
    for term in [sum, difference] {
        assert_eq!(
            reduced(Kernel::new(10_000_000, SYNTAX), &term),
            reduced(Kernel::uncached(10_000_000, SYNTAX), &term),
            "{term}"
        );
    }
}
