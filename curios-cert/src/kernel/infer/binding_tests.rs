//! A `let` in the kernel: bound for typing, and read by value by everything that reads a term for more than its type.

use {
    super::test_support::*,
    crate::{infer, whnf},
    curios_core::{Intrinsic, Rec, Reducer, Subterm, Term, Totality},
};

/// `match v | true => Nat | false => Bool`: a type that is `Nat` exactly where `v` is `true`, which every fixture here reads a `Bool` through.
fn family(v: Term) -> Term {
    Term::bool_match(v, None, Term::type_ground(), bool_type(), nat_type())
}

/// A `let`'s tail is typed over its binder — a variable there has the binder's type — and the `let`'s own type is stated by value, since the binder closes with it.
///
/// Mutation-checked: with conversion comparing its terms as they are handed, the second binding's annotation is a name reduction has no value for.
#[test]
fn a_lets_tail_is_typed_over_its_binder_and_its_type_stated_by_value() {
    let mut kernel = kernel();
    let alias = binder(0, "T");
    let y = binder(1, "y");

    let term = Term::let_block(
        vec![
            (alias, Term::type_ground(), nat_type()),
            (y, Term::free_var(&alias), nat(3)),
        ],
        Term::free_var(&y),
    );

    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
}

/// Reduction reads by value where typing enters it: a lambda checks against a `let`-bound function type, which the rule reduces to find its telescope.
///
/// Mutation-checked: with a forced reduction taking its term as it is handed, the annotation's name reaches the reducer.
#[test]
fn a_lambda_checks_against_a_let_bound_function_type() {
    let mut kernel = kernel();
    let alias = binder(0, "T");
    let f = binder(1, "f");
    let x = binder(2, "x");

    let arrow = Term::func_type([(x, nat_type())], nat_type());
    let term = Term::let_block(
        vec![
            (alias, Term::type_ground(), arrow.clone()),
            (
                f,
                Term::free_var(&alias),
                Term::func([(x, nat_type())], Term::free_var(&x)),
            ),
        ],
        Term::free_var(&f),
    );

    assert_eq!(infer(&mut kernel, &term), Ok(arrow));
}

/// An arm binds again the `let` its solution moves. `c` stands for `b`, and in the arm where `b` is `true` it stands for `true`: a substituted `let` would have had the solution substituted through its value, and a bound one is bound again at it.
///
/// Mutation-checked: with the arm leaving a moved `let` as it was, `family(c)` is stuck on `b` inside the arm that decided it.
#[test]
fn an_arm_binds_again_the_let_its_solution_moves() {
    let mut kernel = kernel();
    let b = binder(0, "b");
    let c = binder(1, "c");
    let z = binder(2, "z");

    let arm = Term::let_(&z, family(Term::free_var(&c)), nat(3), nat(0));
    let term = Term::func(
        [(b, bool_type())],
        Term::let_(
            &c,
            bool_type(),
            Term::free_var(&b),
            Term::bool_match(Term::free_var(&b), None, nat_type(), nat(0), arm),
        ),
    );

    assert!(infer(&mut kernel, &term).is_ok());
}

/// A `let` that stands for a moved `let` moves with it: `d` is `c` and `c` is `b`, so where `b` is `true` so is `d`. Each is bound again from its value as written, and `d` after `c`, so `d` reads `c` where it now stands.
///
/// Mutation-checked: with the `let`s bound again innermost first, `d` is read through the `c` the arm has not yet moved.
#[test]
fn a_let_over_a_moved_let_moves_with_it() {
    let mut kernel = kernel();
    let b = binder(0, "b");
    let c = binder(1, "c");
    let d = binder(2, "d");
    let z = binder(3, "z");

    let arm = Term::let_(&z, family(Term::free_var(&d)), nat(3), nat(0));
    let term = Term::func(
        [(b, bool_type())],
        Term::let_block(
            vec![
                (c, bool_type(), Term::free_var(&b)),
                (d, bool_type(), Term::free_var(&c)),
            ],
            Term::bool_match(Term::free_var(&b), None, nat_type(), nat(0), arm),
        ),
    );

    assert!(infer(&mut kernel, &term).is_ok());
}

/// And a `let` the solution re-types stays a `let`: bound again at its specialized type, it still stands for its value. `c` is `b` stated at a type that names `b`, and the arm reads `c`'s value through `family`.
///
/// Mutation-checked: with a re-typed `let` assumed at its new type, it stands for nothing and `family(c)` is stuck on it.
#[test]
fn a_let_an_arm_retypes_keeps_its_value() {
    let mut kernel = kernel();
    let b = binder(0, "b");
    let c = binder(1, "c");
    let v = binder(2, "v");
    let z = binder(3, "z");

    // `Bool` under either case, and stuck on a variable: a type that names `b` and says nothing more.
    let carrier =
        |at: Term| Term::bool_match(at, None, Term::type_ground(), bool_type(), bool_type());
    let value = Term::bool_match(
        Term::free_var(&b),
        Some(&v),
        carrier(Term::free_var(&v)),
        Term::intrinsic(Intrinsic::Bool(false)),
        Term::intrinsic(Intrinsic::Bool(true)),
    );
    let arm = Term::let_(&z, family(Term::free_var(&c)), nat(3), nat(0));
    let term = Term::func(
        [(b, bool_type())],
        Term::let_(
            &c,
            carrier(Term::free_var(&b)),
            value,
            Term::bool_match(Term::free_var(&b), None, nat_type(), nat(0), arm),
        ),
    );

    assert!(infer(&mut kernel, &term).is_ok());
}

/// A `let` bound again is typed again: what was remembered of a term naming it was read off the type and the value it had, and an arm that moves it gives it others. `w` is typed at `family(b)` before the match, and is a `Nat` in the arm where `b` is `true`.
///
/// Mutation-checked: with the typings left standing where a `let` is bound again, `w` answers inside the arm at the type it had outside.
#[test]
fn a_let_bound_again_is_typed_again() {
    let mut kernel = kernel();
    let b = binder(0, "b");
    let w = binder(1, "w");
    let before = binder(2, "before");
    let v = binder(3, "v");
    let z = binder(4, "z");

    let value = Term::bool_match(
        Term::free_var(&b),
        Some(&v),
        family(Term::free_var(&v)),
        Term::intrinsic(Intrinsic::Bool(true)),
        nat(3),
    );
    let arm = Term::let_(&z, nat_type(), Term::free_var(&w), nat(0));
    let term = Term::func(
        [(b, bool_type())],
        Term::let_block(
            vec![
                (w, family(Term::free_var(&b)), value),
                (before, family(Term::free_var(&b)), Term::free_var(&w)),
            ],
            Term::bool_match(Term::free_var(&b), None, nat_type(), nat(0), arm),
        ),
    );

    assert!(infer(&mut kernel, &term).is_ok());
}

/// An elimination reads its scrutinee by value: `x` stands for `g(b)`, so the arm's equation is `g(b) = true`, kept under the spelling a substituted `let` leaves, and it answers `g(b)` written out in the arm.
///
/// Mutation-checked: with the scrutinee read as it is written, `x` is a variable the arm solves, and nothing is known of `g(b)`.
#[test]
fn an_elimination_reads_a_let_bound_scrutinee_by_value() {
    let mut kernel = kernel();
    let g = binder(0, "g");
    let b = binder(1, "b");
    let x = binder(2, "x");
    let z = binder(3, "z");
    let guard = Term::apply(Term::free_var(&g), [Term::free_var(&b)]);

    let arm = Term::let_(&z, family(guard.clone()), nat(3), nat(0));
    let term = Term::func(
        [
            (
                g,
                Term::func_type([(binder(4, "v"), bool_type())], bool_type()),
            ),
            (b, bool_type()),
        ],
        Term::let_(
            &x,
            bool_type(),
            guard,
            Term::bool_match(Term::free_var(&x), None, nat_type(), nat(0), arm),
        ),
    );

    assert!(infer(&mut kernel, &term).is_ok());
}

/// A recorded position states what its term stands for: the obligations read a position for what it reaches, and a `let`-bound local's name reaches nothing. `h` stands for the proof `p`, and the position the tail records is `p`.
///
/// Mutation-checked: with a position recorded as it is written, one names the binder the kernel minted for `h`.
#[test]
fn a_recorded_position_is_stated_by_value() {
    let mut kernel = kernel();
    let proposition = binder(0, "P");
    let proof = binder(1, "p");
    let h = binder(2, "h");
    kernel.assume(&proposition, &Term::prop());
    kernel.assume(&proof, &Term::free_var(&proposition));

    let term = Term::let_(
        &h,
        Term::free_var(&proposition),
        Term::free_var(&proof),
        Term::free_var(&h),
    );
    infer(&mut kernel, &term).expect("the `let` is typed");
    let (positions, failure) = kernel.take_checked();

    assert!(failure.is_none());
    assert!(!positions.is_empty());
    for position in &positions {
        assert!(
            position
                .term
                .free_vars()
                .iter()
                .all(|name| *name == proposition || *name == proof),
            "a position names a binder the `let` opened: {}",
            position.term
        );
    }
}

/// A position that reaches a group that does not descend says so, though the group was typed where a `let` binds it and not inside the position's term: `stuck` is a `Bool` that never arrives, and the type stated over it holds the group once it is read by value.
///
/// Mutation-checked: with no `let`-bound local read as enclosing such a group, the type over `stuck` is recorded as reaching none.
#[test]
fn a_position_over_a_let_bound_partial_group_says_so() {
    let mut kernel = kernel();
    let f = binder(0, "f");
    let stuck = binder(1, "stuck");
    let alias = binder(2, "T");

    let looping = Term::rec([(f, bool_type(), Term::free_var(&f))], Term::free_var(&f));
    let term = Term::let_block(
        vec![
            (stuck, bool_type(), looping),
            (alias, Term::type_ground(), family(Term::free_var(&stuck))),
        ],
        nat(0),
    );
    infer(&mut kernel, &term).expect("the `let` is typed");
    let (positions, _) = kernel.take_checked();
    let over_the_group = positions
        .iter()
        .filter(|position| position.term.has_group())
        .collect::<Vec<_>>();

    assert!(!over_the_group.is_empty());
    assert!(
        over_the_group
            .iter()
            .all(|position| position.encloses_partial)
    );
}

/// A call is graded on what it passes: `k` stands for the predecessor, so `countdown(k)` descends as `countdown(pred)` does.
///
/// Mutation-checked: with a call's arguments graded as they are written, `k` is a binder nothing is known of and the group does not descend.
#[test]
fn a_call_through_a_let_bound_argument_is_graded_by_value() {
    let mut kernel = kernel();
    let countdown = binder(0, "countdown");
    let n = binder(1, "n");
    let motive = binder(2, "m");
    let pred = binder(3, "pred");
    let hypothesis = binder(4, "ih");
    let k = binder(5, "k");

    let signature = Term::func_type([(n, nat_type())], nat_type());
    let body = Term::func(
        [(n, nat_type())],
        Term::nat_match(
            Term::free_var(&n),
            Some(&motive),
            nat_type(),
            nat(0),
            &pred,
            &hypothesis,
            Term::let_(
                &k,
                nat_type(),
                Term::free_var(&pred),
                Term::apply(Term::free_var(&countdown), [Term::free_var(&k)]),
            ),
        ),
    );
    let term = Term::rec(
        [(countdown, signature, body)],
        Term::apply(Term::free_var(&countdown), [nat(3)]),
    );
    let Subterm::Rec(Rec { group, .. }) = &*term else {
        panic!("the fixture changed shape");
    };

    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
    assert_eq!(kernel.group_verdict(group), Some(Totality::Total));
}

/// A recursive group is read by value: its verdict is filed under the group a substituted `let` leaves, which is the group a recorded position holds and the obligations look up.
///
/// Mutation-checked: with the group checked as it is written, its verdict is filed under a spelling that names the binder the kernel minted for `k`.
#[test]
fn a_group_under_a_let_is_filed_by_value() {
    let mut kernel = kernel();
    let k = binder(0, "k");
    let f = binder(1, "f");

    let group_over = |value: Term| Term::rec([(f, nat_type(), value)], Term::free_var(&f));
    let term = Term::let_(&k, nat_type(), nat(5), group_over(Term::free_var(&k)));
    let by_value = group_over(nat(5));
    let Subterm::Rec(Rec { group, .. }) = &*by_value else {
        panic!("the fixture changed shape");
    };

    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
    assert_eq!(kernel.group_verdict(group), Some(Totality::Total));
}

/// Reduction reads by value where it is entered: a `let`-bound name gives way to what it stands for.
///
/// Mutation-checked: with a reduction taking its term as it is handed, the name reaches the reducer, which has no value for it.
#[test]
fn reduction_reads_a_let_bound_name_by_value() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let reduct = kernel.scoped(|kernel| {
        kernel.bind(&x, &nat_type(), &nat(2), false);
        whnf(kernel, Term::free_var(&x))
    });

    assert_eq!(reduct.ok(), Some(nat(2)));
}

/// A remembered reduct is keyed on its term by value, so it never rests on what a `let`-bound name stood for when it was taken. An arm binds `x` again at another value; the reduct tables carry no binder and are not cleared when the arm is left, so an entry filed under a spelling that names `x` would answer for the outer `x` with what the inner one stood for.
///
/// Mutation-checked: with a forced reduction asking its table about the term as it is handed, the reduct taken under the arm's binding answers after the arm.
#[test]
fn a_remembered_reduct_does_not_rest_on_a_lets_binding() {
    let mut kernel = kernel();
    let n = binder(0, "n");
    let x = binder(1, "x");
    kernel.assume(&n, &nat_type());
    let sum = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), Term::free_var(&n)));
    let at = |value: usize| Term::intrinsic(Intrinsic::nat_add(nat(value), Term::free_var(&n)));

    kernel.scoped(|kernel| {
        kernel.bind(&x, &nat_type(), &nat(2), false);
        let inside = kernel.scoped(|kernel| {
            kernel.bind(&x, &nat_type(), &nat(3), false);
            kernel.reduce_forced(sum.clone())
        });
        let outside = kernel.reduce_forced(sum.clone());

        assert_eq!(inside.ok(), kernel.reduce_forced(at(3)).ok());
        assert_eq!(outside.ok(), kernel.reduce_forced(at(2)).ok());
    });
}
