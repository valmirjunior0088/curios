//! The calls the certifier records while it types a group's bodies, held to the gate a type-yielding member must pass: each fixture's only recursive call is one the recorder must see, graded under one kind of context it must build.
//!
//! A member whose declared type yields a sort is erased, so it must descend — the local gate `check_group` applies — and a group's calls are the only thing that gate reads. A call the recorder missed would be an edge the closure never saw, and a group with no edges is accepted, so every fixture below that refuses is the evidence that the call is recorded at all, and every one that accepts is the evidence that the context it was graded under was built.

use {
    crate::{test_support::*, *},
    curios_analysis::{Segment, test_support::*},
    curios_common::fill,
    curios_core::{Subterm, Term, Totality},
};

fn certifier() -> Certifier<'static> {
    Certifier::new(100_000, SYNTAX)
}

/// The group `rec f(n : Nat) -> Type = ${body}`, as the term that states it and names `f` in its tail: the member every fixture defines.
const GROUP: &str = "#{node = Rec} (let f: (n: #NatType) -> Type = (n: #NatType) => ${body}; f)";

/// [`GROUP`] at `body`.
fn group(body: &str) -> Term {
    term(&fill!(GROUP, body))
}

/// The two-arm split of `${scrutinee}` at `Type`: `Nat` when it is zero, `${cons}` when it is `pred + 1`.
const SPLIT: &str = "#{goal = Type} match ${scrutinee} | 0 => #NatType | pred + 1 => ${cons} end";

/// [`SPLIT`] of `scrutinee` with `cons` in its successor arm.
fn split(scrutinee: &str, cons: &str) -> String {
    fill!(SPLIT, scrutinee, cons)
}

/// The control every accepting fixture below is read against: a call at the caller's own argument is a cycle with nothing decreasing, and the gate refuses it. Mutation-checked: recording no applied call empties the group's calls, and this is then accepted.
#[test]
fn a_type_calling_itself_at_its_own_argument_is_refused() {
    assert!(matches!(
        infer(&mut certifier(), &group("f(n)")),
        Err(Error::NotDescending { .. }),
    ));
}

/// A `Nat` arm refines its scrutinee to one successor over the predecessor it binds, so a call at the predecessor is below the caller's argument. Mutation-checked: refining the scrutinee from the arm's typing spelling, `pred + 1`, reads nothing below it, and this is refused.
#[test]
fn a_type_descending_through_a_nat_arm_is_accepted() {
    assert!(infer(&mut certifier(), &group(&split("n", "f(pred)"))).is_ok());
}

/// A lambda applied on the spot is read through its binder: `((m) => f(m))(pred)` calls `f` at what `m` stands for, which is below the caller's argument. Mutation-checked: typing the lambda without its binders standing for the arguments leaves `m` a fresh binder nothing is below, and this is refused.
#[test]
fn a_descent_through_an_applied_lambda_is_read_through_its_binder() {
    let body = split("n", "((m: #NatType) => f(m))(pred)");

    assert!(infer(&mut certifier(), &group(&body)).is_ok());
}

/// A comparison behind a definition is read as a guard before the arm's case equation is assumed: the arm where `is_zero(n)` is false rules zero out for `n`, so `n - 1` is below it. Mutation-checked: read after the equation, the scrutinee reduces to the arm's own `false` and compares nothing, and this is refused.
#[test]
fn a_guard_behind_a_definition_establishes_what_its_arm_descends_on() {
    let mut certifier = certifier();
    declared(
        &mut certifier,
        "let /is_zero: (x: #NatType) -> #BoolType = (x: #NatType) => #NatEql(x, 0);",
    );

    let body =
        "#{goal = Type} match /is_zero(n) | false => f(#NatSub(n, 1)) | true => #NatType end";

    assert!(infer(&mut certifier, &group(body)).is_ok());
}

/// A call standing only in a nominal value's parameter is recorded, because the parameter is typed: `f(n)` sits in `Box(f(n)) { 3 }`, whose field is projected where it is inferred, so no conversion ever reads the value's type. Mutation-checked: counting a value's parameters without typing them leaves the group with no call, and it is accepted.
#[test]
fn a_call_in_a_nominal_values_parameter_is_recorded() {
    let mut certifier = certifier();
    declared(
        &mut certifier,
        "struct /Box(A: Type): pub Type {\n    x: #NatType,\n}",
    );

    let body = "let k: #NatType = #{field = Struct(/Box, 0)} (/Box(f(n)) { 3 }).0; #NatType";

    assert!(matches!(
        infer(&mut certifier, &group(body)),
        Err(Error::NotDescending { .. }),
    ));
}

/// A term naming a member is typed wherever it stands, whatever the certifier remembers of it. `rec a : Prop = b, b : Prop = b` states one body twice, and the call it holds is `a`'s to `b` in the first and `b`'s to itself in the second — the cycle the gate refuses. The members are propositions, so the group owes descent and its bodies read it by value, and nothing there classifies `b`: a body that is a type former over `b`, as `List(b)` is, would read `b` by value at its sort and run the budget out before the gate. Mutation-checked: with a remembered judgment answering for a term that names a member, the second body's call is never recorded, and the group is accepted.
#[test]
fn a_call_is_recorded_in_every_body_that_states_it() {
    assert!(matches!(
        infer(
            &mut certifier(),
            &term("(let a: Prop = b and b: Prop = b; a)")
        ),
        Err(Error::NotDescending { .. }),
    ));
}

/// What inferring `term` costs `certifier`, read off what its declaration has consumed on either side.
fn spent_inferring(certifier: &mut Certifier, term: &Term) -> u64 {
    let before = certifier.consumed().units();
    infer(certifier, term).expect("is typed");
    let after = certifier.consumed().units();

    after - before
}

/// A typing remembered on one side of what an arm establishes for the recorder does not answer on the other. A group typed inside a term closes under what the arms around it established, and the default arm of a `switch` rules zero out for its scrutinee while assuming no equation and opening no binder, so nothing else stands between a term typed outside it and the same term inside. Mutation-checked: without the clear where the arm's knowledge is entered the term is answered inside the bracket for nothing, and without the one where it is retracted, after it.
#[test]
fn a_remembered_type_does_not_outlive_what_an_arm_established() {
    let stated = group("#NatType");
    let Subterm::Rec(rec) = &*stated else {
        unreachable!("a group")
    };

    let mut certifier = certifier();
    let (locals, term) = opened(
        &mut certifier,
        "(rec: (n: #NatType) -> Type, k: #NatType) => #NatAdd(k, 1)",
    );
    certifier.open_group(rec.group(), &[locals[0]]);
    certifier.begin_member(0);

    let first = spent_inferring(&mut certifier, &term);
    let again = spent_inferring(&mut certifier, &term);
    let inside = certifier.scoped(|certifier| {
        certifier
            .assume_nonzero(locals[1])
            .expect("the arm's knowledge enters");
        spent_inferring(certifier, &term)
    });
    let after = spent_inferring(&mut certifier, &term);

    assert!(first > 0, "typing it spends");
    assert_eq!(again, 0, "and a second typing is remembered");
    assert!(inside > 0, "the arm's knowledge is entered");
    assert!(after > 0, "and retracted");
}

/// A walk refused inside [`Certifier::tried`] leaves the recorder as it found it: the call it graded before the refusal is taken back, and the group closes on the calls of the walks that stood. The call is the control's, `f` at its caller's own argument, so the group that keeps it does not descend, as the same walk in an ordinary bracket leaves it.
#[test]
fn a_refused_walk_takes_back_the_calls_it_recorded() {
    let closes = |tried: bool| {
        let mut certifier = certifier();
        let term = group("#NatType");
        let Subterm::Rec(rec) = &*term else {
            unreachable!("a group is a `rec`");
        };
        let (f, n) = (certifier.fresh(Some("f")), certifier.fresh(Some("n")));

        certifier.assume(&f, &rec.group().member_type(0));
        certifier.assume(&n, &self::term("#NatType"));
        certifier.open_group(rec.group(), &[f]);
        certifier.begin_member(0);
        certifier.bind_parameter(&n);
        let refused = |certifier: &mut Certifier| {
            certifier.record_call(&f, &[Segment::Arguments(vec![Term::free_var(&n)])])?;
            Err::<(), _>(Error::Unclassified(self::term("0")))
        };
        let outcome = match tried {
            true => certifier.tried(refused),
            false => certifier.scoped(refused),
        };
        assert!(outcome.is_err());

        certifier.close_group(rec.group(), vec![false])
    };

    assert_eq!(closes(true), Totality::Total);
    assert_ne!(closes(false), Totality::Total);
}

/// An arm that is no arm of its elimination records no call: `f`'s body splits on `not(true)`, and its `true` arm is a function type whose domain calls `f` at the caller's own argument and whose result applies a number, which no rule types. The domain is typed, and its call graded, before the result is refused. The arm is one no reduction takes, the call is taken back with it, and the group descends. The control is the same arm under `not(false)`, where it is the arm taken and its refusal is the group's.
///
/// Mutation-checked: with what a refused walk recorded kept, the first group holds the call and does not descend.
#[test]
fn an_arm_no_reduction_takes_records_no_call() {
    let closes = |argument: &str| {
        let body = fill!(
            "#{goal = Type} match ((c: #BoolType) => #{goal = #BoolType} match c | false => true | true => false end)(${argument}) | false => #NatType | true => (x: f(n)) -> 0(0) end",
            argument
        );

        infer(&mut certifier(), &group(&body))
    };

    assert!(closes("true").is_ok());
    assert!(closes("false").is_err());
}

/// A group this walk never typed is read in the refusing direction: no verdict is on record, so it does not descend, and each member reads as enclosing a group that does not, which is what the obligations read of an item whose bodies nothing typed. The control is the same group once typed, whose check files the verdict and the enclosure it found.
#[test]
fn a_group_this_walk_never_typed_reads_as_one_that_does_not_descend() {
    let term = group(&split("n", "f(pred)"));
    let Subterm::Rec(rec) = &*term else {
        panic!("`group` states a group: {term:?}");
    };
    let mut certifier = certifier();

    assert_eq!(certifier.group_verdict(rec.group()), None);
    assert!(certifier.member_encloses_partial(rec.group(), 0));

    assert!(infer(&mut certifier, &term).is_ok());
    assert_eq!(certifier.group_verdict(rec.group()), Some(Totality::Total));
    assert!(!certifier.member_encloses_partial(rec.group(), 0));
}

/// An arm that splits a lambda's binder splits what the binder stands for: in `((x) => match x | 0 => … | k + 1 => f(k) end)(n)` the lambda's body runs with `x` being `n`, so the arm's `k + 1` is `n`'s case too and `k` is below `n`. Mutation-checked: with the arm refining `x` alone, `k` is below nothing the caller's parameter reads, and this is refused.
#[test]
fn an_arm_splitting_an_applied_lambdas_binder_splits_what_the_binder_stands_for() {
    let body = fill!(
        "((x: #NatType) => ${split})(n)",
        split = split("x", "f(pred)")
    );

    assert!(infer(&mut certifier(), &group(&body)).is_ok());
}

/// Zero ruled out of a lambda's binder is ruled out of what it stands for: in `((x) => match x | 0 => … | _ => f(x - 1) end)(n)` the default runs with `x` being `n` and nonzero, so `x - 1` is below `n`, as a default over `n` itself reads it, and a boolean guard's `false` arm over `x == 0` alike. Mutation-checked: ruling zero out of the binder alone, the call reads at most `n`, and both are refused.
#[test]
fn zero_ruled_out_of_an_applied_lambdas_binder_is_ruled_out_of_what_it_stands_for() {
    for body in [
        "((x: #NatType) => #{goal = Type} match x | 0 => #NatType | _ => f(#NatSub(x, 1)) end)(n)",
        "((x: #NatType) => #{goal = Type} match #NatEql(x, 0) | false => f(#NatSub(x, 1)) | true => #NatType end)(n)",
    ] {
        assert!(infer(&mut certifier(), &group(body)).is_ok(), "{body}");
    }
}


/// A carrier's element type is typed where the match states it, so a call standing there is recorded: `f(n)` sits in the element type of a split over `id(l)`, which no reduction reads past conversion and no variable's solution types. Mutation-checked: converting the element type without typing it leaves the group with no call, and it is accepted.
#[test]
fn a_call_in_a_carriers_element_type_is_recorded() {
    let mut certifier = certifier();
    declared(
        &mut certifier,
        "let /id: (l: #ListType(#NatType)) -> #ListType(#NatType) = (l: #ListType(#NatType)) => l;",
    );

    let body = "#{goal = Type, cases = {carrier = {elem = ((x: Type) => #NatType)(f(n))}}} match /id(#List(#NatType, [])) | [] => #NatType | [h, ..t] => #NatType end";

    assert!(matches!(
        infer(&mut certifier, &group(body)),
        Err(Error::NotDescending { .. }),
    ));
}

/// A lambda applied on the spot to a `let`-bound local stands for what the `let` stands for, as every graded call reads the local: in `let u = n; ((x) => match x | 0 => … | pred + 1 => f(pred) end)(u)` the arm takes `n` apart, so the call descends. Mutation-checked: with `infer_telescope` handing `refine_size` the argument as written, `x` stands for `u`, which no graded call names, and the group is refused.
#[test]
fn a_lambda_applied_to_a_let_bound_local_stands_for_what_the_let_stands_for() {
    let body = fill!(
        "let u: #NatType = n; ((x: #NatType) => ${split})(u)",
        split = split("x", "f(pred)")
    );

    assert!(infer(&mut certifier(), &group(&body)).is_ok(), "{body}");
}
