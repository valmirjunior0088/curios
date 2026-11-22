//! Where the recorder sees a call: in a nominal value's parameter, in every body stating it, in a carrier's or a list literal's element type, a match's stated instance and a proof argument; none an arm no reduction takes, and none a refused walk keeps.

use {
    super::test_support::*,
    crate::{test_support::*, *},
    curios_analysis::Segment,
    curios_common::fill,
    curios_core::{Subterm, Term, Totality},
};

/// A proposition with one proof, which the fixtures below that state a proof declare.
const TRUE: &str = "induct /True: pub Prop\n| qed()\nend";

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

/// The instance a match states is typed where it is held to the scrutinee's type, so a call standing only there is recorded: `f(n)` sits in the stated instance of a split over `qed()`, which reduction reads the arm's payload off and no arm names.
#[test]
fn a_call_in_a_matchs_stated_instance_is_recorded() {
    let mut certifier = certifier();
    declared(&mut certifier, TRUE);

    let body = "#{goal = Type, cases = {stated = ((y: Type) => #{node = InductType} /True())(f(n))}} match #{node = Variant} /True/qed() | qed() => #NatType end";

    assert!(matches!(
        infer(&mut certifier, &group(body)),
        Err(Error::NotDescending { .. }),
    ));
}

/// An argument at a proposition is typed as every argument is, so a call standing only in a proof is recorded: `f(n)` sits in the proof `/keep` takes beside the type it answers, which erasure deletes and `/keep`'s reduct drops.
#[test]
fn a_call_in_a_proof_argument_is_recorded() {
    let mut certifier = certifier();
    declared(
        &mut certifier,
        &fill!(
            "${TRUE}\nlet /keep: (b: Type, p: #{node = InductType} /True()) -> Type = (b: Type, p: #{node = InductType} /True()) => b;",
            TRUE
        ),
    );

    let body = "/keep(#NatType, ((y: Type) => #{node = Variant} /True/qed())(f(n)))";

    assert!(matches!(
        infer(&mut certifier, &group(body)),
        Err(Error::NotDescending { .. }),
    ));
}

/// An intrinsic's type operand is typed, so a call standing only in a list literal's element type is recorded: `f(n)` sits in the element type of the empty list a split takes apart, and no arm or goal names it.
#[test]
fn a_call_in_a_list_literals_element_type_is_recorded() {
    let body = "#{goal = Type, cases = {carrier = {elem = #NatType}}} match #List(((y: Type) => #NatType)(f(n)), []) | [] => #NatType | [h, ..t] => #NatType end";

    assert!(matches!(
        infer(&mut certifier(), &group(body)),
        Err(Error::NotDescending { .. }),
    ));
}
