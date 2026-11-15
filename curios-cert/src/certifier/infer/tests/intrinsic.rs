//! Intrinsic operands and results, the narrowing that needs its bound, and the free-monoid arms.

use {
    crate::certifier::infer::test_support::*,
    crate::{test_support::*, *},
    curios_analysis::test_support::*,
    curios_common::fill,
    curios_core::{Global, Level, Term, UniverseContext},
};

/// Declare `Holds` in `certifier` as the predicate a bound's proof inhabits, with no body: the fixture registry only names it, and opaque is enough.
fn declare_holds(certifier: &mut Certifier) {
    certifier
        .declare(
            &Global::Authored(SYNTAX.proof.holds.qualifier()),
            &term("(b: #BoolType) -> Prop"),
            &UniverseContext::default(),
        )
        .expect("a name is declared once");
}

#[test]
fn a_channel_allocation_checks_its_positive_capacity_evidence() {
    let mut certifier = certifier();
    declare_holds(&mut certifier);
    let [allocated, of_nothing, unproved] = under(
        &mut certifier,
        "positive: /std/Bool/Holds(true)",
        "#Channel(#NatType, 1, positive), #Channel(#NatType, 0, positive), #Channel(#NatType, 1, 1)",
    );

    assert_eq!(
        infer(&mut certifier, &allocated),
        Ok(term("#IoType(#ChannelType(#NatType))")),
    );
    for invalid in [of_nothing, unproved] {
        assert!(matches!(
            infer(&mut certifier, &invalid),
            Err(Error::Mismatch { .. })
        ));
    }
}

/// An intrinsic's operands are checked against the types its rule demands.
#[test]
fn an_intrinsic_operand_of_the_wrong_type_is_refused() {
    let mut certifier = certifier();

    assert!(matches!(
        infer(&mut certifier, &term("#NatAdd(true, 1)")),
        Err(Error::Mismatch { .. }),
    ));
}

/// A bound stated only on `/sys`'s wrapper is re-checked wherever that application survives and nowhere else: one unfolding leaves the bare operation, which a signature reading its operand alone would admit. Carrying the proof as an operand is what makes the check a property of the node, so it is this crate — not the elaborator that built the node — that decides the narrowing was justified.
///
/// `Holds` is declared rather than defined, since the fixture registry only names it. Opaque is enough: what is under test is that the operand is checked against the proposition at all — the reflection of a comparison the signature builds.
#[test]
fn a_narrowing_to_nat_is_refused_without_its_bound() {
    let mut certifier = certifier();
    declare_holds(&mut certifier);
    let [narrowed, unproved] = under(
        &mut certifier,
        "x: #IntType, ok: /std/Bool/Holds(#IntLe(#Int(0), x))",
        "#IntToNat(x, ok), #IntToNat(x, 1)",
    );

    assert_eq!(infer(&mut certifier, &narrowed), Ok(term("#NatType")));
    assert!(matches!(
        infer(&mut certifier, &unproved),
        Err(Error::Mismatch { .. }),
    ));
}

#[test]
fn an_intrinsic_operation_has_the_result_type_its_rule_states() {
    let mut certifier = certifier();

    assert_eq!(
        infer(&mut certifier, &term("#NatAdd(1, 2)")),
        Ok(term("#NatType"))
    );
    assert_eq!(
        infer(&mut certifier, &term("#NatLt(1, 2)")),
        Ok(term("#BoolType"))
    );
}

/// A list literal carries its element type, every element checks against it — and `[]` types at exactly that carried element, though it has no element to read a type from.
#[test]
fn a_list_literal_checks_its_elements_against_its_carried_type() {
    let mut certifier = certifier();
    let list = term("#ListType(#NatType)");

    assert_eq!(
        infer(&mut certifier, &term("#List(#NatType, [1, 2])")),
        Ok(list.clone())
    );
    assert!(matches!(
        infer(&mut certifier, &term("#List(#NatType, [1, true])")),
        Err(Error::Mismatch { .. }),
    ));
    assert_eq!(
        infer(&mut certifier, &term("#List(#NatType, [])")),
        Ok(list)
    );
}

/// A free-monoid cons arm is checked under its binders — the peeled generator and the tail — against the goal at one generator over the tail, and a body of the wrong type at the case's instance is refused.
#[test]
fn a_free_monoid_arm_must_inhabit_the_goal_at_its_case() {
    let mut certifier = certifier();
    // Goal `Nat`: the zero arm at `Nat` and the succ arm's predecessor at `Nat`; then a succ arm that produces a Bool where the goal demands a Nat, and a zero arm of the wrong type — the identity case is a case like any other.
    let [counts, wrong, wrong_zero] = under(
        &mut certifier,
        "n: #NatType",
        "#{goal = #NatType} match n | 0 => 0 | pred + 1 => pred end, #{goal = #NatType} match n | 0 => 0 | pred + 1 => true end, #{goal = #NatType} match n | 0 => true | pred + 1 => pred end",
    );

    assert_eq!(infer(&mut certifier, &counts), Ok(term("#NatType")));
    assert!(matches!(
        infer(&mut certifier, &wrong),
        Err(Error::Mismatch { .. }),
    ));
    assert!(matches!(
        infer(&mut certifier, &wrong_zero),
        Err(Error::Mismatch { .. }),
    ));
}

/// The carrier's element type must agree with the scrutinee's: the arms are typed against the carrier's copy, and the values flowing through the match carry the scrutinee's.
#[test]
fn a_free_monoid_carrier_must_match_its_scrutinee() {
    let mut certifier = certifier();
    // Carrier claims Bool elements over a Nat-list scrutinee.
    let [mismatched] = under(
        &mut certifier,
        "xs: #ListType(#NatType)",
        "#{goal = #NatType, cases = {carrier = {elem = #BoolType}}} match xs | [] => 0 | [head, ..tail] => 0 end,",
    );

    assert!(matches!(
        infer(&mut certifier, &mismatched),
        Err(Error::Mismatch { .. }),
    ));
}

/// A case form names the carrier it eliminates, and that claim needs establishing like any other.
///
/// `check_free_monoid` establishes it — `Carrier::Nat` matches the scrutinee's type against `NatType`, `Carrier::Bytes` against `BytesType`, and `Carrier::List` converts its carried element type against the scrutinee's — and `Cases::Induct` gets it from needing an `InductType` to read a declaration off at all. The boolean and dispatch forms establish it against `Bool` and `Nat`: no typing rule looks at a case form otherwise, so no ordering discipline would catch a claim left unchecked.
///
/// What it costs is the discipline the free-monoid rule states for itself: the arms are typed at the *case values* — `false` and `true`, or the enumerated literals — while the result is typed at the goal and a value flowing through the match carries the scrutinee's type, so a disagreement types the arms at one carrier and runs them at another. `curios-elab` refuses both spellings at `check_intrinsic_head`, so no surface program reaches them.
#[test]
fn a_boolean_elimination_requires_a_boolean_scrutinee() {
    let mut certifier = certifier();

    assert!(
        infer(
            &mut certifier,
            &term("#{goal = #NatType} match 0 | false => 1 | true => 2 end")
        )
        .is_err(),
        "the certifier typed a boolean elimination of a `Nat`",
    );
}

/// The same for the dispatch form, whose default is checked at the scrutinee's own instance while its enumerated arms are checked at `Nat` literals.
#[test]
fn a_dispatch_requires_a_natural_scrutinee() {
    let mut certifier = certifier();

    assert!(
        infer(
            &mut certifier,
            &term("#{goal = #NatType} match true | 0 => 1 | _ => 2 end")
        )
        .is_err(),
        "the certifier typed a `Nat` dispatch on a `Bool`",
    );
}

/// The values the forms of [`a_description_a_cell_and_a_channel_are_taken_apart_by_nothing`] take apart, each held at `${wrap}` over what it carries.
const HELD: &str = "pair: ${pair}, record: ${record}, refuted: ${refuted}, truth: ${truth}, count: ${count}, list: ${list}";

/// Each form that takes a value apart, over the values [`HELD`] holds: a tuple's and a record's projection, a match on an empty family, a boolean elimination, a dispatch, a natural-number fold and a list fold.
const FORMS: &str = "pair.0, #{field = Struct(/Boxed, 0)} record.0, #{goal = #NatType} match refuted end, #{goal = #NatType} match truth | false => 0 | true => 1 end, #{goal = #NatType} match count | 0 => 1 | _ => 2 end, #{goal = #NatType} match count | 0 => 0 | head + 1 => 1 end, #{goal = #NatType, cases = {carrier = {elem = #NatType}}} match list | [] => 0 | [head, ..tail] => 1 end";

/// A description, a cell and a channel are eliminated by nothing, so no term reads the value one holds or describes: each form that takes a value apart — a tuple's or a record's projection, a match on a family, and each carrier's own case forms — is refused over one, however well its arms would type at what it carries. Read, a description of `{False, Nat}` that exits would hand back the `False` it never returns.
#[test]
fn a_description_a_cell_and_a_channel_are_taken_apart_by_nothing() {
    let no_components: fn(&Error) -> bool = |error| matches!(error, Error::NotATuple(_));
    let no_family: fn(&Error) -> bool = |error| matches!(error, Error::Unclassified(_));
    let names = [
        ("a tuple's projection", no_components),
        ("a record's projection", no_components),
        ("a match on an empty family", no_family),
        ("a boolean elimination", no_family),
        ("a dispatch", no_family),
        ("a natural-number fold", no_family),
        ("a list fold", no_family),
    ];

    // Each form over a value held at `wrap` over what it carries, with the refusal it meets where the wrapper is a carrier.
    let forms = |certifier: &mut Certifier, wrap: &str| {
        declared(certifier, BOXED);
        declared(certifier, "induct /Absurd: pub Prop\nend");
        let held = |carried: &str| fill!(wrap, carried);
        let terms: [Term; 7] = under(
            certifier,
            &fill!(
                HELD,
                pair = held("{f: #NatType, n: #BoolType}"),
                record = held("#{node = StructType} /Boxed(#NatType)"),
                refuted = held("#{node = InductType} /Absurd()"),
                truth = held("#BoolType"),
                count = held("#NatType"),
                list = held("#ListType(#NatType)")
            ),
            FORMS,
        );

        names.into_iter().zip(terms)
    };

    // The control: each form over the carried value itself types, so what refuses it below is the carrier.
    let mut certifier = certifier();
    for ((form, _), term) in forms(&mut certifier, "${carried}") {
        assert_eq!(
            infer(&mut certifier, &term),
            Ok(self::term("#NatType")),
            "{form} stopped typing"
        );
    }

    for carrier in ["#IoType", "#CellType", "#ChannelType"] {
        let mut certifier = self::certifier();
        let wrap = fill!("${carrier}(${carried})", carrier, carried = "${carried}");
        for ((form, refused), term) in forms(&mut certifier, &wrap) {
            let answer = infer(&mut certifier, &term);
            assert!(
                answer.as_ref().err().is_some_and(refused),
                "{form} over a `{carrier}` answered {answer:?}",
            );
        }
    }
}

/// The control for both, and it is what a fix by brick would fail: each form at its own carrier is ordinary code and must keep typing.
#[test]
fn each_intrinsic_elimination_at_its_own_carrier_is_still_accepted() {
    let mut certifier = certifier();
    let nat = term("#NatType");

    assert_eq!(
        infer(
            &mut certifier,
            &term("#{goal = #NatType} match true | false => 1 | true => 2 end")
        ),
        Ok(nat.clone())
    );
    assert_eq!(
        infer(
            &mut certifier,
            &term("#{goal = #NatType} match 0 | 0 => 1 | _ => 2 end")
        ),
        Ok(nat)
    );
}

/// An operation stuck on its operands is classified by the type its signature produces, the type typing read it at: a read of a list of `Type 1`s is a type at `Type 1`, and one of a list of propositions a proposition. The lookup answered only for the formers, and refused a type typing had accepted. The control is an operation producing a number, which classifies nothing.
#[test]
fn a_stuck_read_of_a_list_of_types_is_a_type_at_their_sort() {
    let one = Level::zero().succ().expect("level zero has a successor");
    let read = |certifier: &mut Certifier, element: &str| {
        let [read] = under(
            certifier,
            &fill!("ts: #ListType(${element})", element),
            &fill!("#ListGet(${element}, ts, 0, h),", element),
        );

        read
    };

    let mut certifier = certifier();
    let types = read(&mut certifier, "#{level = 1} Type");
    assert_eq!(Sort::of(&mut certifier, &types), Ok(Sort::Type(one)));

    let mut certifier = self::certifier();
    let propositions = read(&mut certifier, "Prop");
    assert_eq!(
        Sort::of(&mut certifier, &propositions),
        Ok(Sort::Prop(Level::zero()))
    );

    let mut certifier = self::certifier();
    let [number] = under(&mut certifier, "n: #NatType", "#NatAdd(n, 1),");
    assert!(matches!(
        Sort::of(&mut certifier, &number),
        Err(Error::Unclassified(_))
    ));
}
