//! Each roster row held to what it types: a literal at the carrier its payload is a value of, a fold's value at the carrier its row states and nowhere past the domain it states, an operation the algebra declares at the carrier it is declared at, a row no fold answers at a description, and a parameterized former at its element's level.

use {
    crate::certifier::infer::test_support::*,
    crate::{test_support::*, *},
    curios_common::fill,
    curios_core::{Declaration, Intrinsic, Reducer, Subterm, Term, Var, Visit},
};

/// The vocabulary a bound is stated in, defined as `/sys` defines it — `True` with its one proof, a proposition with none, and `Holds(b)` reducing to one or the other — so a bound is discharged by its decision reducing, as a program's is, rather than assumed.
const BOUNDS: &str = r#"
induct /std/Bool/True: pub Prop
| qed()
end
induct /False: pub Prop
end
let /std/Bool/Holds: (b: #BoolType) -> Prop =
    (b: #BoolType) => match b: (_) => Prop | false => #{node = InductType} /False() | true => #{node = InductType} /std/Bool/True() end;
"#;

/// A certifier holding [`BOUNDS`].
fn certifier_with_bounds() -> Certifier<'static> {
    let mut certifier = certifier();
    declared(&mut certifier, BOUNDS);

    certifier
}

/// The one proof of `True`, which closes every bound whose decision reduces to `true` and no other.
const QED: &str = "#{node = Variant} /std/Bool/True/qed()";

/// `template` with its bound closed by [`QED`], which it writes `${qed}`.
fn proved(template: &str) -> String {
    fill!(template, qed = QED)
}

/// A `Flt` literal at `value`, a number, written as the reader reads one: signed, with its decimal point, which Rust leaves out of a value with no fraction.
fn flt(value: f64) -> String {
    let written = format!("{value:+}");
    let written = match written.contains('.') {
        true => written,
        false => written + ".0",
    };

    fill!("#Flt(${written})", written)
}

/// The type a carrier of the algebra is, as the roster spells it.
fn carried(carrier: curios_algebra::Carrier) -> Term {
    term(match carrier {
        curios_algebra::Carrier::Natural => "#NatType",
        curios_algebra::Carrier::Integer => "#IntType",
        curios_algebra::Carrier::Boolean => "#BoolType",
        curios_algebra::Carrier::Byte => "#ByteType",
        curios_algebra::Carrier::Float => "#FltType",
        curios_algebra::Carrier::Bytes => "#BytesType",
        curios_algebra::Carrier::List => unreachable!("no intrinsic is declared at a list"),
    })
}

/// Whether a reduct is a value of a carrier, the form a fold lands in: a literal, and for `Nat` one with no symbolic part left.
fn is_value(term: &Term) -> bool {
    match &**term {
        Subterm::Intrinsic(Intrinsic::Nat(value)) => value.to_natural().is_some(),
        Subterm::Intrinsic(
            Intrinsic::Bool(_)
            | Intrinsic::Byte(_)
            | Intrinsic::Int(_)
            | Intrinsic::Flt(_)
            | Intrinsic::Bytes(_)
            | Intrinsic::List { .. },
        ) => true,
        _ => false,
    }
}

/// One closed application of every row a fold answers, its operands at the carriers the row states and inside the domain it states — at that domain's edge wherever it has one.
fn inside() -> Vec<Term> {
    let rows = vec![
        "#BoolAnd(true, false)".to_string(),
        "#BoolOr(true, false)".to_string(),
        "#BoolXor(true, false)".to_string(),
        "#BoolEql(true, false)".to_string(),
        "#BoolNeq(true, false)".to_string(),
        "#NatMin(7, 2)".to_string(),
        "#NatMax(7, 2)".to_string(),
        "#NatEql(7, 2)".to_string(),
        "#NatNeq(7, 2)".to_string(),
        "#NatAdd(7, 2)".to_string(),
        "#NatSub(7, 2)".to_string(),
        "#NatMul(7, 2)".to_string(),
        "#NatLt(7, 2)".to_string(),
        "#NatLe(7, 2)".to_string(),
        "#NatAnd(7, 2)".to_string(),
        "#NatOr(7, 2)".to_string(),
        "#NatXor(7, 2)".to_string(),
        "#NatShl(7, 2)".to_string(),
        "#NatShr(7, 2)".to_string(),
        "#NatPow(7, 2)".to_string(),
        "#IntMin(#Int(-7), #Int(2))".to_string(),
        "#IntMax(#Int(-7), #Int(2))".to_string(),
        "#IntEql(#Int(-7), #Int(2))".to_string(),
        "#IntNeq(#Int(-7), #Int(2))".to_string(),
        "#IntAdd(#Int(-7), #Int(2))".to_string(),
        "#IntSub(#Int(-7), #Int(2))".to_string(),
        "#IntMul(#Int(-7), #Int(2))".to_string(),
        "#IntLt(#Int(-7), #Int(2))".to_string(),
        "#IntLe(#Int(-7), #Int(2))".to_string(),
        "#IntAnd(#Int(-7), #Int(2))".to_string(),
        "#IntOr(#Int(-7), #Int(2))".to_string(),
        "#IntXor(#Int(-7), #Int(2))".to_string(),
        // A shift's count is a natural on both carriers.
        "#IntShl(#Int(-7), 2)".to_string(),
        "#IntShr(#Int(-7), 2)".to_string(),
        "#FltAdd(TiesToEven, #Flt(+2.5), #Flt(-0.5))".to_string(),
        "#FltSub(TiesToEven, #Flt(+2.5), #Flt(-0.5))".to_string(),
        "#FltMul(TiesToEven, #Flt(+2.5), #Flt(-0.5))".to_string(),
        "#FltDiv(TiesToEven, #Flt(+2.5), #Flt(-0.5))".to_string(),
        "#FltRem(#Flt(+2.5), #Flt(-0.5))".to_string(),
        "#FltEql(#Flt(+2.5), #Flt(-0.5))".to_string(),
        "#FltNeq(#Flt(+2.5), #Flt(-0.5))".to_string(),
        "#FltLt(#Flt(+2.5), #Flt(-0.5))".to_string(),
        "#FltLe(#Flt(+2.5), #Flt(-0.5))".to_string(),
        "#FltMin(#Flt(+2.5), #Flt(-0.5))".to_string(),
        "#FltMax(#Flt(+2.5), #Flt(-0.5))".to_string(),
        "#FltCopysign(#Flt(+2.5), #Flt(-0.5))".to_string(),
        // The one literal that carries a term: a floor of successors over a natural.
        "#Nat(Succ(3, 4))".to_string(),
        // The least divisor a quotient takes at each carrier, and a negative one at `Int`.
        proved("#NatDiv(7, 1, ${qed})"),
        proved("#NatRem(7, 1, ${qed})"),
        proved("#IntDiv(#Int(-7), #Int(-1), ${qed})"),
        proved("#IntRem(#Int(-7), #Int(-1), ${qed})"),
        // The greatest natural a byte holds, and the least integer a natural does.
        "#ByteToNat(#Byte(255))".to_string(),
        proved("#NatToByte(255, ${qed})"),
        "#NatToInt(7)".to_string(),
        // A magnitude and a sign of an integer below zero.
        "#IntAbs(#Int(-7))".to_string(),
        "#IntSign(#Int(-7))".to_string(),
        proved("#IntToNat(#Int(0), ${qed})"),
        "#NatToFlt(TiesToEven, 7)".to_string(),
        "#IntToFlt(TiesToEven, #Int(-7))".to_string(),
        "#FltNeg(#Flt(+2.5))".to_string(),
        "#FltAbs(#Flt(-2.5))".to_string(),
        "#FltSqrt(TiesToEven, #Flt(+2.5))".to_string(),
        "#FltRoundIntegral(TiesToEven, #Flt(+2.5))".to_string(),
        "#FltFma(TiesToEven, #Flt(+2.5), #Flt(-0.5), #Flt(+1.0))".to_string(),
        // Both ends of each narrowing's domain: the negative zero and the greatest finite value into `Nat`, the two finite extremes and the least subnormal into `Int`.
        proved("#FltToNat(#Flt(-0.0), ${qed})"),
        fill!("#FltToNat(${max}, ${qed})", max = flt(f64::MAX), qed = QED),
        fill!("#FltToInt(${max}, ${qed})", max = flt(f64::MAX), qed = QED),
        fill!("#FltMantissa(${min}, ${qed})", min = flt(f64::MIN), qed = QED),
        fill!(
            "#FltExponent(${least}, ${qed})",
            least = flt(f64::from_bits(1)),
            qed = QED
        ),
        "#FltToLeBytes(#Flt(+2.5))".to_string(),
        proved("#FltOfLeBytes(x[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], ${qed})"),
        "#BytesLen(x[0xA5, 0xA5])".to_string(),
        "#BytesEql(x[0xA5, 0xA5], x[0xA5])".to_string(),
        // The last index, and a window ending where the run does.
        proved("#BytesGet(x[0xA5, 0xA5], 1, ${qed})"),
        proved("#BytesSlice(x[0xA5, 0xA5], 1, 1, ${qed})"),
        "#BytesAppend(x[0xA5, 0xA5], #Byte(90))".to_string(),
        "#BytesConcat([x[0xA5, 0xA5], x[0xA5]])".to_string(),
        "#BytesReplicate(3, #Byte(90))".to_string(),
        proved("#BytesAnd(x[0xA5, 0xA5], x[0xA5, 0xA5], ${qed})"),
        proved("#BytesOr(x[0xA5, 0xA5], x[0xA5, 0xA5], ${qed})"),
        proved("#BytesXor(x[0xA5, 0xA5], x[0xA5, 0xA5], ${qed})"),
        "#ListLen(#NatType, #List(#NatType, [4, 5]))".to_string(),
        proved("#ListGet(#NatType, #List(#NatType, [4, 5]), 1, ${qed})"),
        proved("#ListSlice(#NatType, #List(#NatType, [4, 5]), 1, 1, ${qed})"),
        "#ListAppend(#NatType, #List(#NatType, [4, 5]), 6)".to_string(),
        "#ListConcat(#NatType, [#List(#NatType, [4, 5]), #List(#NatType, [6])])".to_string(),
        // A map into another carrier, so the image's element type is the row's `to` and not its `from`.
        "#ListMap(#NatType, #BoolType, #List(#NatType, [4, 5]), (item: #NatType) => #NatLt(item, 5))"
            .to_string(),
        "#ListFold(#NatType, #NatType, #List(#NatType, [4, 5]), 0, (item: #NatType, accumulator: #NatType) => #NatAdd(item, accumulator))"
            .to_string(),
    ];

    rows.iter().map(|row| term(row)).collect()
}

/// One closed application of every row that states a bound, one step past the edge of the domain it states, the bound closed with the proof that closes it inside. A channel's capacity is the one bound here over a row no fold answers.
fn outside() -> Vec<Term> {
    let rows = [
        proved("#NatDiv(7, 0, ${qed})"),
        proved("#NatRem(7, 0, ${qed})"),
        proved("#IntDiv(#Int(-7), #Int(0), ${qed})"),
        proved("#IntRem(#Int(-7), #Int(0), ${qed})"),
        proved("#NatToByte(256, ${qed})"),
        proved("#IntToNat(#Int(-1), ${qed})"),
        // The least negative value, and the three values that are no number.
        fill!(
            "#FltToNat(${least}, ${qed})",
            least = flt(-f64::from_bits(1)),
            qed = QED
        ),
        proved("#FltToNat(#Flt(+inf.0), ${qed})"),
        proved("#FltToInt(#Flt(+inf.0), ${qed})"),
        proved("#FltMantissa(#Flt(-inf.0), ${qed})"),
        proved("#FltExponent(#Flt(+nan.0), ${qed})"),
        proved("#FltOfLeBytes(x[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], ${qed})"),
        proved("#FltOfLeBytes(x[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], ${qed})"),
        proved("#ListGet(#NatType, #List(#NatType, [4, 5]), 2, ${qed})"),
        proved("#ListSlice(#NatType, #List(#NatType, [4, 5]), 1, 2, ${qed})"),
        proved("#Channel(#NatType, 0, ${qed})"),
        proved("#BytesGet(x[0xA5, 0xA5], 2, ${qed})"),
        proved("#BytesSlice(x[0xA5, 0xA5], 1, 2, ${qed})"),
        proved("#BytesAnd(x[0xA5, 0xA5], x[0xA5], ${qed})"),
        proved("#BytesOr(x[0xA5, 0xA5], x[0xA5], ${qed})"),
        proved("#BytesXor(x[0xA5, 0xA5], x[0xA5], ${qed})"),
    ];

    rows.iter().map(|row| term(row)).collect()
}

/// A row is the definition of what its operation means, so a wrong one disagrees with nothing a program can spell — but it disagrees with the fold, the carrier's own computation, whose operands and result are Rust values of one type each. A row stating another operand carrier than the fold reads leaves a closed application stuck; one stating another result types the application at a carrier its reduct is not a value of, and conversion then holds that reduct at the wrong type; a bound weaker than the fold's domain admits an application the fold has no value for, under laws that take the bound for granted.
///
/// So every application in [`inside`] is typed by its row, folds to a value, and that value is typed by its own literal row at the type the operation's row stated.
///
/// Mutation-checked a row at a time: the `Nat` comparisons producing `Nat` fail here on the two types, an `Int` shift counted by an `Int` on the application its row no longer types, and `NatToByte`'s bound tightened to `nat < 255` on the edge it no longer admits.
#[test]
fn every_fold_lands_in_the_carrier_its_row_states() {
    for application in inside() {
        let mut certifier = certifier_with_bounds();

        let stated = infer(&mut certifier, &application)
            .unwrap_or_else(|error| panic!("{application:?} is refused: {error:?}"));
        let folded = certifier
            .reduce_forced(application.clone())
            .unwrap_or_else(|error| panic!("{application:?} has no value: {error:?}"));

        assert!(
            is_value(&folded),
            "{application:?} is typed and stays stuck at {folded:?}",
        );
        assert_eq!(
            infer(&mut certifier, &folded),
            Ok(stated),
            "{application:?} folds to {folded:?}, a value of another type",
        );
    }
}

/// The other side of each edge: one step past the domain a row states, the proof that closed the bound inside it no longer does, and the certifier refuses the application by that mismatch — at operands the fold has no value for, where it reports or declines.
///
/// The second assertion holds each sample to the edge: one the fold still answered would show a bound stronger than its operation needs, and say nothing about where the row draws it.
///
/// Mutation-checked as the fixture above is: `NatToByte`'s bound loosened to `nat < 257`, and `BytesGet`'s to `index <= len`, each admit their sample here.
#[test]
fn a_bounded_row_is_refused_one_step_past_its_folds_domain() {
    for application in outside() {
        let mut certifier = certifier_with_bounds();

        assert!(
            matches!(
                infer(&mut certifier, &application),
                Err(Error::Mismatch { .. })
            ),
            "{application:?} is typed past its bound",
        );
        assert!(
            !certifier
                .reduce_forced(application.clone())
                .is_ok_and(|folded| is_value(&folded)),
            "{application:?} has a value its bound refuses",
        );
    }
}

/// The carriers are written a second time inside the trusted base: `Intrinsic::algebra` declares each operation a law reads at a carrier, and a law is stated per operation of a carrier — a bound over the naturals, a negation over a total order — so an operation declared at a carrier its row does not type is a law applied to values it was never true of. Each declaration is read against its row: a conversion from its source into the carrier, a comparison over the carrier into `Bool`, a shift of the carrier by a natural, an integer's magnitude from the integers into the naturals, and every other operation closed on the carrier, over one operand, two, or a fused multiply-add's three.
///
/// The tally is every declaration `algebra` makes today, so a grid that stopped reaching them fails. A declaration added there owes [`inside`] its row, and nothing fails until it has one.
///
/// Mutation-checked: `IntShr` declared at the naturals, where a right shift never exceeds what it shifts, fails on the row's `Int`.
#[test]
fn an_operation_is_declared_to_the_algebra_at_the_carrier_its_row_types() {
    let mut declared = 0;
    let (nat, boolean) = (term("#NatType"), term("#BoolType"));

    for application in inside() {
        let Subterm::Intrinsic(intrinsic) = &*application else {
            unreachable!("the grid holds intrinsics");
        };
        let Declaration::Operation {
            carrier,
            operation,
            operands,
        } = intrinsic.algebra()
        else {
            continue;
        };
        declared += 1;

        let mut certifier = certifier_with_bounds();
        let mut typed = |term: &Term| {
            infer(&mut certifier, term)
                .unwrap_or_else(|error| panic!("{term:?} is refused: {error:?}"))
        };
        let result = typed(&application);
        let operands = operands
            .as_slice()
            .iter()
            .map(|operand| typed(operand))
            .collect::<Vec<_>>();

        let carrier = carried(carrier);
        let stated = match operation {
            curios_algebra::Operation::Conversion { from } => (vec![carried(from)], carrier),
            curios_algebra::Operation::Equal
            | curios_algebra::Operation::Unequal
            | curios_algebra::Operation::Less
            | curios_algebra::Operation::AtMost => {
                (vec![carrier.clone(), carrier], boolean.clone())
            }
            curios_algebra::Operation::ShiftLeft | curios_algebra::Operation::ShiftRight => {
                (vec![carrier.clone(), nat.clone()], carrier)
            }
            curios_algebra::Operation::Sum
            | curios_algebra::Operation::Difference
            | curios_algebra::Operation::Product
            | curios_algebra::Operation::Quotient
            | curios_algebra::Operation::Remainder
            | curios_algebra::Operation::And
            | curios_algebra::Operation::Or
            | curios_algebra::Operation::Xor
            | curios_algebra::Operation::Power
            | curios_algebra::Operation::RoundedSum(_)
            | curios_algebra::Operation::RoundedProduct(_)
            | curios_algebra::Operation::Min
            | curios_algebra::Operation::Max
            | curios_algebra::Operation::Copysign => {
                (vec![carrier.clone(), carrier.clone()], carrier)
            }
            curios_algebra::Operation::Successor
            | curios_algebra::Operation::Signum
            | curios_algebra::Operation::Negation => (vec![carrier.clone()], carrier),
            curios_algebra::Operation::Absolute => {
                (vec![carried(curios_algebra::Carrier::Integer)], carrier)
            }
            curios_algebra::Operation::Fused(_) => (
                vec![carrier.clone(), carrier.clone(), carrier.clone()],
                carrier,
            ),
            curios_algebra::Operation::Concat
            | curios_algebra::Operation::Length
            | curios_algebra::Operation::Append => {
                unreachable!("no intrinsic is declared as an operation on words")
            }
        };

        assert_eq!((operands, result), stated, "{application:?}");
    }

    assert_eq!(declared, 57);
}

/// The classification walks a declared operation by position: the operands its meaning reads are the first its traversal meets, in the declaration's order, and what the traversal meets after them is what the declaration leaves out. So a walk that reads as many children as the declaration has operands reads the declaration's, and rebuilds the operation by the same traversal.
///
/// Mutation-checked: `NatDiv` declared over its divisor and then its dividend fails on the first child.
#[test]
fn the_operands_an_operation_is_declared_over_are_the_first_its_traversal_meets() {
    for application in inside() {
        let Subterm::Intrinsic(intrinsic) = &*application else {
            unreachable!("the grid holds intrinsics");
        };
        let Declaration::Operation { operands, .. } = intrinsic.algebra() else {
            continue;
        };
        let mut masking = Visit::masking(|_, _: &Var| None, term("Type"));
        intrinsic.traverse(&mut masking);
        let children = masking.take_masked_children();
        let declared = operands
            .as_slice()
            .iter()
            .map(|operand| (*operand).clone())
            .collect::<Vec<_>>();

        assert!(declared.len() <= children.len(), "{application:?}");
        assert_eq!(children[..declared.len()], declared, "{application:?}");
    }
}

/// The families coordination outcomes are read through, at the names the registry gives them. Typing an outcome reads its family's arity and sort and never its cases, so each is declared with none.
const OUTCOMES: &str = r#"
#{polarities = [Strict]}
induct /sys/Option(A: Type): pub Type
end
#{polarities = [Strict]}
induct /sys/Channel/Take(A: Type): pub Type
end
induct /sys/Channel/Push: pub Type
end
"#;

/// The rows no fold answers are the effect carrier's: its two constructors, and the cell and channel operations. Nothing computes against them, and the one thing a wrong row among them could do to admit is hand out what a description yields — `Io` has no eliminator only while no row is one, and a host call that diverges inhabits `Io` at any type. So each is typed at an `Io`, and reduction leaves each the node it was: `bind(pure(x), f)` is not `f(x)`.
///
/// Mutation-checked as the grid above is: `IoBind` producing its bare result type, and a channel's count a bare `Nat`, each fail the first assertion.
#[test]
fn every_row_no_fold_answers_produces_an_inert_description() {
    let mut certifier = certifier_with_bounds();
    declared(&mut certifier, OUTCOMES);
    let rows: [Term; 12] = under(
        &mut certifier,
        "cell: #CellType(#NatType), channel: #ChannelType(#NatType)",
        &[
            "#Cell(#NatType)".to_string(),
            "#CellFill(#NatType, cell, 1)".to_string(),
            "#CellPoll(#NatType, cell, {})".to_string(),
            proved("#Channel(#NatType, 1, ${qed})"),
            "#ChannelPush(#NatType, channel, 1)".to_string(),
            "#ChannelTake(#NatType, channel, {})".to_string(),
            "#ChannelClose(#NatType, channel)".to_string(),
            "#ChannelClosed(#NatType, channel)".to_string(),
            "#ChannelCount(#NatType, channel)".to_string(),
            "#ChannelCapacity(#NatType, channel)".to_string(),
            "#IoPure(#NatType, 1)".to_string(),
            "#IoBind(#NatType, #NatType, #IoPure(#NatType, 1), (item: #NatType) => #IoPure(#NatType, item))"
                .to_string(),
        ]
        .join(", "),
    );

    for application in rows {
        let stated = infer(&mut certifier, &application)
            .unwrap_or_else(|error| panic!("{application:?} is refused: {error:?}"));
        assert!(
            matches!(&*stated, Subterm::Intrinsic(Intrinsic::IoType(_))),
            "{application:?} is typed at {stated:?}, which is no description",
        );

        let reduced = certifier
            .reduce_forced(application.clone())
            .unwrap_or_else(|error| panic!("{application:?} does not reduce: {error:?}"));
        assert_eq!(reduced, application);
    }
}

/// What the grid above hangs from: a fold's value is typed by its literal's row, so a literal's row wrong in step with an operation's would pass there. A literal's payload is a Rust value of one carrier, and its row states that carrier, whose former is small.
///
/// Mutation-checked as the grid is: a handle typed at `Nat` fails here and nowhere else in this file.
#[test]
fn a_literal_is_typed_at_the_carrier_its_payload_is_a_value_of() {
    let mut certifier = certifier();

    for (literal, former) in [
        ("true", "#BoolType"),
        ("0", "#NatType"),
        ("7", "#NatType"),
        ("#Byte(7)", "#ByteType"),
        ("#Int(-7)", "#IntType"),
        ("#Flt(+2.5)", "#FltType"),
        ("x[0xA5, 0xA5]", "#BytesType"),
        ("#Handle(7)", "#HandleType"),
        ("#List(#NatType, [4, 5])", "#ListType(#NatType)"),
    ] {
        let former = term(former);

        assert_eq!(infer(&mut certifier, &term(literal)), Ok(former.clone()));
        assert_eq!(infer(&mut certifier, &former), Ok(term("Type")));
    }
}

/// A parameterized former's sort is its element's level and never its element's sort: a list, a cell, a channel or a description of proofs is data at level zero, and one of types sits where those types do, a level above the types themselves. Reading the element's sort instead would type a list of proofs at `Prop`, and pinning the level at zero would make a list of types as small as the types it lists.
///
/// Mutation-checked at `sort_of_intrinsic`: answering `Prop` for an element that is one fails the first case, and answering level zero whatever the element's level fails the third.
#[test]
fn a_parameterized_former_sits_at_its_elements_level_and_never_in_prop() {
    for former in ["#ListType", "#CellType", "#ChannelType", "#IoType"] {
        let mut certifier = certifier();
        let formed: [Term; 3] = under(
            &mut certifier,
            "P: Prop",
            &fill!("${former}(P), ${former}(#NatType), ${former}(Type)", former),
        );

        for (formed, sort) in formed
            .into_iter()
            .zip(["Type", "Type", "#{level = 1} Type"])
        {
            assert_eq!(infer(&mut certifier, &formed), Ok(term(sort)));
        }
    }
}
