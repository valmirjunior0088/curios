//! Each roster row held to what it types: a literal at the carrier its payload is a value of, a fold's value at the carrier its row states and nowhere past the domain it states, an operation the algebra declares at the carrier it is declared at, a row no fold answers at a description, and a parameterized former at its element's level.

use {
    super::test_support::*,
    crate::{Error, Kernel, infer},
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Atom, Declaration, Free, Global, InductDecl, InductParam, Intrinsic, Nat, Polarity,
        Reducer, Subterm, Telescope, Term, UniverseContext,
    },
    curios_num::{Binary, Floating, Grain, Integer, Natural, Rounding},
    curios_utilities::{Qualifier, SyntaxName},
};

/// A family over `parameters` types with no index, at `result_sort`.
fn family(
    parameters: u32,
    constructors: Vec<(Atom, InductParam)>,
    result_sort: Term,
) -> InductDecl {
    InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::build(
            (0..parameters).map(|index| (binder(90 + index, "A"), Term::type_ground())),
            Telescope::done(()),
        ),
        constructors,
        result_sort,
        module: Qualifier::default(),
        rep_public: true,
        polarities: (0..parameters).map(|_| Polarity::Strict).collect(),
    }
}

/// A kernel holding the vocabulary a bound is stated in, defined as `/sys` defines it — `True` with its one proof, a proposition with none, and `Holds(b)` reducing to one or the other — so a bound is discharged by its decision reducing, as a program's is, rather than assumed.
fn kernel_with_bounds() -> Kernel {
    let mut kernel = kernel();
    let nullary = |name| Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new());

    let truth = Global::Authored(SYNTAX.proof.true_type.qualifier());
    let falsity = Global::Authored(Qualifier::from(["False"]));
    kernel.declare_induct(
        &truth,
        &family(
            0,
            vec![(
                Atom::from("qed"),
                InductParam::new(Telescope::done(Vec::new()), Vec::new()),
            )],
            Term::prop(),
        ),
    );
    kernel.declare_induct(&falsity, &family(0, Vec::new(), Term::prop()));

    let decision = binder(0, "b");
    kernel.define(
        &Free::global(SYNTAX.proof.holds.qualifier()),
        &Term::func_type([(binder(1, "b"), bool_type())], Term::prop()),
        &Term::func(
            [(decision, bool_type())],
            Term::bool_match(
                Term::free_var(&decision),
                None,
                Term::prop(),
                nullary(falsity),
                nullary(truth),
            ),
        ),
        &UniverseContext::default(),
    );

    kernel
}

/// The family a coordination outcome is read through, at the name the registry gives it. Typing an outcome reads its family's arity and sort and never its cases, so it is declared with none.
fn declare_outcome(kernel: &mut Kernel, name: SyntaxName, parameters: u32) {
    kernel.declare_induct(
        &Global::Authored(name.qualifier()),
        &family(parameters, Vec::new(), Term::type_ground()),
    );
}

/// The one proof of `True`, which closes every bound whose decision reduces to `true` and no other.
fn qed() -> Term {
    Term::variant(
        Global::Authored(SYNTAX.proof.true_type.qualifier()),
        Vec::<Term>::new(),
        "qed",
        Vec::<Term>::new(),
    )
}

fn truth(value: bool) -> Term {
    Term::intrinsic(Intrinsic::Bool(value))
}

fn byte(value: u8) -> Term {
    Term::intrinsic(Intrinsic::Byte(value))
}

fn int(value: i32) -> Term {
    Term::intrinsic(Intrinsic::Int(Integer::from(value)))
}

fn flt(value: f64) -> Term {
    Term::intrinsic(Intrinsic::Flt(Floating::from_bits(value.to_bits())))
}

fn bytes(run: &[u8]) -> Term {
    Term::intrinsic(Intrinsic::Bin(Grain::X, Binary::from_bytes(run.to_vec())))
}

fn bits(run: &[bool]) -> Term {
    Term::intrinsic(Intrinsic::Bin(
        Grain::B,
        Binary::from_bits(run.iter().copied()),
    ))
}

fn list(items: &[usize]) -> Term {
    Term::intrinsic(Intrinsic::List {
        element: nat_type(),
        items: items.iter().map(|item| nat(*item)).collect(),
    })
}

/// A run of `length` generators at `grain`.
fn run(grain: Grain, length: usize) -> Term {
    match grain {
        Grain::X => bytes(&vec![0xA5; length]),
        Grain::B => bits(&vec![true; length]),
    }
}

/// One generator of `grain`, which no [`run`] holds.
fn generator(grain: Grain) -> Term {
    match grain {
        Grain::X => byte(0x5A),
        Grain::B => truth(false),
    }
}

/// The type a carrier of the algebra is, as the roster spells it.
fn carried(carrier: curios_algebra::Carrier) -> Term {
    Term::intrinsic(match carrier {
        curios_algebra::Carrier::Natural => Intrinsic::NatType,
        curios_algebra::Carrier::Integer => Intrinsic::IntType,
        curios_algebra::Carrier::Boolean => Intrinsic::BoolType,
        curios_algebra::Carrier::Byte => Intrinsic::ByteType,
        curios_algebra::Carrier::Float => Intrinsic::FltType,
        curios_algebra::Carrier::Packed(grain) => Intrinsic::BinType(grain),
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
            | Intrinsic::Bin(..)
            | Intrinsic::List { .. },
        ) => true,
        _ => false,
    }
}

/// One closed application of every row a fold answers, its operands at the carriers the row states and inside the domain it states — at that domain's edge wherever it has one.
fn inside() -> Vec<Term> {
    let rounding = Rounding::TiesToEven;
    let mut rows = Vec::new();

    let bool_binary: [fn(Term, Term) -> Intrinsic; 5] = [
        Intrinsic::BoolAnd,
        Intrinsic::BoolOr,
        Intrinsic::BoolXor,
        Intrinsic::BoolEql,
        Intrinsic::BoolNeq,
    ];
    rows.extend(bool_binary.map(|row| row(truth(true), truth(false))));

    let nat_binary: [fn(Term, Term) -> Intrinsic; 12] = [
        Intrinsic::NatEql,
        Intrinsic::NatNeq,
        Intrinsic::NatAdd,
        Intrinsic::NatSub,
        Intrinsic::NatMul,
        Intrinsic::NatLt,
        Intrinsic::NatLe,
        Intrinsic::NatAnd,
        Intrinsic::NatOr,
        Intrinsic::NatXor,
        Intrinsic::NatShl,
        Intrinsic::NatShr,
    ];
    rows.extend(nat_binary.map(|row| row(nat(7), nat(2))));

    let int_binary: [fn(Term, Term) -> Intrinsic; 10] = [
        Intrinsic::IntEql,
        Intrinsic::IntNeq,
        Intrinsic::IntAdd,
        Intrinsic::IntSub,
        Intrinsic::IntMul,
        Intrinsic::IntLt,
        Intrinsic::IntLe,
        Intrinsic::IntAnd,
        Intrinsic::IntOr,
        Intrinsic::IntXor,
    ];
    rows.extend(int_binary.map(|row| row(int(-7), int(2))));

    // A shift's count is a natural on both carriers.
    let int_shift: [fn(Term, Term) -> Intrinsic; 2] = [Intrinsic::IntShl, Intrinsic::IntShr];
    rows.extend(int_shift.map(|row| row(int(-7), nat(2))));

    let flt_rounded: [fn(Rounding, Term, Term) -> Intrinsic; 4] = [
        Intrinsic::FltAdd,
        Intrinsic::FltSub,
        Intrinsic::FltMul,
        Intrinsic::FltDiv,
    ];
    rows.extend(flt_rounded.map(|row| row(rounding, flt(2.5), flt(-0.5))));

    let flt_binary: [fn(Term, Term) -> Intrinsic; 8] = [
        Intrinsic::FltRem,
        Intrinsic::FltEql,
        Intrinsic::FltNeq,
        Intrinsic::FltLt,
        Intrinsic::FltLe,
        Intrinsic::FltMin,
        Intrinsic::FltMax,
        Intrinsic::FltCopysign,
    ];
    rows.extend(flt_binary.map(|row| row(flt(2.5), flt(-0.5))));

    rows.extend([
        // The one literal that carries a term: a floor of successors over a natural.
        Intrinsic::Nat(Nat::Succ(Natural::from(3u32), nat(4))),
        // The least divisor a quotient takes at each carrier, and a negative one at `Int`.
        Intrinsic::NatDiv {
            dividend: nat(7),
            divisor: nat(1),
            non_zero: qed(),
        },
        Intrinsic::NatRem {
            dividend: nat(7),
            divisor: nat(1),
            non_zero: qed(),
        },
        Intrinsic::IntDiv {
            dividend: int(-7),
            divisor: int(-1),
            non_zero: qed(),
        },
        Intrinsic::IntRem {
            dividend: int(-7),
            divisor: int(-1),
            non_zero: qed(),
        },
        // The greatest natural a byte holds, and the least integer a natural does.
        Intrinsic::ByteToNat(byte(u8::MAX)),
        Intrinsic::NatToByte {
            nat: nat(255),
            below: qed(),
        },
        Intrinsic::NatToInt(nat(7)),
        Intrinsic::IntToNat {
            int: int(0),
            non_neg: qed(),
        },
        Intrinsic::NatToFlt(rounding, nat(7)),
        Intrinsic::IntToFlt(rounding, int(-7)),
        Intrinsic::FltNeg(flt(2.5)),
        Intrinsic::FltAbs(flt(-2.5)),
        Intrinsic::FltSqrt(rounding, flt(2.5)),
        Intrinsic::FltRoundIntegral(rounding, flt(2.5)),
        Intrinsic::FltFma(rounding, flt(2.5), flt(-0.5), flt(1.0)),
        // Both ends of each narrowing's domain: the negative zero and the greatest finite value into `Nat`, the two finite extremes and the least subnormal into `Int`.
        Intrinsic::FltToNat {
            flt: flt(-0.0),
            non_neg: qed(),
        },
        Intrinsic::FltToNat {
            flt: flt(f64::MAX),
            non_neg: qed(),
        },
        Intrinsic::FltToInt {
            flt: flt(f64::MAX),
            finite: qed(),
        },
        Intrinsic::FltMantissa {
            flt: flt(f64::MIN),
            finite: qed(),
        },
        Intrinsic::FltExponent {
            flt: flt(f64::from_bits(1)),
            finite: qed(),
        },
        Intrinsic::FltToLeBytes(flt(2.5)),
        Intrinsic::FltOfLeBytes {
            bin: bytes(&[0; 8]),
            eight_bytes: qed(),
        },
        // A whole number of bytes read as bits, and of bits read as bytes.
        Intrinsic::BinReinterp {
            grain: Grain::X,
            bin: run(Grain::X, 2),
            aligned: qed(),
        },
        Intrinsic::BinReinterp {
            grain: Grain::B,
            bin: run(Grain::B, 8),
            aligned: qed(),
        },
    ]);

    for grain in [Grain::X, Grain::B] {
        rows.extend([
            Intrinsic::BinLen(grain, run(grain, 2)),
            Intrinsic::BinEql(grain, run(grain, 2), run(grain, 1)),
            // The last index, and a window ending where the run does.
            Intrinsic::BinGet {
                grain,
                bin: run(grain, 2),
                index: nat(1),
                in_range: qed(),
            },
            Intrinsic::BinSlice {
                grain,
                bin: run(grain, 2),
                start: nat(1),
                length: nat(1),
                within: qed(),
            },
            Intrinsic::BinAppend {
                grain,
                bin: run(grain, 2),
                element: generator(grain),
            },
            Intrinsic::BinConcat {
                grain,
                operands: vec![run(grain, 2), run(grain, 1)],
            },
            Intrinsic::BinReplicate {
                grain,
                count: nat(3),
                atom: generator(grain),
            },
            Intrinsic::BinAnd {
                grain,
                left: run(grain, 2),
                right: run(grain, 2),
                same_length: qed(),
            },
            Intrinsic::BinOr {
                grain,
                left: run(grain, 2),
                right: run(grain, 2),
                same_length: qed(),
            },
            Intrinsic::BinXor {
                grain,
                left: run(grain, 2),
                right: run(grain, 2),
                same_length: qed(),
            },
        ]);
    }

    let item = binder(10, "item");
    let accumulator = binder(11, "accumulator");
    rows.extend([
        Intrinsic::ListLen {
            element: nat_type(),
            list: list(&[4, 5]),
        },
        Intrinsic::ListGet {
            element: nat_type(),
            list: list(&[4, 5]),
            index: nat(1),
            in_range: qed(),
        },
        Intrinsic::ListSlice {
            element: nat_type(),
            list: list(&[4, 5]),
            start: nat(1),
            length: nat(1),
            within: qed(),
        },
        Intrinsic::ListAppend {
            element: nat_type(),
            list: list(&[4, 5]),
            item: nat(6),
        },
        Intrinsic::ListConcat {
            element: nat_type(),
            operands: vec![list(&[4, 5]), list(&[6])],
        },
        // A map into another carrier, so the image's element type is the row's `to` and not its `from`.
        Intrinsic::ListMap {
            from: nat_type(),
            to: bool_type(),
            list: list(&[4, 5]),
            function: Term::func(
                [(item, nat_type())],
                Term::intrinsic(Intrinsic::nat_lt(Term::free_var(&item), nat(5))),
            ),
        },
        Intrinsic::ListFold {
            element: nat_type(),
            result: nat_type(),
            list: list(&[4, 5]),
            init: nat(0),
            function: Term::func(
                [(item, nat_type()), (accumulator, nat_type())],
                Term::intrinsic(Intrinsic::nat_add(
                    Term::free_var(&item),
                    Term::free_var(&accumulator),
                )),
            ),
        },
    ]);

    rows.into_iter().map(Term::intrinsic).collect()
}

/// One closed application of every row that states a bound, one step past the edge of the domain it states, the bound closed with the proof that closes it inside. A channel's capacity is the one bound here over a row no fold answers.
fn outside() -> Vec<Term> {
    let mut rows = vec![
        Intrinsic::NatDiv {
            dividend: nat(7),
            divisor: nat(0),
            non_zero: qed(),
        },
        Intrinsic::NatRem {
            dividend: nat(7),
            divisor: nat(0),
            non_zero: qed(),
        },
        Intrinsic::IntDiv {
            dividend: int(-7),
            divisor: int(0),
            non_zero: qed(),
        },
        Intrinsic::IntRem {
            dividend: int(-7),
            divisor: int(0),
            non_zero: qed(),
        },
        Intrinsic::NatToByte {
            nat: nat(256),
            below: qed(),
        },
        Intrinsic::IntToNat {
            int: int(-1),
            non_neg: qed(),
        },
        // The least negative value, and the three values that are no number.
        Intrinsic::FltToNat {
            flt: flt(-f64::from_bits(1)),
            non_neg: qed(),
        },
        Intrinsic::FltToNat {
            flt: flt(f64::INFINITY),
            non_neg: qed(),
        },
        Intrinsic::FltToInt {
            flt: flt(f64::INFINITY),
            finite: qed(),
        },
        Intrinsic::FltMantissa {
            flt: flt(f64::NEG_INFINITY),
            finite: qed(),
        },
        Intrinsic::FltExponent {
            flt: flt(f64::NAN),
            finite: qed(),
        },
        Intrinsic::FltOfLeBytes {
            bin: bytes(&[0; 7]),
            eight_bytes: qed(),
        },
        Intrinsic::FltOfLeBytes {
            bin: bytes(&[0; 9]),
            eight_bytes: qed(),
        },
        Intrinsic::BinReinterp {
            grain: Grain::B,
            bin: run(Grain::B, 7),
            aligned: qed(),
        },
        Intrinsic::ListGet {
            element: nat_type(),
            list: list(&[4, 5]),
            index: nat(2),
            in_range: qed(),
        },
        Intrinsic::ListSlice {
            element: nat_type(),
            list: list(&[4, 5]),
            start: nat(1),
            length: nat(2),
            within: qed(),
        },
        Intrinsic::Channel {
            element: nat_type(),
            capacity: nat(0),
            positive: qed(),
        },
    ];

    for grain in [Grain::X, Grain::B] {
        rows.extend([
            Intrinsic::BinGet {
                grain,
                bin: run(grain, 2),
                index: nat(2),
                in_range: qed(),
            },
            Intrinsic::BinSlice {
                grain,
                bin: run(grain, 2),
                start: nat(1),
                length: nat(2),
                within: qed(),
            },
            Intrinsic::BinAnd {
                grain,
                left: run(grain, 2),
                right: run(grain, 1),
                same_length: qed(),
            },
            Intrinsic::BinOr {
                grain,
                left: run(grain, 2),
                right: run(grain, 1),
                same_length: qed(),
            },
            Intrinsic::BinXor {
                grain,
                left: run(grain, 2),
                right: run(grain, 1),
                same_length: qed(),
            },
        ]);
    }

    rows.into_iter().map(Term::intrinsic).collect()
}

/// A row is the definition of what its operation means, so a wrong one disagrees with nothing a program can spell — but it disagrees with the fold, the carrier's own computation, whose operands and result are Rust values of one type each. A row stating another operand carrier than the fold reads leaves a closed application stuck; one stating another result types the application at a carrier its reduct is not a value of, and conversion then holds that reduct at the wrong type; a bound weaker than the fold's domain admits an application the fold has no value for, under laws that take the bound for granted.
///
/// So every application in [`inside`] is typed by its row, folds to a value, and that value is typed by its own literal row at the type the operation's row stated.
///
/// Mutation-checked a row at a time: the `Nat` comparisons producing `Nat` fail here on the two types, an `Int` shift counted by an `Int` on the application its row no longer types, and `NatToByte`'s bound tightened to `nat < 255` on the edge it no longer admits.
#[test]
fn every_fold_lands_in_the_carrier_its_row_states() {
    for application in inside() {
        let mut kernel = kernel_with_bounds();

        let stated = infer(&mut kernel, &application)
            .unwrap_or_else(|error| panic!("{application:?} is refused: {error:?}"));
        let folded = kernel
            .reduce_forced(application.clone())
            .unwrap_or_else(|error| panic!("{application:?} has no value: {error:?}"));

        assert!(
            is_value(&folded),
            "{application:?} is typed and stays stuck at {folded:?}",
        );
        assert_eq!(
            infer(&mut kernel, &folded),
            Ok(stated),
            "{application:?} folds to {folded:?}, a value of another type",
        );
    }
}

/// The other side of each edge: one step past the domain a row states, the proof that closed the bound inside it no longer does, and the kernel refuses the application by that mismatch — at operands the fold has no value for, where it reports or declines.
///
/// The second assertion holds each sample to the edge: one the fold still answered would show a bound stronger than its operation needs, and say nothing about where the row draws it.
///
/// Mutation-checked as the fixture above is: `NatToByte`'s bound loosened to `nat < 257`, and `BinGet`'s to `index <= len`, each admit their sample here.
#[test]
fn a_bounded_row_is_refused_one_step_past_its_folds_domain() {
    for application in outside() {
        let mut kernel = kernel_with_bounds();

        assert!(
            matches!(
                infer(&mut kernel, &application),
                Err(Error::Mismatch { .. })
            ),
            "{application:?} is typed past its bound",
        );
        assert!(
            !kernel
                .reduce_forced(application.clone())
                .is_ok_and(|folded| is_value(&folded)),
            "{application:?} has a value its bound refuses",
        );
    }
}

/// The carriers are written a second time inside the trusted base: `Intrinsic::algebra` declares each operation a law reads at a carrier, and a law is stated per operation of a carrier — a bound over the naturals, a negation over a total order — so an operation declared at a carrier its row does not type is a law applied to values it was never true of. Each declaration is read against its row: a conversion from its source into the carrier, a comparison over the carrier into `Bool`, a shift of the carrier by a natural, and every other operation closed on the carrier.
///
/// The tally is every declaration `algebra` makes today, the two packed ones at both grains, so a grid that stopped reaching them fails. A declaration added there owes [`inside`] its row, and nothing fails until it has one.
///
/// Mutation-checked: `IntShr` declared at the naturals, where a right shift never exceeds what it shifts, fails on the row's `Int`.
#[test]
fn an_operation_is_declared_to_the_algebra_at_the_carrier_its_row_types() {
    let mut declared = 0;

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

        let mut kernel = kernel_with_bounds();
        let mut typed = |term: &Term| {
            infer(&mut kernel, term)
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
            | curios_algebra::Operation::AtMost => (vec![carrier.clone(), carrier], bool_type()),
            curios_algebra::Operation::ShiftLeft | curios_algebra::Operation::ShiftRight => {
                (vec![carrier.clone(), nat_type()], carrier)
            }
            curios_algebra::Operation::Sum
            | curios_algebra::Operation::Difference
            | curios_algebra::Operation::Product
            | curios_algebra::Operation::Quotient
            | curios_algebra::Operation::Remainder
            | curios_algebra::Operation::And
            | curios_algebra::Operation::Or
            | curios_algebra::Operation::Xor => (vec![carrier.clone(), carrier.clone()], carrier),
            curios_algebra::Operation::Concat
            | curios_algebra::Operation::Length
            | curios_algebra::Operation::Append => {
                unreachable!("no intrinsic is declared as an operation on words")
            }
        };

        assert_eq!((operands, result), stated, "{application:?}");
    }

    assert_eq!(declared, 42);
}

/// The rows no fold answers are the effect carrier's: its two constructors, and the cell and channel operations. Nothing computes against them, and the one thing a wrong row among them could do to admit is hand out what a description yields — `Io` has no eliminator only while no row is one, and a host call that diverges inhabits `Io` at any type. So each is typed at an `Io`, and reduction leaves each the node it was: `bind(pure(x), f)` is not `f(x)`.
///
/// Mutation-checked as the grid above is: `IoBind` producing its bare result type, and a channel's count a bare `Nat`, each fail the first assertion.
#[test]
fn every_row_no_fold_answers_produces_an_inert_description() {
    let mut kernel = kernel_with_bounds();
    declare_outcome(&mut kernel, SYNTAX.option.family, 1);
    declare_outcome(&mut kernel, SYNTAX.channel.take, 1);
    declare_outcome(&mut kernel, SYNTAX.channel.push, 0);

    let cell = binder(20, "cell");
    let channel = binder(21, "channel");
    let item = binder(22, "item");
    kernel.assume(&cell, &Term::intrinsic(Intrinsic::CellType(nat_type())));
    kernel.assume(
        &channel,
        &Term::intrinsic(Intrinsic::ChannelType(nat_type())),
    );
    let pure = |value| {
        Term::intrinsic(Intrinsic::IoPure {
            result: nat_type(),
            value,
        })
    };

    let rows = [
        Intrinsic::Cell {
            element: nat_type(),
        },
        Intrinsic::CellFill {
            element: nat_type(),
            cell: Term::free_var(&cell),
            value: nat(1),
        },
        Intrinsic::CellPoll {
            element: nat_type(),
            cell: Term::free_var(&cell),
            universes: Vec::new(),
        },
        Intrinsic::Channel {
            element: nat_type(),
            capacity: nat(1),
            positive: qed(),
        },
        Intrinsic::ChannelPush {
            element: nat_type(),
            channel: Term::free_var(&channel),
            value: nat(1),
        },
        Intrinsic::ChannelTake {
            element: nat_type(),
            channel: Term::free_var(&channel),
            universes: Vec::new(),
        },
        Intrinsic::ChannelClose {
            element: nat_type(),
            channel: Term::free_var(&channel),
        },
        Intrinsic::ChannelClosed {
            element: nat_type(),
            channel: Term::free_var(&channel),
        },
        Intrinsic::ChannelCount {
            element: nat_type(),
            channel: Term::free_var(&channel),
        },
        Intrinsic::ChannelCapacity {
            element: nat_type(),
            channel: Term::free_var(&channel),
        },
        Intrinsic::IoPure {
            result: nat_type(),
            value: nat(1),
        },
        Intrinsic::IoBind {
            from: nat_type(),
            to: nat_type(),
            action: pure(nat(1)),
            continuation: Term::func([(item, nat_type())], pure(Term::free_var(&item))),
        },
    ];

    for application in rows.map(Term::intrinsic) {
        let stated = infer(&mut kernel, &application)
            .unwrap_or_else(|error| panic!("{application:?} is refused: {error:?}"));
        assert!(
            matches!(&*stated, Subterm::Intrinsic(Intrinsic::IoType(_))),
            "{application:?} is typed at {stated:?}, which is no description",
        );

        let reduced = kernel
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
    let mut kernel = kernel();

    for (literal, former) in [
        (truth(true), Intrinsic::BoolType),
        (nat(0), Intrinsic::NatType),
        (nat(7), Intrinsic::NatType),
        (byte(7), Intrinsic::ByteType),
        (int(-7), Intrinsic::IntType),
        (flt(2.5), Intrinsic::FltType),
        (run(Grain::X, 2), Intrinsic::BinType(Grain::X)),
        (run(Grain::B, 2), Intrinsic::BinType(Grain::B)),
        (Term::intrinsic(Intrinsic::Handle(7)), Intrinsic::HandleType),
        (list(&[4, 5]), Intrinsic::ListType(nat_type())),
    ] {
        let former = Term::intrinsic(former);

        assert_eq!(infer(&mut kernel, &literal), Ok(former.clone()));
        assert_eq!(infer(&mut kernel, &former), Ok(Term::type_ground()));
    }
}

/// A parameterized former's sort is its element's level and never its element's sort: a list, a cell, a channel or a description of proofs is data at level zero, and one of types sits where those types do, a level above the types themselves. Reading the element's sort instead would type a list of proofs at `Prop`, and pinning the level at zero would make a list of types as small as the types it lists.
///
/// Mutation-checked at `sort_of_intrinsic`: answering `Prop` for an element that is one fails the first case, and answering level zero whatever the element's level fails the third.
#[test]
fn a_parameterized_former_sits_at_its_elements_level_and_never_in_prop() {
    let formers: [fn(Term) -> Intrinsic; 4] = [
        Intrinsic::ListType,
        Intrinsic::CellType,
        Intrinsic::ChannelType,
        Intrinsic::IoType,
    ];

    for former in formers {
        let mut kernel = kernel();
        let proposition = binder(0, "P");
        kernel.assume(&proposition, &Term::prop());

        for (element, sort) in [
            (Term::free_var(&proposition), Term::type_ground()),
            (nat_type(), Term::type_ground()),
            (Term::type_ground(), Term::type_at(one())),
        ] {
            assert_eq!(
                infer(&mut kernel, &Term::intrinsic(former(element))),
                Ok(sort),
            );
        }
    }
}
