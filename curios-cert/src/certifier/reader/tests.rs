//! The reading door: each entry asked of terms the certifier accepted.

use {
    crate::{test_support::*, *},
    curios_analysis::{Erased, test_support::*},
    curios_common::Qualifier,
    curios_core::{
        Cost, Global, InductType, Intrinsic, Nat, Subterm, Term, Totality, UniverseContext,
        Universes,
    },
};

fn certifier() -> Certifier<'static> {
    Certifier::new(100_000, SYNTAX)
}

/// `Ix : (Nat) -> Type` with `z : Ix(0)` and `s(p : Nat) : Ix(p + 1)`.
const IX: &str = r#"
induct /Ix: (i: #NatType) -> pub Type
| z(): (0)
| s(p: #NatType): (#Nat(Succ(1, p)))
end
"#;

/// [`IX`] declared in `certifier`, by its name.
fn declare_ix(certifier: &mut Certifier) -> Global {
    declared(certifier, IX);

    Global::Authored(Qualifier::from(["Ix"]))
}

fn ix(family: Global, index: Term) -> InductType {
    InductType {
        name: family,
        universes: Universes::default(),
        params: Vec::new(),
        indices: vec![index],
    }
}

/// Reduction to a head is the certifier's: a function the certifier accepted, applied, reduces to what it returns.
#[test]
fn a_reader_reduces_a_term_to_its_head() {
    let mut certifier = certifier();
    let term = term("((x: #NatType) => x)(3)");
    assert_eq!(infer(&mut certifier, &term), Ok(self::term("#NatType")));

    assert_eq!(
        certifier.read(|reader| reader.head(&term)),
        Ok(self::term("3"))
    );
}

/// A neutral's type, and a type former's sort, are what the certifier infers.
#[test]
fn a_reader_infers_a_neutrals_type_and_a_types_sort() {
    let mut certifier = certifier();
    let (_, call) = opened(
        &mut certifier,
        "(f: (m: #NatType) -> #BoolType, n: #NatType) => f(n)",
    );

    certifier.read(|reader| {
        assert_eq!(reader.infer(&call), Ok(term("#BoolType")));
        assert_eq!(reader.infer(&term("#NatType")), Ok(term("Type")));
    });
}

/// A value's half is classified as the erasure obligations classify a position: a proof at a proposition, a type at a sort, and neither at `Nat`.
#[test]
fn a_reader_classifies_a_types_values_as_the_obligations_do() {
    let mut certifier = certifier();
    let (_, proposition) = opened(&mut certifier, "(P: Prop) => P");

    certifier.read(|reader| {
        assert_eq!(reader.erased(&proposition), Ok(Some(Erased::Proof)));
        assert_eq!(reader.erased(&term("Prop")), Ok(Some(Erased::Type)));
        assert_eq!(reader.erased(&term("#NatType")), Ok(None));
    });
}

/// A reader records nothing of the walk that certified: the proof a check records as an erased position, a reader types and leaves unrecorded.
///
/// Mutation-checked: with `Certifier::read` not holding the positions for reading, the reader's typing records the proof.
#[test]
fn a_reader_records_no_position() {
    let mut certifier = certifier();
    let (locals, proof) = opened(&mut certifier, "(P: Prop, h: P) => h");

    assert!(infer(&mut certifier, &proof).is_ok());
    assert_eq!(
        certifier.take_checked().0.len(),
        1,
        "a check records the proof"
    );

    let typed = certifier.read(|reader| reader.infer(&proof));
    assert_eq!(typed, Ok(Term::free_var(&locals[0])));
    assert!(certifier.take_checked().0.is_empty());
}

/// Nor does a walk take what a reader typed: `rec f(n : Nat) -> Type = f(n)` yields a sort and does not descend, and a reader types it recording no call, so the walk that types it afterwards types it again, grades the call and refuses it, with the memos on as with them off.
///
/// Mutation-checked: with the read's typings kept past it, the walk answers from the reader's and the group stands `Total`.
#[test]
fn a_walk_types_again_what_a_reader_typed() {
    let group = term("#{node = Rec} (let f: (n: #NatType) -> Type = (n: #NatType) => f(n); f)");
    let Subterm::Rec(rec) = &*group else {
        panic!("a group is stated: {group:?}");
    };

    for mut certifier in [certifier(), Certifier::uncached(100_000, SYNTAX)] {
        assert!(certifier.read(|reader| reader.infer(&group)).is_ok());
        assert!(matches!(
            infer(&mut certifier, &group),
            Err(Error::NotDescending { .. })
        ));
        assert_eq!(
            certifier.group_verdict(rec.group()),
            Some(Totality::Partial)
        );
    }
}

/// A case stands for its value inside the frame it was put in force in, as an arm is checked under it, and nowhere after: a variable scrutinee bound to the value, and an expression's equation recorded.
///
/// Mutation-checked: with the case's solution left unbound, `b` reduces to itself inside the frame.
#[test]
fn a_case_stands_for_its_value_within_its_frame() {
    let mut certifier = certifier();
    let (_, both) = opened(
        &mut certifier,
        "(b: #BoolType, f: (m: #NatType) -> #BoolType, n: #NatType) => (b, f(n))",
    );
    let [variable, call] = <[Term; 2]>::try_from(components(&both)).expect("two terms");

    certifier.read(|reader| {
        reader.scoped(|reader| {
            assert_eq!(reader.case(&variable, &term("true")), Ok(()));
            assert_eq!(reader.case(&call, &term("false")), Ok(()));
            assert_eq!(reader.head(&variable), Ok(term("true")));
            assert_eq!(reader.head(&call), Ok(term("false")));
        });
        assert_eq!(reader.head(&variable), Ok(variable.clone()));
        assert_eq!(reader.head(&call), Ok(call.clone()));
    });
}

/// An arm is entered as it is checked: the payload under the reader's binders, the case's targets and value handed back, the index the case forces solved and the scrutinee standing for the value. An arm whose indices clash is entered and said to be ruled out.
///
/// Mutation-checked: with the solutions left unbound, `b` reduces to itself inside the arm.
#[test]
fn a_reader_enters_an_arm_as_it_is_checked() {
    let mut certifier = certifier();
    let family = declare_ix(&mut certifier);
    let (_, hypotheses) = opened(
        &mut certifier,
        "(b: #NatType, subject: #{node = InductType} /Ix()(b), five: #{node = InductType} /Ix()(5)) => (b, subject, five)",
    );
    let [b, subject, five] = <[Term; 3]>::try_from(components(&hypotheses)).expect("three terms");
    let q = certifier.fresh(Some("q"));

    certifier.read(|reader| {
        reader.scoped(|reader| {
            let entered = reader
                .enter_arm(&ix(family, b.clone()), &subject, &"s".into(), &[q])
                .expect("the arm is entered");
            let (successor, value) = (
                Term::intrinsic(Intrinsic::Nat(Nat::Succ(1u32.into(), Term::free_var(&q)))),
                Term::variant(family, Vec::<Term>::new(), "s", vec![Term::free_var(&q)]),
            );

            assert_eq!(entered.targets, vec![successor.clone()]);
            assert_eq!(entered.value, value);
            assert!(!entered.ruled_out);
            assert_eq!(reader.head(&b), Ok(successor));
            assert_eq!(reader.head(&subject), Ok(value));
        });
        reader.scoped(|reader| {
            let entered = reader
                .enter_arm(&ix(family, term("5")), &five, &"z".into(), &[])
                .expect("the arm is entered");

            assert!(entered.ruled_out);
        });
    });
}

/// A binding is read by value, a fresh binder names nothing in scope, and the binding goes with its frame.
#[test]
fn a_reader_binds_a_fresh_local_within_its_frame() {
    let mut certifier = certifier();
    let (locals, _) = opened(&mut certifier, "(x: #NatType) => x");

    certifier.read(|reader| {
        let y = reader.fresh(Some("y"));
        assert_ne!(y, locals[0]);
        reader.scoped(|reader| {
            reader.bind(&y, &term("#NatType"), &term("2"));
            assert_eq!(reader.head(&Term::free_var(&y)), Ok(term("2")));
        });
        assert_eq!(reader.head(&Term::free_var(&y)), Ok(Term::free_var(&y)));
    });
}

/// The lookups read the certifier's environment and note what they read: a family's declaration as a signature, and a definition's body as a body where a reduction opens it.
#[test]
fn a_reader_looks_up_and_notes_what_it_read() {
    let mut certifier = certifier();
    let family = declare_ix(&mut certifier);
    let name = Global::Authored(Qualifier::from(["three"]));
    certifier
        .define(
            &name,
            &term("#NatType"),
            &term("3"),
            &UniverseContext::default(),
        )
        .expect("a name is defined once");
    let _ = certifier.take_reads();

    certifier.read(|reader| {
        assert!(reader.induct_decl(&family).is_some());
        assert_eq!(reader.head(&term("/three")), Ok(term("3")));
        let reads = reader.take_reads();

        assert!(reads.signatures.contains(&family));
        assert!(reads.bodies.contains(&name));
    });
}

/// A reader spends the certifier's budget, and restores it as a declaration's judgment starts on it.
#[test]
fn a_reader_spends_and_restores_the_certifiers_budget() {
    let mut certifier = Certifier::new(1, SYNTAX);

    certifier.read(|reader| {
        assert!(reader.spend(Cost::STEP).is_ok());
        assert!(reader.spend(Cost::STEP).is_err());
        reader.restore_budget();
        assert!(reader.spend(Cost::STEP).is_ok());
    });
}
