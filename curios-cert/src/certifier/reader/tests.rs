//! The reading door: each entry asked of terms the certifier accepted.

use {
    crate::*,
    curios_analysis::{Erased, test_support::*},
    curios_common::{Plicity, Qualifier},
    curios_core::{
        Atom, Cost, Free, Global, InductDecl, InductParam, InductType, Intrinsic, Nat, Subterm,
        Telescope, Term, Totality, UniverseContext, Universes,
    },
};

fn certifier() -> Certifier<'static> {
    Certifier::new(100_000, SYNTAX)
}

fn binder(index: u32, hint: &str) -> Free {
    Free::local(u64::from(index), Some(hint))
}

fn nat(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}

fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

fn boolean(value: bool) -> Term {
    Term::intrinsic(Intrinsic::Bool(value))
}

fn bool_type() -> Term {
    Term::intrinsic(Intrinsic::BoolType)
}

fn succ(tail: Term) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::Succ(1u32.into(), tail)))
}

/// `Ix : (Nat) -> Type` with `z : Ix(0)` and `s(p : Nat) : Ix(p + 1)`.
fn declare_ix(certifier: &mut Certifier) -> Global {
    let family = Global::Authored(Qualifier::from(["Ix"]));
    let p = binder(90, "p");
    let constructors = vec![
        (
            Atom::from("z"),
            InductParam::new(Telescope::done(vec![nat(0)]), Vec::new()),
        ),
        (
            Atom::from("s"),
            InductParam::new(
                Telescope::build([(p, nat_type())], vec![succ(Term::free_var(&p))]),
                vec![Plicity::Explicit],
            ),
        ),
    ];
    certifier
        .declare_induct(
            &family,
            &InductDecl {
                universe_context: UniverseContext::default(),
                arity: Telescope::done(Telescope::build([(binder(91, "i"), nat_type())], ())),
                constructors: constructors.into(),
                result_sort: Term::type_ground(),
                module: Qualifier::from(["Ix"]),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )
        .expect("a name is declared once");

    family
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
    let x = binder(0, "x");
    let term = Term::apply(Term::func([(x, nat_type())], Term::free_var(&x)), [nat(3)]);
    assert_eq!(infer(&mut certifier, &term), Ok(nat_type()));

    assert_eq!(certifier.read(|reader| reader.head(&term)), Ok(nat(3)));
}

/// A neutral's type, and a type former's sort, are what the certifier infers.
#[test]
fn a_reader_infers_a_neutrals_type_and_a_types_sort() {
    let mut certifier = certifier();
    let f = binder(0, "f");
    let n = binder(1, "n");
    certifier.assume(
        &f,
        &Term::func_type([(binder(2, "m"), nat_type())], bool_type()),
    );
    certifier.assume(&n, &nat_type());
    let call = Term::apply(Term::free_var(&f), [Term::free_var(&n)]);

    certifier.read(|reader| {
        assert_eq!(reader.infer(&call), Ok(bool_type()));
        assert_eq!(reader.infer(&nat_type()), Ok(Term::type_ground()));
    });
}

/// A value's half is classified as the erasure obligations classify a position: a proof at a proposition, a type at a sort, and neither at `Nat`.
#[test]
fn a_reader_classifies_a_types_values_as_the_obligations_do() {
    let mut certifier = certifier();
    let proposition = binder(0, "P");
    certifier.assume(&proposition, &Term::prop());

    certifier.read(|reader| {
        assert_eq!(
            reader.erased(&Term::free_var(&proposition)),
            Ok(Some(Erased::Proof))
        );
        assert_eq!(reader.erased(&Term::prop()), Ok(Some(Erased::Type)));
        assert_eq!(reader.erased(&nat_type()), Ok(None));
    });
}

/// A reader records nothing of the walk that certified: the proof a check records as an erased position, a reader types and leaves unrecorded.
///
/// Mutation-checked: with `Certifier::read` not holding the positions for reading, the reader's typing records the proof.
#[test]
fn a_reader_records_no_position() {
    let mut certifier = certifier();
    let proposition = binder(0, "P");
    let proof = binder(1, "h");
    certifier.assume(&proposition, &Term::prop());
    certifier.assume(&proof, &Term::free_var(&proposition));

    assert!(infer(&mut certifier, &Term::free_var(&proof)).is_ok());
    assert_eq!(
        certifier.take_checked().0.len(),
        1,
        "a check records the proof"
    );

    let typed = certifier.read(|reader| reader.infer(&Term::free_var(&proof)));
    assert_eq!(typed, Ok(Term::free_var(&proposition)));
    assert!(certifier.take_checked().0.is_empty());
}

/// Nor does a walk take what a reader typed: `rec f(n : Nat) -> Type = f(n)` yields a sort and does not descend, and a reader types it recording no call, so the walk that types it afterwards types it again, grades the call and refuses it, with the memos on as with them off.
///
/// Mutation-checked: with the read's typings kept past it, the walk answers from the reader's and the group stands `Total`.
#[test]
fn a_walk_types_again_what_a_reader_typed() {
    let f = binder(0, "f");
    let n = binder(1, "n");
    let group = Term::rec(
        [(
            f,
            Term::func_type([(n, nat_type())], Term::type_ground()),
            Term::func(
                [(n, nat_type())],
                Term::apply(Term::free_var(&f), [Term::free_var(&n)]),
            ),
        )],
        Term::free_var(&f),
    );
    let Subterm::Rec(rec) = &*group else {
        panic!("a group is stated: {group:?}");
    };

    for mut certifier in [certifier(), Certifier::uncached(100_000, SYNTAX)] {
        assert!(certifier.read(|reader| reader.infer(&group)).is_ok());
        assert!(matches!(
            infer(&mut certifier, &group),
            Err(Error::NotDescending { .. })
        ));
        assert_eq!(certifier.group_verdict(&rec.group), Some(Totality::Partial));
    }
}

/// A case stands for its value inside the frame it was put in force in, as an arm is checked under it, and nowhere after: a variable scrutinee bound to the value, and an expression's equation recorded.
///
/// Mutation-checked: with the case's solution left unbound, `b` reduces to itself inside the frame.
#[test]
fn a_case_stands_for_its_value_within_its_frame() {
    let mut certifier = certifier();
    let b = binder(0, "b");
    let f = binder(1, "f");
    let n = binder(2, "n");
    certifier.assume(&b, &bool_type());
    certifier.assume(
        &f,
        &Term::func_type([(binder(3, "m"), nat_type())], bool_type()),
    );
    certifier.assume(&n, &nat_type());
    let variable = Term::free_var(&b);
    let call = Term::apply(Term::free_var(&f), [Term::free_var(&n)]);

    certifier.read(|reader| {
        reader.scoped(|reader| {
            assert_eq!(reader.case(&variable, &boolean(true)), Ok(()));
            assert_eq!(reader.case(&call, &boolean(false)), Ok(()));
            assert_eq!(reader.head(&variable), Ok(boolean(true)));
            assert_eq!(reader.head(&call), Ok(boolean(false)));
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
    let b = binder(0, "b");
    let subject = binder(1, "subject");
    let five = binder(2, "five");
    let q = binder(3, "q");
    certifier.assume(&b, &nat_type());
    certifier.assume(
        &subject,
        &Term::induct_type(family, Vec::<Term>::new(), [Term::free_var(&b)]),
    );
    certifier.assume(
        &five,
        &Term::induct_type(family, Vec::<Term>::new(), [nat(5)]),
    );

    certifier.read(|reader| {
        reader.scoped(|reader| {
            let entered = reader
                .enter_arm(
                    &ix(family, Term::free_var(&b)),
                    &Term::free_var(&subject),
                    &Atom::from("s"),
                    &[q],
                )
                .expect("the arm is entered");
            let value = Term::variant(family, Vec::<Term>::new(), "s", vec![Term::free_var(&q)]);

            assert_eq!(entered.targets, vec![succ(Term::free_var(&q))]);
            assert_eq!(entered.value, value);
            assert!(!entered.ruled_out);
            assert_eq!(
                reader.head(&Term::free_var(&b)),
                Ok(succ(Term::free_var(&q)))
            );
            assert_eq!(reader.head(&Term::free_var(&subject)), Ok(value));
        });
        reader.scoped(|reader| {
            let entered = reader
                .enter_arm(
                    &ix(family, nat(5)),
                    &Term::free_var(&five),
                    &Atom::from("z"),
                    &[],
                )
                .expect("the arm is entered");

            assert!(entered.ruled_out);
        });
    });
}

/// A binding is read by value, a fresh binder names nothing in scope, and the binding goes with its frame.
#[test]
fn a_reader_binds_a_fresh_local_within_its_frame() {
    let mut certifier = certifier();
    let x = binder(0, "x");
    certifier.assume(&x, &nat_type());

    certifier.read(|reader| {
        let y = reader.fresh(Some("y"));
        assert_ne!(y, x);
        reader.scoped(|reader| {
            reader.bind(&y, &nat_type(), &nat(2));
            assert_eq!(reader.head(&Term::free_var(&y)), Ok(nat(2)));
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
        .define(&name, &nat_type(), &nat(3), &UniverseContext::default())
        .expect("a name is defined once");
    let _ = certifier.take_reads();

    certifier.read(|reader| {
        assert!(reader.induct_decl(&family).is_some());
        assert_eq!(
            reader.head(&Term::instance(name, Universes::default())),
            Ok(nat(3))
        );
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
