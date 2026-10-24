use {
    crate::{Date, Proof, Status, Ticket, Witness, declared, false_type, refused},
    curios_core::{
        Atom, Free, Global, InductDecl, InductParam, Module, Nominal, Telescope, Term,
        UniverseContext,
    },
    curios_utilities::{Plicity, Qualifier},
};

pub(super) const TICKET: Ticket = Ticket {
    title: "A payload that quantifies over `Prop` is applied at its own family, and the application is graded below its constructor",
    found_on: Date::new(2026, 10, 8),
    status: Status::Fixed,
    witnesses: &[
        Witness {
            what: "a proof by recursion on what a payload returns of the scrutinee, the payload the polymorphic identity",
            proof: Proof::Program(A_PAYLOAD_RETURNS_THE_SCRUTINEE_IT_IS_APPLIED_TO),
            expect: refused![elaborator: curios_elab::Error::UniverseInconsistency { .. }],
        },
        Witness {
            what: "the same recursion on what the payload returns of its constructor built again",
            proof: Proof::Program(A_PAYLOAD_RETURNS_ITS_CONSTRUCTOR_BUILT_AGAIN),
            expect: refused![elaborator: curios_elab::Error::UniverseInconsistency { .. }],
        },
        Witness {
            what: "the recursion on what the payload returns of the scrutinee, as a term",
            proof: Proof::Module(a_payload_applied_at_its_own_family),
            expect: refused![kernel: curios_cert::Error::Oversized { .. }],
        },
    ],
};

/// `f` quantified over every proposition, `Bad` among them, so `f(Bad)(b)` was a `Bad`, and an application of a payload is graded a child of its constructor. With `f` the identity it is `b` itself, and the recursion that was judged to descend took the same value for ever. `Bad` is now sized: holding `f`, it sits above the propositions `f` ranges over, and is none of them.
const A_PAYLOAD_RETURNS_THE_SCRUTINEE_IT_IS_APPLIED_TO: &str = r#"
    use /std/Bool/{False};
    induct Bad: Prop
    | c(f: (A: Prop) -> (A) -> A)
    end
    let bad(b: Bad) -> False = match b | c(f) => bad(f(Bad)(b)) end;
    bad(Bad/c((A) => (x) => x))
    "#;

/// The argument names no variable the arm was handed but the payload: what is graded is the application, whatever it is applied to.
const A_PAYLOAD_RETURNS_ITS_CONSTRUCTOR_BUILT_AGAIN: &str = r#"
    use /std/Bool/{False};
    induct Bad: Prop
    | c(f: (A: Prop) -> (A) -> A)
    end
    let bad(b: Bad) -> False = match b | c(f) => bad(f(Bad)(Bad/c(f))) end;
    bad(Bad/c((A) => (x) => x))
    "#;

/// `induct Bad : Prop | c(f : (A : Prop) -> (x : A) -> A) end` and `rec bad : (b : Bad) -> False = (b) => match b | c(f) => bad(f(Bad)(b)) end`, closed with `bad(Bad/c((A) => (x) => x))`.
///
/// A `Prop`-sorted family carried no size condition, so `Bad` held a payload that quantified over the sort `Bad` was in, and the payload's declared type names `Bad` nowhere, so strict positivity had nothing to refuse. The size order reads an application of a payload as a child of its constructor, on the premise that a payload of a foreign type cannot hand back a value of the family; applied at `Bad` this one did, and at the identity it handed back the scrutinee. The declaration is refused where it is sized: its payload does not fit under `Prop` at level zero.
fn a_payload_applied_at_its_own_family() -> (Module, Term) {
    let family = Global::Authored(Qualifier::from(["Bad"]));
    let family_type = Term::induct_type(family, Vec::<Term>::new(), Vec::<Term>::new());

    // (A : Prop) -> (x : A) -> A
    let proposition = Free::local(920, Some("A"));
    let payload_type = Term::func_type(
        [(proposition, Term::prop())],
        Term::func_type(
            [(Free::local(921, Some("x")), Term::free_var(&proposition))],
            Term::free_var(&proposition),
        ),
    );

    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("c"),
            InductParam::new(
                Telescope::build([(Free::local(922, Some("f")), payload_type)], Vec::new()),
                vec![Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: Term::prop(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let module = Module {
        items: vec![declared(family, Nominal::Inductive(declaration))].into(),
        ..Module::default()
    };

    let member = Free::local(923, Some("bad"));
    let subject = Free::local(924, Some("b"));
    let payload = Free::local(925, Some("f"));

    // (b) => match b | c(f) => bad(f(Bad)(b)) end
    let body = Term::func(
        [(subject, family_type.clone())],
        Term::induct_match(
            Term::free_var(&subject),
            None,
            false_type(),
            [(
                "c",
                vec![payload],
                Term::apply(
                    Term::free_var(&member),
                    [Term::apply(
                        Term::apply(Term::free_var(&payload), [family_type.clone()]),
                        [Term::free_var(&subject)],
                    )],
                ),
            )],
        ),
    );

    // (A) => (x) => x
    let chosen = Free::local(926, Some("A"));
    let handed = Free::local(927, Some("x"));
    let identity = Term::func(
        [(chosen, Term::prop())],
        Term::func([(handed, Term::free_var(&chosen))], Term::free_var(&handed)),
    );

    let proof = Term::rec(
        [(
            member,
            Term::func_type([(Free::local(928, Some("b")), family_type)], false_type()),
            body,
        )],
        Term::apply(
            Term::free_var(&member),
            [Term::variant(family, Vec::<Term>::new(), "c", [identity])],
        ),
    );

    (module, proof)
}
