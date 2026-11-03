use {
    crate::{Date, Proof, Status, Ticket, Witness, declared, false_type, refused},
    curios_common::{Plicity, Qualifier},
    curios_core::{
        Atom, Definitions, Free, Global, InductDecl, InductParam, Many, Nominal, Scope, Telescope,
        Term, UniverseContext,
    },
};

pub(super) const TICKET: Ticket = Ticket {
    title: "A proof index refined by irrelevance is graded as the constructor it was unified with",
    found_on: Date::new(2026, 10, 30),
    status: Status::Fixed,
    witnesses: &[Witness {
        what: "a recursion that hands its own proof back, graded below the index the proof's target refined",
        proof: Proof::Module(a_recursion_on_a_refined_proof_index),
        expect: refused![kernel: curios_cert::Error::NotDescending { .. }],
    }],
};

/// `induct Wit : Prop | base() | w(inner : Wit) end`, `induct Q : (Wit) -> Prop | c(v : Wit) : (Wit/w(v)) end`, and `rec f : (w0 : Wit, q : Q(w0)) -> False = (w0, q) => match q : (_, _) => False | c(v) => f(v, q) end`, closed with `f(Wit/base(), Q/c(Wit/base()))`.
///
/// The arm's inversion refines `w0` to `Wit/w(v)`, so the size context reads `v` below `w0`; but at a proposition `Wit/w(v)` is every proof of `Wit`, `q` is at `Q(v)` by irrelevance, and the call hands `f` the very `q` it matched, whose payload is `v` again.
fn a_recursion_on_a_refined_proof_index() -> (Definitions, Term) {
    let wit = Global::Authored(Qualifier::from(["Wit"]));
    let family = Global::Authored(Qualifier::from(["Q"]));
    let wit_type = || Term::induct_type(wit, Vec::<Term>::new(), Vec::<Term>::new());
    let at = |index: Term| Term::induct_type(family, Vec::<Term>::new(), [index]);
    let base = || Term::variant(wit, Vec::<Term>::new(), "base", Vec::<Term>::new());

    let inner = Free::local(980, Some("inner"));
    let wit_decl = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![
            (
                Atom::from("base"),
                InductParam::new(Telescope::done(Vec::new()), Vec::new()),
            ),
            (
                Atom::from("w"),
                InductParam::new(
                    Telescope::build([(inner, wit_type())], Vec::new()),
                    vec![Plicity::Explicit],
                ),
            ),
        ]
        .into(),
        result_sort: Term::prop(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let held = Free::local(981, Some("v"));
    let q_decl = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build(
            [(Free::local(982, Some("i")), wit_type())],
            (),
        )),
        constructors: vec![(
            Atom::from("c"),
            InductParam::new(
                Telescope::build(
                    [(held, wit_type())],
                    vec![Term::variant(
                        wit,
                        Vec::<Term>::new(),
                        "w",
                        [Term::free_var(&held)],
                    )],
                ),
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

    let definitions = Definitions {
        items: vec![
            declared(wit, Nominal::Inductive(wit_decl)),
            declared(family, Nominal::Inductive(q_decl)),
        ]
        .into(),
        ..Definitions::default()
    };

    let member = Free::local(983, Some("f"));
    let index = Free::local(984, Some("w0"));
    let proof = Free::local(985, Some("q"));
    let payload = Free::local(986, Some("v"));

    // (w0, q) => match q : (_, _) => False | c(v) => f(v, q) end
    let body = Term::func(
        [(index, wit_type()), (proof, at(Term::free_var(&index)))],
        Term::induct_match_scoped_marked(
            Term::free_var(&proof),
            Scope::constant(Many(2), false_type()),
            [(
                "c",
                vec![(Plicity::Explicit, payload)],
                Term::apply(
                    Term::free_var(&member),
                    [Term::free_var(&payload), Term::free_var(&proof)],
                ),
            )],
            None,
        ),
    );

    let stated = Free::local(987, Some("w0"));
    let proof_term = Term::rec(
        [(
            member,
            Term::func_type(
                [
                    (stated, wit_type()),
                    (Free::local(988, Some("q")), at(Term::free_var(&stated))),
                ],
                false_type(),
            ),
            body,
        )],
        Term::apply(
            Term::free_var(&member),
            [
                base(),
                Term::variant(family, Vec::<Term>::new(), "c", [base()]),
            ],
        ),
    );

    (definitions, proof_term)
}
