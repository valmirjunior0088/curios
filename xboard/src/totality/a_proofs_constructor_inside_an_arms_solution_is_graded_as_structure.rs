use {
    crate::{Date, Proof, Status, Ticket, Witness, false_type, refused},
    curios_core::{
        Atom, Definition, Free, Global, InductDecl, InductParam, Item, Many, Module, Nominal,
        RecItem, Scope, Telescope, Term, UniverseContext,
    },
    curios_utilities::{Plicity, Qualifier},
};

pub(super) const TICKET: Ticket = Ticket {
    title: "A proof's constructor inside an arm's solution is graded as structure",
    found_on: Date::new(2026, 10, 30),
    status: Status::Fixed,
    witnesses: &[Witness {
        what: "a recursion graded below a data binder through the proof its index solution holds",
        proof: Proof::Module(a_recursion_through_a_proofs_constructor),
        expect: refused![kernel: curios_cert::Error::NotDescending { .. }],
    }],
};

/// `induct Wit : Prop | base() | w(b : Box) end and Box : Type | box(x : Wit) end`, `induct Q : (Box) -> Prop | c(n : Box) : (Box/box(Wit/w(n))) end`, and `rec f : (t : Box, q : Q(t)) -> False = (t, q) => match q : (_, _) => False | c(n) => match n : (_) => False | box(p) => f(n, q) end end`, closed with `f(Box/box(Wit/w(Box/box(Wit/base()))), Q/c(Box/box(Wit/base())))`.
///
/// The arm's inversion refines `t`, which binds data, to the case's target `Box/box(Wit/w(n))`, and the size order reads that solution as a constructor tree: `n` sits under `Wit/w`, so the call's `n` grades below `t` while `q` is handed back unchanged, and the recursion repeats for ever.
///
/// The solution is the outer direction's (`invert_indices_outer`), which solves a context variable against the whole target rather than decomposing it, so the guard that yields no equation at a `Prop`-sorted variant never fires. The equation holds, by the irrelevance that makes `Wit/base()` and `Wit/w(Box/box(Wit/base()))` one proof; what it does not state is a fact about the value `t` has, which no reduction took apart.
fn a_recursion_through_a_proofs_constructor() -> (Module, Term) {
    let wit = Global::Authored(Qualifier::from(["Wit"]));
    let boxed = Global::Authored(Qualifier::from(["Box"]));
    let family = Global::Authored(Qualifier::from(["Q"]));
    let wit_type = || Term::induct_type(wit, Vec::<Term>::new(), Vec::<Term>::new());
    let box_type = || Term::induct_type(boxed, Vec::<Term>::new(), Vec::<Term>::new());
    let at = |index: Term| Term::induct_type(family, Vec::<Term>::new(), [index]);
    let base = || Term::variant(wit, Vec::<Term>::new(), "base", Vec::<Term>::new());
    let held = |inner: Term| Term::variant(wit, Vec::<Term>::new(), "w", [inner]);
    let wrapped = |inner: Term| Term::variant(boxed, Vec::<Term>::new(), "box", [inner]);

    let declaration = |constructors: Vec<(Atom, InductParam)>,
                       arity: Telescope<Telescope<()>>,
                       result_sort: Term| InductDecl {
        universe_context: UniverseContext::default(),
        arity,
        constructors: constructors.into(),
        result_sort,
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };
    let payload = |binder: Free, type_: Term, targets: Vec<Term>| {
        InductParam::new(
            Telescope::build([(binder, type_)], targets),
            vec![Plicity::Explicit],
        )
    };

    let wit_decl = declaration(
        vec![
            (
                Atom::from("base"),
                InductParam::new(Telescope::done(Vec::new()), Vec::new()),
            ),
            (
                Atom::from("w"),
                payload(Free::local(990, Some("b")), box_type(), Vec::new()),
            ),
        ],
        Telescope::done(Telescope::done(())),
        Term::prop(),
    );
    let box_decl = declaration(
        vec![(
            Atom::from("box"),
            payload(Free::local(991, Some("x")), wit_type(), Vec::new()),
        )],
        Telescope::done(Telescope::done(())),
        Term::type_ground(),
    );

    // `Wit` holds a `Box` and `Box` holds a `Wit`, so the two are declared as one group, which is what puts a proof's constructor over a payload a program can read.
    let carriers = Item::Rec(RecItem::new(vec![
        Definition::former(wit, Qualifier::default(), Nominal::Inductive(wit_decl)),
        Definition::former(boxed, Qualifier::default(), Nominal::Inductive(box_decl)),
    ]));

    let index = Free::local(992, Some("n"));
    let q_decl = declaration(
        vec![(
            Atom::from("c"),
            payload(
                index,
                box_type(),
                vec![wrapped(held(Term::free_var(&index)))],
            ),
        )],
        Telescope::done(Telescope::build(
            [(Free::local(993, Some("i")), box_type())],
            (),
        )),
        Term::prop(),
    );

    let module = Module {
        items: vec![
            carriers,
            Item::Rec(RecItem::new(vec![Definition::former(
                family,
                Qualifier::default(),
                Nominal::Inductive(q_decl),
            )])),
        ]
        .into(),
        ..Module::default()
    };

    let member = Free::local(994, Some("f"));
    let binder = Free::local(995, Some("t"));
    let proof = Free::local(996, Some("q"));
    let solved = Free::local(997, Some("n"));
    let inner = Free::local(998, Some("p"));

    // (t, q) => match q : (_, _) => False | c(n) => match n : (_) => False | box(p) => f(n, q) end end
    let body = Term::func(
        [(binder, box_type()), (proof, at(Term::free_var(&binder)))],
        Term::induct_match_scoped_marked(
            Term::free_var(&proof),
            Scope::constant(Many(2), false_type()),
            [(
                "c",
                vec![(Plicity::Explicit, solved)],
                Term::induct_match_scoped_marked(
                    Term::free_var(&solved),
                    Scope::constant(Many(1), false_type()),
                    [(
                        "box",
                        vec![(Plicity::Explicit, inner)],
                        Term::apply(
                            Term::free_var(&member),
                            [Term::free_var(&solved), Term::free_var(&proof)],
                        ),
                    )],
                    None,
                ),
            )],
            None,
        ),
    );

    let stated = Free::local(999, Some("t"));
    let proof_term = Term::rec(
        [(
            member,
            Term::func_type(
                [
                    (stated, box_type()),
                    (Free::local(1000, Some("q")), at(Term::free_var(&stated))),
                ],
                false_type(),
            ),
            body,
        )],
        Term::apply(
            Term::free_var(&member),
            [
                wrapped(held(wrapped(base()))),
                Term::variant(family, Vec::<Term>::new(), "c", [wrapped(base())]),
            ],
        ),
    );

    (module, proof_term)
}
