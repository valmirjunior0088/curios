use {
    curios_core::{
        Atom, Definition, DefinitionKind, Free, Global, InductDecl, InductParam, Item, Module,
        Nominal, Telescope, Term, Totality, UniverseContext,
    },
    curios_utilities::Qualifier,
    xboard::{
        Date, Part, Proof, Status, Ticket, Witness, declared, false_name, false_type, refused,
    },
};

/// What is believed without being judged again? A unit already in scope, a cached verdict, a reused payload, a module taken as finished, an exhausted budget.
pub(super) const ADMISSION: Part = Part {
    name: "admission",
    tickets: &[Ticket {
        title: "A name declared twice means two things to the kernel",
        found_on: Date::new(2026, 10, 6),
        status: Status::Fixed,
        witnesses: &[
            Witness {
                what: "an inhabited `False` under the prelude's own name",
                proof: Proof::Module(a_registry_entry_under_the_preludes_false),
                expect: refused![kernel: curios_cert::Error::Redeclared(_)],
            },
            Witness {
                what: "a theorem typed under one definition of its statement and read under another",
                proof: Proof::Module(a_statement_declared_twice),
                expect: refused![kernel: curios_cert::Error::Redeclared(_)],
            },
        ],
    }],
};

/// A top-level definition, as the walk binds one.
fn authored(name: &Global, type_: Term, body: Term) -> Item {
    Item::Let(Definition {
        name: *name,
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::default(),
        totality: Totality::Total,
        proof: false,
        variances: Vec::new(),
        type_,
        body,
    })
}

/// A nullary family at `result_sort` holding `constructors`, each of no payload.
fn family(result_sort: Term, constructors: &[&str]) -> InductDecl {
    InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: constructors
            .iter()
            .map(|tag| {
                (
                    Atom::from(*tag),
                    InductParam::new(Telescope::done(Vec::new()), Vec::new()),
                )
            })
            .collect(),
        result_sort,
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    }
}

/// One declaration, the prelude's `False` holding a constructor `mk`, closed with `False/mk()`.
///
/// Declared over the environment a walk is seeded with, the entry stands where the prelude's did and is the one every rule reads. Checking it would not refuse it: an inhabited proposition is a well-formed declaration, and what is wrong with this one is its name.
fn a_registry_entry_under_the_preludes_false() -> (Module, Term) {
    let module = Module {
        items: vec![declared(
            false_name(),
            Nominal::Inductive(family(Term::prop(), &["mk"])),
        )]
        .into(),
        ..Module::default()
    };

    (
        module,
        Term::variant(false_name(), Vec::<Term>::new(), "mk", Vec::<Term>::new()),
    )
}

/// `p : Prop = Holds`, then `thm : p = Holds/qed()`, then `p : Prop = ((x : Holds) => False)(thm)`, closed with `thm`.
///
/// A walk takes items as they are stored, and a name declared twice is written twice: `thm` is typed while `p` is `Holds`, and the second `p`, which names `thm`, is defined where the first stood. The closing term is then read at a `p` that reduces to `False`.
fn a_statement_declared_twice() -> (Module, Term) {
    let holds = Global::Authored(Qualifier::from(["Holds"]));
    let statement = Global::Authored(Qualifier::from(["p"]));
    let theorem = Global::Authored(Qualifier::from(["thm"]));
    let holds_type = Term::induct_type(holds, Vec::<Term>::new(), Vec::<Term>::new());
    let named = |name: &Global| Term::free_var(&Free::from(name));

    let module = Module {
        items: vec![
            declared(holds, Nominal::Inductive(family(Term::prop(), &["qed"]))),
            authored(&statement, Term::prop(), holds_type.clone()),
            authored(
                &theorem,
                named(&statement),
                Term::variant(holds, Vec::<Term>::new(), "qed", Vec::<Term>::new()),
            ),
            authored(
                &statement,
                Term::prop(),
                Term::apply(
                    Term::func([(Free::local(900, Some("x")), holds_type)], false_type()),
                    [named(&theorem)],
                ),
            ),
        ]
        .into(),
        ..Module::default()
    };

    (module, named(&theorem))
}
