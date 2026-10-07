use {
    curios_core::{
        Atom, Definition, DefinitionKind, Free, Global, InductDecl, InductParam, Item, Module,
        StructDecl, StructType, Subterm, Telescope, Term, Totality, UniverseContext,
    },
    curios_utilities::Qualifier,
    xboard::{Date, Part, Proof, Status, Ticket, Witness, false_name, false_type, refused},
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
            Witness {
                what: "Curry's paradox through a record that shares its name with an inductive",
                proof: Proof::Module(a_record_under_an_inductives_name),
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

/// One registry entry, the prelude's `False` holding a constructor `mk`, closed with `False/mk()`.
///
/// Declared over the environment a walk is seeded with, the entry stands where the prelude's did and is the one every rule reads. Checking it would not refuse it: an inhabited proposition is a well-formed declaration, and what is wrong with this one is its name.
fn a_registry_entry_under_the_preludes_false() -> (Module, Term) {
    let module = Module {
        induct_decls: [(false_name(), family(Term::prop(), &["mk"]))]
            .into_iter()
            .collect(),
        ..Module::default()
    };

    (
        module,
        Term::variant(false_name(), Vec::<Term>::new(), "mk", Vec::<Term>::new()),
    )
}

/// `p : Prop = ((x : Holds) => False)(thm)`, then `thm : p = Holds/qed()`, then `p : Prop = Holds`, closed with `thm`.
///
/// A walk orders items by what each names, and a name declared twice has two owners: `thm` waits for one `p` and is typed while `p` is `Holds`, and the other `p` names `thm`, so it is judged after it and defined where the first stood. The closing term is then read at a `p` that reduces to `False`.
fn a_statement_declared_twice() -> (Module, Term) {
    let holds = Global::Authored(Qualifier::from(["Holds"]));
    let statement = Global::Authored(Qualifier::from(["p"]));
    let theorem = Global::Authored(Qualifier::from(["thm"]));
    let holds_type = Term::induct_type(holds, Vec::<Term>::new(), Vec::<Term>::new());
    let named = |name: &Global| Term::free_var(&Free::from(name));

    let module = Module {
        items: vec![
            authored(
                &statement,
                Term::prop(),
                Term::apply(
                    Term::func(
                        [(Free::local(900, Some("x")), holds_type.clone())],
                        false_type(),
                    ),
                    [named(&theorem)],
                ),
            ),
            authored(
                &theorem,
                named(&statement),
                Term::variant(holds, Vec::<Term>::new(), "qed", Vec::<Term>::new()),
            ),
            authored(&statement, Term::prop(), holds_type),
        ],
        induct_decls: [(holds, family(Term::prop(), &["qed"]))]
            .into_iter()
            .collect(),
        ..Module::default()
    };

    (module, named(&theorem))
}

/// `struct Bad { f : (Bad) -> False }` beside an empty `induct Bad`, closed with `w(Bad { w })` for `w = (b) => b.0(b)`.
///
/// Strict positivity runs over the names of both registries and files each declaration under its name, asking the `induct` registry first: of two declarations under one name the family is analysed twice and the record never, so the negative occurrence in its field is the one the paradox is built from.
fn a_record_under_an_inductives_name() -> (Module, Term) {
    let bad = Global::Authored(Qualifier::from(["Bad"]));
    let bad_type: Term = Subterm::StructType(StructType {
        name: bad,
        universes: Vec::new(),
        params: Vec::new(),
    })
    .into();

    let record = StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::build(
            [(
                Free::local(901, Some("f")),
                Term::func_type(
                    [(Free::local(902, Some("devoured")), bad_type.clone())],
                    false_type(),
                ),
            )],
            (),
        )),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let module = Module {
        induct_decls: [(bad, family(Term::type_ground(), &[]))]
            .into_iter()
            .collect(),
        struct_decls: [(bad, record)].into_iter().collect(),
        ..Module::default()
    };

    // (b) => b.0(b)
    let subject = Free::local(903, Some("b"));
    let open = Term::func(
        [(subject, bad_type)],
        Term::apply(
            Term::proj(Term::free_var(&subject), 0),
            [Term::free_var(&subject)],
        ),
    );

    (
        module,
        Term::apply(
            open.clone(),
            [Term::struct_(bad, Vec::<Term>::new(), [open])],
        ),
    )
}
