use {
    curios_core::{
        Free, Global, Level, Module, StructDecl, StructType, Subterm, Telescope, Term,
        UniverseContext,
    },
    curios_utilities::Qualifier,
    xboard::{Date, Part, Proof, Status, Ticket, Witness, false_type, refused},
};

/// Is this a type, and at which sort? For every former — Π, Σ, a declared family or record — with its levels, subsumption, `Prop` and positivity, and for what a declaration, an intrinsic or a `foreign` row may state.
pub(super) const FORMATION: Part = Part {
    name: "formation",
    tickets: &[Ticket {
        title: "A parameter named through an instance is an occurrence positivity does not see",
        found_on: Date::new(2026, 10, 3),
        status: Status::Fixed,
        witnesses: &[Witness {
            what: "Curry's paradox through a record whose negative use of its parameter is an instance of it",
            proof: Proof::Module(a_parameter_named_through_an_instance),
            expect: refused![kernel: curios_cert::Error::NotPositive { .. }],
        }],
    }],
};

/// `struct Sink(A) { f : (A) -> False }` and `struct Bad { x : Sink(Bad) }`, with `Sink`'s one use of `A` spelled as an instance of it rather than as a bare occurrence, closed with `w(Bad { Sink { w } })` for `w = (b) => b.0.0(b)`.
///
/// The polarity walk records a parameter occurrence at its `Var` rule, and reads every shape it cannot see through by descending into that shape's child terms. An instance's variable head is the node's own data rather than a child, so the descent finds nothing and the parameter keeps the `Unused` the fixpoint seeds it at — whereupon `Sink(Bad)` carries `Bad` at `Unused`, `Bad` reaches itself through nothing, and the negative occurrence the instance stood at is the one the paradox is built from. Instantiating the declaration dissolves the instance, so the field `Bad` is built with is the honest `(Bad) -> False`.
fn a_parameter_named_through_an_instance() -> (Module, Term) {
    let sink = Global::Authored(Qualifier::from(["Sink"]));
    let bad = Global::Authored(Qualifier::from(["Bad"]));

    let bad_type: Term = Subterm::StructType(StructType {
        name: bad,
        universes: Vec::new(),
        params: Vec::new(),
    })
    .into();
    let sink_at_bad: Term = Subterm::StructType(StructType {
        name: sink,
        universes: Vec::new(),
        params: vec![bad_type.clone()],
    })
    .into();

    let parameter = Free::local(900, Some("A"));
    let devoured = Free::local(901, Some("devoured"));
    let sink_decl = StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::build(
            [(parameter, Term::type_ground())],
            Telescope::build(
                [(
                    Free::local(902, Some("f")),
                    Term::func_type(
                        [(devoured, Term::instance_of(&parameter, vec![Level::zero()]))],
                        false_type(),
                    ),
                )],
                (),
            ),
        ),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let bad_decl = StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::build(
            [(Free::local(903, Some("x")), sink_at_bad)],
            (),
        )),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let module = Module {
        struct_decls: [(sink, sink_decl), (bad, bad_decl)].into_iter().collect(),
        ..Module::default()
    };

    let eater = Free::local(904, Some("w"));
    let subject = Free::local(905, Some("b"));

    // (w) => w(Bad { x = Sink(Bad) { f = w } })
    let devour = Term::func(
        [(
            eater,
            Term::func_type([(subject, bad_type.clone())], false_type()),
        )],
        Term::apply(
            Term::free_var(&eater),
            [Term::struct_(
                bad,
                Vec::<Term>::new(),
                [Term::struct_(
                    sink,
                    [bad_type.clone()],
                    [Term::free_var(&eater)],
                )],
            )],
        ),
    );

    // (b) => b.0.0(b)
    let open = Term::func(
        [(subject, bad_type)],
        Term::apply(
            Term::proj(Term::proj(Term::free_var(&subject), 0), 0),
            [Term::free_var(&subject)],
        ),
    );

    (module, Term::apply(devour, [open]))
}
