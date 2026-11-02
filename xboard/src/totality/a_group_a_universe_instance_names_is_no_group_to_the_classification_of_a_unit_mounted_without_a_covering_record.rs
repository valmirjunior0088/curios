use {
    crate::{Date, Mounting, Proof, Status, Ticket, Witness, declared, false_type, refused},
    curios_core::{
        Certification, Definition, DefinitionKind, Free, Global, InstanceHead, Intrinsic, Item,
        Level, Many, Module, Nat, Nominal, RecGroup, RecMemberScopes, Scope, StructDecl,
        StructType, Subterm, Telescope, Term, Totality, UniverseContext, Universes,
    },
    curios_utilities::Qualifier,
};

pub(super) const TICKET: Ticket = Ticket {
    title: "A group a universe instance names is no group to the classification of a unit mounted without a covering record",
    found_on: Date::new(2026, 11, 2),
    status: Status::Open,
    witnesses: &[Witness {
        what: "`False` projected out of a partial definition a unit in scope reads as total",
        proof: Proof::Mounted(a_group_a_universe_instance_names),
        expect: refused![kernel: curios_cert::Error::NotTotal { .. }],
    }],
};

/// `struct Box : Type { v : Nat, pf : False }` and `let bad : Box = Box { v = 0, pf = bad.pf }` in a unit mounted with an empty record, closed with `bad.pf`.
///
/// Obligation (V) refuses a proof reaching a definition not known to terminate, and what it asks is `Totals::partial`. For a unit whose record does not cover it — an empty one included — the classification is `Totals::seeded`'s, from the unit's own terms, which `curios-cert`'s own documentation says answers as an item the walk judges does. It does not: it reads a group off a term by matching the `Rec` node alone, where `curios_core::Term::has_group` states the relation as a `Rec` node *or the member a universe instance is taken of*, and `bad`'s self-reference is the second spelling — a member of a group carrying one universe parameter, projected at a stated level. So `bad` reads total, (V) goes inert over everything that reaches it, and `bad.pf` is admitted at `False`.
///
/// The forgery is the record and nothing else. The library is a unit a walk accepts and itself classifies partial: `bad` does not descend, `Box` is relevant and yields no sort, so no member of the group is one erasure deletes and `check_group`'s local gate does not fire. Judged with its own record in scope the same projection is refused.
fn a_group_a_universe_instance_names() -> (Mounting, Module, Term) {
    let boxed = Global::Authored(Qualifier::from(["Box"]));
    let bad = Global::Authored(Qualifier::from(["bad"]));

    let box_type: Term = Subterm::StructType(StructType {
        name: boxed,
        universes: Universes::default(),
        params: Vec::new(),
    })
    .into();

    // struct Box : Type { v : Nat, pf : False } — the `Nat` field is what keeps the carrier relevant, so that the group's member is no value erasure deletes.
    let boxed_decl = StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::build(
            [
                (
                    Free::local(920, Some("v")),
                    Term::intrinsic(Intrinsic::NatType),
                ),
                (Free::local(921, Some("pf")), false_type()),
            ],
            (),
        )),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    // rec bad : Box = Box { v = 0, pf = bad.pf }, carrying one universe parameter so that naming its member states an instance.
    let member = Free::local(922, Some("bad"));
    let group = RecGroup::new(vec![RecMemberScopes {
        type_: Scope::close(Many(1), &[&member], box_type.clone()),
        body: Scope::close(
            Many(1),
            &[&member],
            Term::struct_(
                boxed,
                Vec::<Term>::new(),
                [
                    Term::intrinsic(Intrinsic::Nat(Nat::new(0u32))),
                    Term::proj_struct(Term::free_var(&member), boxed, 1),
                ],
            ),
        ),
    }])
    .with_universe_context(UniverseContext {
        parameter_count: 1,
        quality_count: 0,
        constraints: Vec::new(),
    });

    let definition = Item::Let(Box::new(Definition {
        name: bad,
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::default(),
        // The honest stamp: the definition does not descend. Nothing here reads it — an environment reads a record and a proof mark — and a walk over this unit would recompute it.
        totality: Totality::Partial,
        proof: false,
        value_sort: None,
        variances: Vec::new(),
        field_variances: Vec::new(),
        parameter_field: None,
        type_: box_type,
        body: Term::instance(InstanceHead::RecProj(group, 0), vec![Level::zero()]),
    }));

    let library = Module {
        items: vec![declared(boxed, Nominal::Struct(boxed_decl)), definition].into(),
        ..Module::default()
    };

    (
        Mounting {
            module: library,
            certification: Certification::default(),
        },
        Module::default(),
        Term::proj_struct(Term::free_var(&Free::from(&bad)), boxed, 1),
    )
}
