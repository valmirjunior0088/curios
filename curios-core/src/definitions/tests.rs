use {
    super::*,
    crate::{test_support::*, *},
    curios_common::RootKind,
};

fn definition(name: &str, universe_context: UniverseContext) -> Definition {
    let global = Global::Authored(Qualifier::from([name]));
    Definition {
        name: global,
        kind: DefinitionKind::Authored,
        universe_context,
        island: Qualifier::empty(),
        proof: false,
        value_sort: None,
        type_: Term::type_ground(),
        body: Term::instance(global, Universes::default()),
    }
}

/// `name`, a member of a group that names it by `member`, whose body is that one name.
fn member(name: &str, universe_context: UniverseContext, member: &Free) -> Definition {
    Definition {
        body: Term::free_var(member),
        ..definition(name, universe_context)
    }
}

/// A top-level group binds its members in its own scopes: a member's body names the local the group closed over, which the group's binder takes, and the group publishes the member under its declaration.
#[test]
fn a_top_level_rec_item_binds_its_members_in_the_shared_group() {
    let loop_ = fresh(Some("loop"));
    let rec = RecItem::new(vec![(
        loop_,
        member("loop", UniverseContext::empty(), &loop_),
    )]);

    assert!(
        rec.group
            .iter()
            .next()
            .unwrap()
            .body
            .body()
            .mentions()
            .is_empty()
    );
    assert_eq!(
        rec.group.member_body(0).as_rec_proj(),
        Some((&rec.group, 0)),
        "a member's body sees itself as a projection of its own group"
    );

    let opened = rec.definitions();
    assert_eq!(
        opened[0].body,
        Term::instance(
            Global::Authored(Qualifier::from(["loop"])),
            Universes::default()
        )
    );
}

#[test]
fn recursive_members_cannot_silently_discard_different_universe_contexts() {
    let polymorphic = UniverseContext {
        parameter_count: 1,
        quality_count: 0,
        constraints: Vec::new(),
    };
    let (left, right) = (fresh(Some("left")), fresh(Some("right")));

    assert_eq!(
        RecItem::try_new(vec![
            (left, member("left", UniverseContext::empty(), &left)),
            (right, member("right", polymorphic, &right)),
        ])
        .unwrap_err(),
        UniverseError::MismatchedRecursiveContexts,
    );
}

/// Every local the terms of the items handed in mention is found, with the name owning the position — and nothing of an item not handed in.
///
/// What the certifier refuses before it mints a binder of its own, since a binder aliasing a free local silently identifies two terms that differ. Leaving an item out is the control: a walk over a baseline hands in the items whose terms are not the baseline's, and the walk that filed the baseline refused a local of its own in the others.
#[test]
fn every_local_a_term_mentions_is_found() {
    let mentioned = Free::local(4_242, Some("y"));
    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_: Term::intrinsic(crate::Intrinsic::NatType),
        body: Term::free_var(&mentioned),
    };

    let definitions = Definitions {
        items: vec![Item::Let(Box::new(definition))].into(),
    };

    assert_eq!(
        free_locals_of(definitions.items.iter()),
        BTreeSet::from([(Some(Global::Authored(Qualifier::from(["held"]))), mentioned)])
    );
    assert_eq!(free_locals_of([]), BTreeSet::new());
}

/// A module carrying `body` in its one definition.
fn stored(body: Term) -> Definitions {
    Definitions {
        items: vec![Item::Let(Box::new(Definition {
            name: Global::Authored(Qualifier::from(["held"])),
            kind: DefinitionKind::Authored,
            universe_context: UniverseContext::empty(),
            island: Qualifier::default(),
            proof: false,
            value_sort: None,
            type_: Term::intrinsic(crate::Intrinsic::NatType),
            body,
        }))]
        .into(),
    }
}

/// `definitions` declaring `witness`, a definition introduced as a witness.
fn declaring_witness(mut definitions: Definitions, witness: Global) -> Definitions {
    definitions.items.push(Item::Let(Box::new(Definition {
        name: witness,
        kind: DefinitionKind::Witness,
        ..definition("witness", UniverseContext::empty())
    })));

    definitions
}

/// The mounts of a unit that claims `/mine`.
fn mine() -> Vec<Mount> {
    vec![Mount::new(Qualifier::from(["mine"]), RootKind::Ordinary)]
}

/// The refusals are the whole of the check's value: one that only ever meets conforming input asserts nothing about what it would do with the other kind.
///
/// Each of these is an identity minted by one compilation's counter. Stored, it is read by a compilation that mints from its own counter and would hand the same index out again — which aliases silently rather than crashing, and is why the seam that writes a unit refuses rather than reports.
#[test]
fn a_stored_unit_may_not_carry_a_free_local() {
    let definitions = stored(Term::free_var(&Free::local(7, Some("y"))));

    assert_eq!(
        validate_stored_identities(&definitions, &[]),
        Err(Positional::FreeLocal {
            owner: Some(Global::Authored(Qualifier::from(["held"]))),
            index: 7,
        })
    );
}

#[test]
fn a_stored_unit_may_not_carry_a_metavariable() {
    let definitions = stored(Term::hole(crate::MetavarId::from(3)));

    assert_eq!(
        validate_stored_identities(&definitions, &[]),
        Err(Positional::Metavar {
            owner: Some(Global::Authored(Qualifier::from(["held"]))),
        })
    );
}

/// A bound variable no scope in the unit binds is refused at the same seam, for a reason of its own.
///
/// A loose index is not an identity a counter handed out — it is an index with no binder, which `Mentions` answers nothing for, so the free-local clause above sees none of it. Stored, it is read at whatever depth the term it is spliced under happens to give it, which is a different term with no error anywhere; and handed to a walk, it is what the certifier refuses by `Error::NotClosed`.
#[test]
fn a_stored_unit_may_not_carry_a_loose_bound_variable() {
    let definitions = stored(Term::var(Var::bound(2)));

    assert_eq!(
        validate_stored_identities(&definitions, &[]),
        Err(Positional::LooseIndex {
            owner: Some(Global::Authored(Qualifier::from(["held"]))),
            reach: 3,
        })
    );
}

/// The control. A scope's body legitimately carries the indices its own binders bind, and every term in every stored unit holds some, so the clause reads a position's reach against the binders above it — which is none, a position being where a term starts.
#[test]
fn a_stored_unit_may_carry_a_scope_whose_body_names_its_binders() {
    let binder = Free::local(11, Some("x"));
    let definitions = stored(Term::func(
        [(binder, Term::intrinsic(crate::Intrinsic::NatType))],
        Term::free_var(&binder),
    ));

    assert_eq!(validate_stored_identities(&definitions, &[]), Ok(()));
}

/// A witness declared here, scoped to a mount this module does not own.
///
/// A witness's name holds the module that declares it, so a module declaring a witness under somebody else's mount is claiming a name in a space it does not own, which two units could both declare.
#[test]
fn a_stored_unit_may_not_declare_a_witness_under_a_mount_it_does_not_own() {
    let witness = Global::Witness(WitnessId::Unspelled {
        module: Qualifier::from(["theirs", "Shape"]),
        place: 0,
    });
    let definitions =
        declaring_witness(stored(Term::intrinsic(crate::Intrinsic::NatType)), witness);

    assert_eq!(
        validate_stored_identities(&definitions, &mine()),
        Err(Positional::UnscopedWitness { witness })
    );
}

/// A witness declared here under its place among its module's witnesses: the name a signature that spells no key is lowered under, which the same text written in another order gives to another declaration.
#[test]
fn a_stored_unit_may_not_declare_a_witness_under_its_place() {
    let witness = Global::Witness(WitnessId::Unspelled {
        module: Qualifier::from(["mine", "Shape"]),
        place: 0,
    });
    let definitions =
        declaring_witness(stored(Term::intrinsic(crate::Intrinsic::NatType)), witness);

    assert_eq!(
        validate_stored_identities(&definitions, &mine()),
        Err(Positional::PlacedWitness { witness })
    );

    let spelled = Global::Witness(WitnessId::Spelled {
        concept: Qualifier::from(["mine", "Show"]),
        key: WitnessKey::from([HeadKey::Nat]),
        module: Qualifier::from(["mine", "Shape"]),
        place: 0,
    });
    let definitions =
        declaring_witness(stored(Term::intrinsic(crate::Intrinsic::NatType)), spelled);

    assert_eq!(validate_stored_identities(&definitions, &mine()), Ok(()));
}

/// The control that keeps the refusal about *declaring*, not about mentioning.
///
/// A stored unit legitimately names witnesses its predecessors declared, scoped to their mounts — every unit compiled against `/std` does. Reading this question off the terms instead of off the declarations would refuse all of them, which is why it is asked over [`Definitions::witnesses`], the names the module declares as witnesses, and deliberately not through the position walk.
#[test]
fn a_stored_unit_may_name_a_witness_another_mount_declared() {
    let definitions = stored(Term::instance(
        Global::Witness(WitnessId::Unspelled {
            module: Qualifier::from(["theirs", "Shape"]),
            place: 3,
        }),
        Universes::default(),
    ));

    assert_eq!(validate_stored_identities(&definitions, &mine()), Ok(()));
}

/// The control. A free *global* is how one definition names another and is in every stored unit there has ever been, so a check that refused it would refuse the prelude — which is what makes this the test that the refusals above are aimed at something narrower than "a free variable".
#[test]
fn a_stored_unit_may_carry_a_global_it_names() {
    let definitions = stored(Term::instance(
        Global::Authored(Qualifier::from(["elsewhere"])),
        Universes::default(),
    ));

    assert_eq!(validate_stored_identities(&definitions, &[]), Ok(()));
}

/// A module with no registry and no entry, over `items`.
fn over(items: Vec<Item>) -> Definitions {
    Definitions {
        items: items.into(),
    }
}

fn authored(name: &str) -> Global {
    Global::Authored(Qualifier::from([name]))
}

/// A construction names its declaration in the registry rather than in the variable graph, so `mentions` misses the edge and `reaches` must not.
#[test]
fn a_construction_reaches_the_nominal_head_it_never_mentions() {
    let mut packed = definition("packed", UniverseContext::empty());
    packed.body = Term::struct_(
        authored("Box"),
        Vec::<Term>::new(),
        [Term::intrinsic(crate::Intrinsic::NatType)],
    );

    assert!(!packed.mentions().contains(&authored("Box")));
    assert!(packed.reaches().contains(&authored("Box")));
}

/// The item declaring `name` a record whose one field is a `Payload`.
fn boxed(name: &str) -> Item {
    let record = Nominal::Struct(StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::build(
            [(
                Free::local(0, Some("value")),
                Term::instance(authored("Payload"), Universes::default()),
            )],
            (),
        )),
        result_sort: Term::type_ground(),
        module: Qualifier::empty(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    });

    Item::Nominal(Nominals::new(vec![(authored(name), record)]))
}

/// A struct's field types live only in its registry entry, so the item declaring it reaches them through the entry.
#[test]
fn a_struct_declaration_reaches_its_field_types() {
    let definitions = over(vec![boxed("Box")]);

    let reached = definitions.reaches(&definitions.items[0]);

    assert!(reached.contains(&authored("Payload")));
}

/// A unit's nominal marks are read off the definitions it declares as type formers, which each place says of its names: of a stored module the formers are restored, and no other item is, each other item standing on bytes that hold none.
#[test]
fn nominal_marks_read_the_type_formers_and_no_other_item() {
    let former = boxed("Box");
    let items = [
        Item::Let(Box::new(definition("before", UniverseContext::empty()))),
        former.clone(),
        Item::Let(Box::new(definition("after", UniverseContext::empty()))),
    ];
    let unreadable = || Blob::of(b"no item".to_vec());
    let parts = vec![
        unreadable(),
        Blob::of(
            curios_archive::to_bytes(&former)
                .expect("an item archives")
                .to_vec(),
        ),
        unreadable(),
    ];

    let mut definitions = over(Vec::new());
    definitions.items = Items::parted(Outlines::of(&Items::from(items.to_vec())), parts)
        .expect("a part for each item");

    let marks = definitions.nominal_plicities(&definitions.located());

    assert!(
        marks.is_empty(),
        "a nullary former has no parameter to mark"
    );
}

/// `name`, whose body mentions each of `mentioned`.
fn reading(name: &str, mentioned: &[&str]) -> Item {
    let mut reading = definition(name, UniverseContext::empty());
    reading.body = Term::tuple(
        mentioned
            .iter()
            .map(|mentioned| Term::instance(authored(mentioned), Universes::default())),
    );

    Item::Let(Box::new(reading))
}

/// The first name of each item, in the order the module holds them.
fn names(definitions: &Definitions) -> Vec<String> {
    definitions.items.iter().map(Item::describe).collect()
}

/// An item follows what the registry entries it declares reach, which no term of its own names.
#[test]
fn an_item_follows_what_its_registry_entry_reaches() {
    let definitions = over(vec![boxed("Box"), reading("Payload", &[])]);

    assert_eq!(names(&definitions.ordered()), ["/Payload", "/Box"]);
}

/// A lexicon revised at the names a module changed answers as the module's own does: a name added shortens against the others and a name taken away no longer counts, a former made again as a plain definition writes no fields, and a former new to the module writes its own.
#[test]
fn a_lexicon_revised_at_what_changed_answers_as_the_whole_one() {
    let nested = |path: &[&str]| {
        let mut definition = definition("placeholder", UniverseContext::empty());
        definition.name = Global::Authored(Qualifier::from(path.to_vec()));
        Item::Let(Box::new(definition))
    };
    let then = over(vec![
        boxed("Box"),
        reading("Payload", &[]),
        nested(&["a", "shared"]),
        nested(&["gone", "shared"]),
    ]);
    let now = over(vec![
        Item::Let(Box::new(definition("Box", UniverseContext::empty()))),
        boxed("Crate"),
        reading("Payload", &[]),
        nested(&["a", "shared"]),
        nested(&["b", "other"]),
    ]);
    let changed = [
        authored("Box"),
        authored("Crate"),
        Global::Authored(Qualifier::from(["gone", "shared"])),
        Global::Authored(Qualifier::from(["b", "other"])),
    ]
    .into_iter()
    .collect::<BTreeSet<_>>();

    let revised = now.lexicon_over(&then.lexicon(), &Places::of(&now), &changed);
    let read = |lexicon: &Lexicon| {
        then.symbols()
            .into_iter()
            .chain(now.symbols())
            .map(|name| {
                let carried = suffixes_of(&name)
                    .iter()
                    .map(|suffix| lexicon.symbols.carrying(suffix))
                    .collect::<Vec<_>>();
                let fields = lexicon
                    .members
                    .fields
                    .get(&name)
                    .map(|fields| fields.to_vec());

                (lexicon.symbols.contains(&name), carried, fields)
            })
            .collect::<Vec<_>>()
    };
    assert_eq!(read(&revised), read(&now.lexicon()));
    assert_eq!(
        then.lexicon().symbols.carrying("shared"),
        2,
        "two names end in `shared`"
    );
    assert_eq!(
        revised.symbols.carrying("shared"),
        1,
        "the one ending in `shared` with it is gone"
    );
}
