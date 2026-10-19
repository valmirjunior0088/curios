use {
    super::*,
    crate::{
        HeadKey, Level, NumLit, Outlines, Subterm, Transient, UniverseMetaId, WitnessId, WitnessKey,
    },
    curios_utilities::{RootKind, Sign},
};

fn definition(name: &str, universe_context: UniverseContext) -> Definition {
    let global = Global::Authored(Qualifier::from([name]));
    Definition {
        name: global,
        kind: DefinitionKind::Authored,
        universe_context,
        island: Qualifier::empty(),
        totality: Totality::default(),
        proof: false,
        variances: Vec::new(),
        field_variances: Vec::new(),
        type_: Term::type_ground(),
        body: Term::free_var(&Free::from(&global)),
    }
}

#[test]
fn top_level_rec_item_captures_its_exports_into_the_shared_group() {
    let rec = RecItem::new(vec![definition("loop", UniverseContext::empty())]);

    assert!(
        rec.group
            .iter()
            .next()
            .unwrap()
            .body
            .body()
            .free_vars()
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
        Term::free_var(&Free::global(Qualifier::from(["loop"])))
    );
}

#[test]
fn recursive_members_cannot_silently_discard_different_universe_contexts() {
    let polymorphic = UniverseContext {
        parameter_count: 1,
        quality_count: 0,
        constraints: Vec::new(),
    };

    assert_eq!(
        RecItem::try_new(vec![
            definition("left", UniverseContext::empty()),
            definition("right", polymorphic),
        ])
        .unwrap_err(),
        UniverseError::MismatchedRecursiveContexts,
    );
}

/// Every local the terms of the items handed in mention is found, with the name owning the position — and nothing of an item not handed in.
///
/// What the kernel refuses before it mints a binder of its own, since a binder aliasing a free local silently identifies two terms that differ. Leaving an item out is the control: a walk over a baseline hands in the items whose terms are not the baseline's, and the walk that filed the baseline refused a local of its own in the others.
#[test]
fn every_local_a_term_mentions_is_found() {
    let mentioned = Free::local(4_242, Some("y"));
    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::default(),
        totality: Totality::Total,
        proof: false,
        variances: Vec::new(),
        field_variances: Vec::new(),
        type_: Term::intrinsic(crate::Intrinsic::NatType),
        body: Term::free_var(&mentioned),
    };

    let module = Module {
        items: vec![Item::Let(definition)].into(),
        mounts: Vec::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    };

    assert_eq!(
        free_locals_of(module.items.iter()),
        BTreeSet::from([(Some(Global::Authored(Qualifier::from(["held"]))), mentioned)])
    );
    assert_eq!(free_locals_of([]), BTreeSet::new());
}

/// A module carrying `body` in its one definition.
fn stored(body: Term) -> Module {
    Module {
        items: vec![Item::Let(Definition {
            name: Global::Authored(Qualifier::from(["held"])),
            kind: DefinitionKind::Authored,
            universe_context: UniverseContext::empty(),
            island: Qualifier::default(),
            totality: Totality::Total,
            proof: false,
            variances: Vec::new(),
            field_variances: Vec::new(),
            type_: Term::intrinsic(crate::Intrinsic::NatType),
            body,
        })]
        .into(),
        mounts: Vec::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    }
}

/// The refusals are the whole of the check's value: one that only ever meets conforming input asserts nothing about what it would do with the other kind.
///
/// Each of these is an identity minted by one compilation's counter. Stored, it is read by a compilation that mints from its own counter and would hand the same index out again — which aliases silently rather than crashing, and is why the seam that writes a unit refuses rather than reports.
#[test]
fn a_stored_unit_may_not_carry_a_free_local() {
    let module = stored(Term::free_var(&Free::local(7, Some("y"))));

    assert_eq!(
        validate_stored_identities(&module),
        Err(Positional::FreeLocal {
            owner: Some(Global::Authored(Qualifier::from(["held"]))),
            index: 7,
        })
    );
}

#[test]
fn a_stored_unit_may_not_carry_a_metavariable() {
    let module = stored(Term::hole(crate::MetavarId::from(3)));

    assert_eq!(
        validate_stored_identities(&module),
        Err(Positional::Metavar {
            owner: Some(Global::Authored(Qualifier::from(["held"]))),
        })
    );
}

/// A witness declared here, scoped to a mount this module does not own.
///
/// A witness's name holds the module that declares it, so a module declaring a witness under somebody else's mount is claiming a name in a space it does not own, which two units could both declare.
#[test]
fn a_stored_unit_may_not_declare_a_witness_under_a_mount_it_does_not_own() {
    let mut module = stored(Term::intrinsic(crate::Intrinsic::NatType));
    module.mounts = vec![Mount::new(Qualifier::from(["mine"]), RootKind::Ordinary)];
    let witness = Global::Witness(WitnessId::Unspelled {
        module: Qualifier::from(["theirs", "Shape"]),
        place: 0,
    });
    module.witnesses.insert(witness);

    assert_eq!(
        validate_stored_identities(&module),
        Err(Positional::UnscopedWitness { witness })
    );
}

/// A witness declared here under its place among its module's witnesses: the name a signature that spells no key is lowered under, which the same text written in another order gives to another declaration.
#[test]
fn a_stored_unit_may_not_declare_a_witness_under_its_place() {
    let mut module = stored(Term::intrinsic(crate::Intrinsic::NatType));
    module.mounts = vec![Mount::new(Qualifier::from(["mine"]), RootKind::Ordinary)];
    let witness = Global::Witness(WitnessId::Unspelled {
        module: Qualifier::from(["mine", "Shape"]),
        place: 0,
    });
    module.witnesses.insert(witness);

    assert_eq!(
        validate_stored_identities(&module),
        Err(Positional::PlacedWitness { witness })
    );

    let spelled = Global::Witness(WitnessId::Spelled {
        concept: Qualifier::from(["mine", "Show"]),
        key: WitnessKey::from([HeadKey::Nat]),
        module: Qualifier::from(["mine", "Shape"]),
        place: 0,
    });
    module.witnesses = BTreeSet::from([spelled]);

    assert_eq!(validate_stored_identities(&module), Ok(()));
}

/// The control that keeps the refusal about *declaring*, not about mentioning.
///
/// A stored unit legitimately names witnesses its predecessors declared, scoped to their mounts — every unit compiled against `/std` does. Reading this question off the terms instead of off the declarations would refuse all of them, which is why it is asked over `Module::witnesses` and deliberately not through the position walk.
#[test]
fn a_stored_unit_may_name_a_witness_another_mount_declared() {
    let mut module = stored(Term::free_var(&Free::Global(Global::Witness(
        WitnessId::Unspelled {
            module: Qualifier::from(["theirs", "Shape"]),
            place: 3,
        },
    ))));
    module.mounts = vec![Mount::new(Qualifier::from(["mine"]), RootKind::Ordinary)];

    assert_eq!(validate_stored_identities(&module), Ok(()));
}

/// The control. A free *global* is how one definition names another and is in every stored unit there has ever been, so a check that refused it would refuse the prelude — which is what makes this the test that the refusals above are aimed at something narrower than "a free variable".
#[test]
fn a_stored_unit_may_carry_a_global_it_names() {
    let module = stored(Term::free_var(&Free::global(Qualifier::from([
        "elsewhere",
    ]))));

    assert_eq!(validate_stored_identities(&module), Ok(()));
}

#[test]
fn a_meta_free_module_projects_as_zonked() {
    let module = Module {
        items: vec![Item::Let(definition("plain", UniverseContext::empty()))].into(),
        mounts: Vec::new(),
        witnesses: Default::default(),
        tests: Default::default(),
    };

    assert!(Zonked::project(&module).is_ok());
}

#[test]
fn a_surviving_metavariable_refuses_the_zonked_projection() {
    let mut holed = definition("holed", UniverseContext::empty());
    holed.body = Term::hole(0);
    let module = Module {
        items: vec![Item::Let(holed)].into(),
        mounts: Vec::new(),
        witnesses: Default::default(),
        tests: Default::default(),
    };

    let refusal = Zonked::project(&module).expect_err("the hole must refuse the projection");
    assert!(refusal.to_string().contains("holed"), "{refusal}");
}

#[test]
fn a_surviving_transient_refuses_the_zonked_projection() {
    let mut infixed = definition("infixed", UniverseContext::empty());
    infixed.body = Term::from(Subterm::Transient(Transient::NumLit(NumLit::Number {
        magnitude: 7u32.into(),
        sign: Sign::Unmarked,
    })));
    let module = Module {
        items: vec![Item::Let(infixed)].into(),
        mounts: Vec::new(),
        witnesses: Default::default(),
        tests: Default::default(),
    };

    let refusal = Zonked::project(&module).expect_err("the transient must refuse the projection");
    assert!(refusal.to_string().contains("infixed"), "{refusal}");
}

/// A level that holds a universe metavariable refuses the projection, in the two places `curios-cert`'s `a_level_holding_an_unsolved_universe_metavariable_is_refused` puts one: a definition's declared type and a registry entry's result sort. The level stands in a term's node and in no metavariable a term holds, so the evidence reads a flag of its own for it; the same two places at a ground level project.
#[test]
fn a_surviving_universe_metavariable_refuses_the_zonked_projection() {
    let at = |level: Level| {
        let mut typed = definition("typed", UniverseContext::empty());
        typed.type_ = Term::type_at(level.clone());

        let mut family = definition("Levelled", UniverseContext::empty());
        family.kind = DefinitionKind::Nominal(Rc::new(Nominal::Inductive(InductDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::done(())),
            constructors: Vec::new().into(),
            result_sort: Term::type_at(level),
            module: Qualifier::default(),
            rep_public: true,
            polarities: Vec::new(),
            variances: Vec::new(),
        })));

        [("typed", typed), ("Levelled", family)]
    };

    for (name, item) in at(Level::meta(UniverseMetaId::from(0usize))) {
        let refusal = Zonked::project(&over(vec![Item::Let(item)]))
            .expect_err("the level must refuse the projection");
        assert!(refusal.to_string().contains(name), "{refusal}");
    }
    for (name, item) in at(Level::zero()) {
        assert!(
            Zonked::project(&over(vec![Item::Let(item)])).is_ok(),
            "{name}"
        );
    }
}

/// A recompile's module is held to being finished by the items it made, every other being a finished baseline's: a hole in an item made refuses it, and a module whose items made are finished projects.
#[test]
fn a_revised_module_is_asked_of_its_items_made() {
    let plain = Item::Let(definition("plain", UniverseContext::empty()));
    let mut holed = definition("holed", UniverseContext::empty());
    holed.body = Term::hole(0);
    let holed = Item::Let(holed);

    let refusal = Zonked::revised(&over(vec![plain.clone(), holed.clone()]), [&holed])
        .expect_err("the hole must refuse the projection");
    assert!(refusal.to_string().contains("holed"), "{refusal}");
    assert!(Zonked::revised(&over(vec![plain.clone()]), [&plain]).is_ok());
}

/// A module with no registry and no entry, over `items`.
fn over(items: Vec<Item>) -> Module {
    Module {
        items: items.into(),
        mounts: Vec::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
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

/// `former` as the former of a record whose one field is a `Payload`: the entry rides in the definition that declares it.
fn boxed(mut former: Definition) -> Definition {
    former.kind = DefinitionKind::Nominal(Rc::new(Nominal::Struct(StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::build(
            [(
                Free::local(0, Some("value")),
                Term::free_var(&Free::Global(authored("Payload"))),
            )],
            (),
        )),
        result_sort: Term::type_ground(),
        module: Qualifier::empty(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    })));
    former
}

/// A struct's field types live only in its registry entry, so the item declaring it reaches them through the entry.
#[test]
fn a_struct_declaration_reaches_its_field_types() {
    let module = over(vec![Item::Let(boxed(definition(
        "Box",
        UniverseContext::empty(),
    )))]);

    let reached = module.reaches(&module.items[0]);

    assert!(reached.contains(&authored("Payload")));
}

/// A unit's nominal marks are read off the definitions it declares as type formers, which each place says of its names: of a stored module the formers are restored, and no other item is, each other item standing on bytes that hold none.
#[test]
fn nominal_marks_read_the_type_formers_and_no_other_item() {
    let former = Item::Let(boxed(definition("Box", UniverseContext::empty())));
    let items = [
        Item::Let(definition("before", UniverseContext::empty())),
        former.clone(),
        Item::Let(definition("after", UniverseContext::empty())),
    ];
    let unreadable = || Object::of(b"no item".to_vec());
    let parts = vec![
        unreadable(),
        Object::of(
            curios_archive::to_bytes(&former)
                .expect("an item archives")
                .to_vec(),
        ),
        unreadable(),
    ];

    let mut module = over(Vec::new());
    module.items = Items::parted(Outlines::of(&Items::from(items.to_vec())), parts)
        .expect("a part for each item");

    let marks = module.nominal_plicities(&module.located());

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
            .map(|mentioned| Term::free_var(&Free::Global(authored(mentioned)))),
    );

    Item::Let(reading)
}

/// The first name of each item, in the order the module holds them.
fn names(module: &Module) -> Vec<String> {
    module.items.iter().map(Item::describe).collect()
}

/// An item follows what the registry entries it declares reach, which no term of its own names.
#[test]
fn an_item_follows_what_its_registry_entry_reaches() {
    let Item::Let(former) = reading("Box", &[]) else {
        unreachable!("a reading is one definition");
    };
    let module = over(vec![Item::Let(boxed(former)), reading("Payload", &[])]);

    assert_eq!(names(&module.ordered()), ["/Payload", "/Box"]);
}

/// A lexicon revised at the names a module changed answers as the module's own does: a name added shortens against the others and a name taken away no longer counts, a former made again as a plain definition writes no fields, and a former new to the module writes its own.
#[test]
fn a_lexicon_revised_at_what_changed_answers_as_the_whole_one() {
    let nested = |path: &[&str]| {
        let mut definition = definition("placeholder", UniverseContext::empty());
        definition.name = Global::Authored(Qualifier::from(path.to_vec()));
        Item::Let(definition)
    };
    let then = over(vec![
        Item::Let(boxed(definition("Box", UniverseContext::empty()))),
        reading("Payload", &[]),
        nested(&["a", "shared"]),
        nested(&["gone", "shared"]),
    ]);
    let now = over(vec![
        Item::Let(definition("Box", UniverseContext::empty())),
        Item::Let(boxed(definition("Crate", UniverseContext::empty()))),
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
        let spelling = Spelling::default().with_lexicons(lexicon, &[]);
        then.module_symbols()
            .into_iter()
            .chain(now.module_symbols())
            .map(|name| {
                (
                    spelling.symbol(&name),
                    spelling.structure_fields(&name).map(<[_]>::to_vec),
                )
            })
            .collect::<Vec<_>>()
    };
    assert_eq!(read(&revised), read(&now.lexicon()));
    let shared = Global::Authored(Qualifier::from(["a", "shared"]));
    let spelled = |lexicon: &Lexicon| {
        Spelling::default()
            .with_lexicons(lexicon, &[])
            .symbol(&shared)
    };
    assert_eq!(
        spelled(&then.lexicon()),
        "a/shared",
        "two names end in `shared`"
    );
    assert_eq!(
        spelled(&revised),
        "shared",
        "the one ending in `shared` with it is gone"
    );
}
