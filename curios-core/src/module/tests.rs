use {super::*, crate::WitnessId, curios_utilities::RootKind};

fn definition(name: &str, universe_context: UniverseContext) -> Definition {
    let global = Global::Authored(Qualifier::from([name]));
    Definition {
        name: global.clone(),
        kind: DefinitionKind::Authored,
        universe_context,
        island: Qualifier::empty(),
        totality: Totality::default(),
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

/// The floor must clear every local a module's terms mention, whatever `Module::binder_floor` claims.
///
/// A binder the kernel mints while comparing under a telescope or eta-contracting aliases a free local the moment the floor is too low, and two terms that differ stop being distinguishable. The carried number is the elaborator's word and nothing checks it, so the walk derives its own and the caller takes the larger.
#[test]
fn the_floor_clears_every_local_a_term_mentions() {
    let mentioned = Free::local(4_242, Some("y"));
    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::default(),
        totality: Totality::Total,
        type_: Term::intrinsic(crate::Intrinsic::NatType),
        body: Term::free_var(&mentioned),
    };

    let module = Module {
        items: vec![Item::Let(definition)],
        mounts: Vec::new(),
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        // The understated claim the walk must not believe.
        binder_floor: 0,
        entry: Some(Entrypoint {
            body: Term::intrinsic(crate::Intrinsic::NatType),
            type_: None,
        }),
    };

    assert_eq!(derived_binder_floor(&module), 4_243);
}

/// A module carrying `body` in its one definition, and `entrypoint` as the program's own body.
fn stored(body: Term, entrypoint: Term) -> Module {
    Module {
        items: vec![Item::Let(Definition {
            name: Global::Authored(Qualifier::from(["held"])),
            kind: DefinitionKind::Authored,
            universe_context: UniverseContext::empty(),
            island: Qualifier::default(),
            totality: Totality::Total,
            type_: Term::intrinsic(crate::Intrinsic::NatType),
            body,
        })],
        mounts: Vec::new(),
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 0,
        entry: Some(Entrypoint {
            body: entrypoint,
            type_: None,
        }),
    }
}

/// The refusals are the whole of the check's value: one that only ever meets conforming input asserts nothing about what it would do with the other kind.
///
/// Each of these is an identity minted by one compilation's counter. Stored, it is read by a compilation that mints from its own counter and would hand the same index out again — which aliases silently rather than crashing, and is why the seam that writes a unit refuses rather than reports.
#[test]
fn a_stored_unit_may_not_carry_a_free_local() {
    let module = stored(
        Term::free_var(&Free::local(7, Some("y"))),
        Term::intrinsic(crate::Intrinsic::NatType),
    );

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
    let module = stored(
        Term::hole(crate::MetavarId::from(3)),
        Term::intrinsic(crate::Intrinsic::NatType),
    );

    assert_eq!(
        validate_stored_identities(&module),
        Err(Positional::Metavar {
            owner: Some(Global::Authored(Qualifier::from(["held"]))),
        })
    );
}

/// Every position the floor walk covers is a position this one covers, because both read the same list. The entrypoint is the position most easily left out of a hand-written list: it belongs to no declared name, so it is the one a walk over `items` misses.
#[test]
fn a_stored_unit_may_not_carry_one_in_its_entrypoint() {
    let module = stored(
        Term::intrinsic(crate::Intrinsic::NatType),
        Term::free_var(&Free::local(1, None)),
    );

    assert_eq!(
        validate_stored_identities(&module),
        Err(Positional::FreeLocal {
            owner: None,
            index: 1,
        })
    );
}

/// A witness declared here, scoped to a mount this module does not own.
///
/// Before B1 there was nothing to check: an identity was a bare ordinal, so "unscoped" named no state a module could be in. What makes it checkable is that the ordinal now counts *within* a mount — and a module declaring a witness under somebody else's mount is claiming an ordinal in a space it does not own, which two compilations would both hand out.
#[test]
fn a_stored_unit_may_not_declare_a_witness_under_a_mount_it_does_not_own() {
    let mut module = stored(
        Term::intrinsic(crate::Intrinsic::NatType),
        Term::intrinsic(crate::Intrinsic::NatType),
    );
    module.mounts = vec![Mount::new(Qualifier::from(["mine"]), RootKind::Ordinary)];
    let witness = Global::Witness(WitnessId::new(Qualifier::from(["theirs"]), 0));
    module.witnesses.insert(witness.clone());

    assert_eq!(
        validate_stored_identities(&module),
        Err(Positional::UnscopedWitness { witness })
    );
}

/// The control that keeps the refusal about *declaring*, not about mentioning.
///
/// A stored unit legitimately names witnesses its predecessors declared, scoped to their mounts — every unit compiled against `/std` does. Reading this question off the terms instead of off the declarations would refuse all of them, which is why it is asked over `Module::witnesses` and deliberately not through the position walk.
#[test]
fn a_stored_unit_may_name_a_witness_another_mount_declared() {
    let mut module = stored(
        Term::free_var(&Free::Global(Global::Witness(WitnessId::new(
            Qualifier::from(["theirs"]),
            3,
        )))),
        Term::intrinsic(crate::Intrinsic::NatType),
    );
    module.mounts = vec![Mount::new(Qualifier::from(["mine"]), RootKind::Ordinary)];

    assert_eq!(validate_stored_identities(&module), Ok(()));
}

/// The control. A free *global* is how one definition names another and is in every stored unit there has ever been, so a check that refused it would refuse the prelude — which is what makes this the test that the refusals above are aimed at something narrower than "a free variable".
#[test]
fn a_stored_unit_may_carry_a_global_it_names() {
    let module = stored(
        Term::free_var(&Free::global(Qualifier::from(["elsewhere"]))),
        Term::free_var(&Free::global(Qualifier::from(["elsewhere"]))),
    );

    assert_eq!(validate_stored_identities(&module), Ok(()));
}

#[test]
fn a_meta_free_module_projects_as_zonked() {
    let module = Module {
        items: vec![Item::Let(definition("plain", UniverseContext::empty()))],
        mounts: Vec::new(),
        universe_seeds: Vec::new(),
        induct_decls: Default::default(),
        struct_decls: Default::default(),
        concepts: Default::default(),
        witnesses: Default::default(),
        binder_floor: 0,
        entry: None,
    };

    assert!(Zonked::project(&module).is_ok());
}

#[test]
fn a_surviving_metavariable_refuses_the_zonked_projection() {
    let mut holed = definition("holed", UniverseContext::empty());
    holed.body = Term::hole(0);
    let module = Module {
        items: vec![Item::Let(holed)],
        mounts: Vec::new(),
        universe_seeds: Vec::new(),
        induct_decls: Default::default(),
        struct_decls: Default::default(),
        concepts: Default::default(),
        witnesses: Default::default(),
        binder_floor: 0,
        entry: None,
    };

    let refusal = Zonked::project(&module).expect_err("the hole must refuse the projection");
    assert!(refusal.to_string().contains("holed"), "{refusal}");
}
