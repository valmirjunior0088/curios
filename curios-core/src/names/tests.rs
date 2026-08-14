use {
    super::{Free, Global, Mint, WitnessId},
    curios_utilities::Qualifier,
    std::collections::HashSet,
};

fn authored(segments: [&str; 2]) -> Global {
    Global::Authored(Qualifier::from(segments))
}

// The case the flat `Global { qualifier, disambiguator: Option<u32> }` shape could not keep apart without reading the optional field: a witness declared *in* `/std/Nat` and the definition *named* `/std/Nat`. Under that shape both carry the same qualifier and are told apart only by whether the other field is populated — one field with two readings. The sum makes them different values outright.
#[test]
fn a_witness_is_not_the_definition_named_after_its_module() {
    let module_definition = authored(["std", "Nat"]);
    let witness_declared_there = Global::Witness(WitnessId::new(Qualifier::from(["std"]), 0));

    assert_ne!(module_definition, witness_declared_there);
}

// Two witnesses in one module are distinct without either carrying a manufactured name to tell them apart. `/std/Nat` declares nine `satisfy` blocks, which is the situation that forced the `witness@N` spelling.
#[test]
fn witnesses_sharing_a_module_stay_distinct() {
    let mount = Qualifier::from(["std"]);
    let witnesses = (0..9).map(|n| Free::Global(Global::Witness(WitnessId::new(mount.clone(), n))));

    assert_eq!(witnesses.collect::<HashSet<_>>().len(), 9);
}

// The collision B1 exists to remove: two units elaborated in separate compilations both mint from zero, so the ordinal alone cannot tell their witnesses apart. The mount is what does.
#[test]
fn one_ordinal_under_two_mounts_is_two_witnesses() {
    let here = Global::Witness(WitnessId::new(Qualifier::from(["std"]), 0));
    let there = Global::Witness(WitnessId::new(Qualifier::from(["json"]), 0));

    assert_ne!(here, there);
    assert_eq!(here.to_string(), "/std/witness@0");
    assert_eq!(there.to_string(), "/json/witness@0");
}

// A global and a local are different kinds of thing, not two spellings — the distinction `has_local_free` used to draw by testing for a marker character, and which a marker collision has already broken once.
#[test]
fn a_global_never_equals_a_local() {
    assert_ne!(
        Free::Global(authored(["std", "Nat"])),
        Free::local(0, Some("Nat")),
    );
    assert!(Free::local(0, None).is_local());
    assert!(!Free::Global(authored(["std", "Nat"])).is_local());
}

// Identity is the qualifier's segments, not its rendered spelling: `/Foo/bar` and a single segment that happens to contain a slash are different names. Nothing may collapse them by comparing joined text.
#[test]
fn segment_structure_decides_identity_not_rendered_text() {
    let two_segments = Global::Authored(Qualifier::from(["Foo", "bar"]));
    let one_segment = Global::Authored(Qualifier::from(["Foo/bar"]));

    assert_ne!(two_segments, one_segment);
    assert_eq!(two_segments.to_string(), one_segment.to_string());
}

// Distinct mints are distinct identities regardless of what a binder chose to call itself: the hint is display metadata, so it cannot make two identities collide or one identity split.
#[test]
fn mints_are_identities_independent_of_any_hint() {
    let mints = (0..64).map(|n| Free::local(n, Some("x")));

    assert_eq!(mints.collect::<HashSet<_>>().len(), 64);
}

// The half of that law the collection test cannot see: one identity under two hints is one binder, everywhere identity is consulted.
#[test]
fn a_hint_never_splits_one_identity() {
    let written = Mint::new(7, Some("xs"));
    let rebuilt = Mint::new(7, None);

    assert_eq!(written, rebuilt);
    assert_eq!(
        HashSet::from([Free::Local(written.clone()), Free::Local(rebuilt)]).len(),
        1
    );
    assert_eq!(written.hint(), Some("xs"));
    assert_eq!(written.with_hint(Some("ys")).hint(), Some("ys"));
}
