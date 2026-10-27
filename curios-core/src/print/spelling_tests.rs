//! The names a print spells: a binder suffixed beside a like-named global or label, a reader's own declaration claiming its bare name, suffixes counted across a scope, and a level a metavariable heads.

use {super::*, crate::Level};

#[test]
fn a_binder_hinted_like_a_shortened_global_is_suffixed() {
    let global = Global::Authored(Qualifier::from(["main", "helper"]));
    let shorten = build_shorten(std::slice::from_ref(&global));
    assert_eq!(shorten.get(&global).as_deref(), Some("helper"));

    let binder = Free::local(0, Some("helper"));
    let names = DisplayNames {
        terms: Vec::new(),
        free: BTreeSet::from([Free::Global(global), binder]),
    };
    let rename = build_rename(
        &names,
        &Spelling::default().with_short_names(Rc::new(shorten)),
    );
    assert_eq!(rename.get(&binder).map(String::as_str), Some("helper2"));
}

/// A tuple label keeps the spelling it was written with, being part of its tuple type's identity, and a binder that would read like it is the one suffixed: a function's parameter `frame` beside its result's field `frame`.
///
/// The two binders are met by rendering: neither scope remembers an identity, so the rename is built from the labels a dry run of the render mints.
#[test]
fn a_tuple_label_keeps_its_spelling_and_a_like_named_binder_is_suffixed() {
    let nat = Term::intrinsic(Intrinsic::NatType);
    let result = Term::tuple_type([(Free::local(1, Some("frame")), nat.clone())]);
    let term = Term::func_type([(Free::local(0, Some("frame")), nat)], result);

    let spelling = Spelling::default();
    let rename = Rc::new(build_rename(&display_names(&term), &spelling));
    let rendered = term
        .spelled(&Rc::new(spelling.with_pretty_names(rename)))
        .to_string();

    assert_eq!(rendered, "(frame2: Nat) -> {frame: Nat}");
}

/// A reader's own declaration keeps the bare name, and a like-named one from the environment takes the longer spelling that actually reaches it.
///
/// One suffix table over both tiers tied `Holds` between the two, so *neither* shortened: the name the reader had just written reported as `/Holds` while `/std/Bool/Holds` — which no bare `Holds` reaches — reported as `Bool/Holds`.
#[test]
fn a_readers_own_declaration_claims_the_bare_name_before_its_environment() {
    let own = Global::Authored(Qualifier::from(["Holds"]));
    let nested = Global::Authored(Qualifier::from(["std", "Bool", "Holds"]));

    let shorten = build_shorten_layered(std::slice::from_ref(&own), std::slice::from_ref(&nested));

    assert_eq!(shorten.get(&own).as_deref(), Some("Holds"));
    assert_eq!(shorten.get(&nested).as_deref(), Some("Bool/Holds"));
}

/// A suffix is counted over every unit of the scope, each unit having counted its own: two units that each declare a `T` under one module name leave neither `T` nor `Vec/T` naming one of them, and a name only one unit carries a suffix of still shortens to it.
#[test]
fn a_suffix_is_counted_across_the_units_of_a_scope() {
    let one = Global::Authored(Qualifier::from(["one", "Vec", "T"]));
    let two = Global::Authored(Qualifier::from(["two", "Vec", "T"]));
    let lone = Global::Authored(Qualifier::from(["two", "Map", "empty"]));

    let shorten = Shorten::over(
        Rc::default(),
        [
            Rc::new(Symbols::of([one])),
            Rc::new(Symbols::of([two, lone])),
        ],
    );

    for shared in [&one, &two] {
        let spelled = shorten.get(shared);

        assert!(
            !matches!(spelled.as_deref(), Some("T" | "Vec/T")),
            "{} took a suffix the other unit's global carries: {spelled:?}",
            shared.faithful()
        );
    }
    assert_eq!(shorten.get(&lone).as_deref(), Some("empty"));
}

/// A *nested* own declaration gets no such claim. Reaching `/Vec/nil` needs `Vec/nil` or an import, exactly as reaching `/std/Vec/nil` does, so its bare label is no more writable than the environment's and it stays in the shared contest — where a genuine tie leaves both spelled in full.
///
/// Handing it the label instead spelled a goal candidate the reader could not paste: `? \u{2248} nil()` for a constructor only `Vec/nil()` reaches.
#[test]
fn a_nested_own_declaration_does_not_claim_its_bare_label() {
    let own_type = Global::Authored(Qualifier::from(["Vec"]));
    let own_ctor = Global::Authored(Qualifier::from(["Vec", "nil"]));
    let outer = Global::Authored(Qualifier::from(["std", "Vec", "nil"]));

    let own = [own_type, own_ctor];
    let shorten = build_shorten_layered(&own, std::slice::from_ref(&outer));

    // The type itself does sit at the unit root, so it takes its label and the environment's twin gives way.
    assert_eq!(shorten.get(&own_type).as_deref(), Some("Vec"));
    assert_eq!(shorten.get(&own_ctor), None);
}

/// The render meets the binders its dry run met, in the same order: two nested binders written `x` read apart only if the rename built from the dry run's labels spells the labels the render mints, since a label it cannot find falls back to its bare hint.
///
/// Mutation-checked: a dry run that mints from a different position than the render fails it.
#[test]
fn a_render_spells_the_binders_its_dry_run_met() {
    let nat = Term::intrinsic(Intrinsic::NatType);
    let inner = Free::local(1, Some("x"));
    let term = Term::func(
        [(Free::local(0, Some("x")), nat.clone())],
        Term::func([(inner, nat)], Term::free_var(&inner)),
    );

    let spelling = Spelling::default();
    let rename = Rc::new(build_rename(&display_names(&term), &spelling));
    let rendered = term
        .spelled(&Rc::new(spelling.with_pretty_names(rename)))
        .to_string();

    assert_eq!(rendered, "(x) => (x2) => x2");
}

/// Like-named binders are spelled apart by the least suffix that is free, however many there are: the first keeps the name, and each one after takes the next number no global and no binder before it holds.
#[test]
fn like_named_binders_take_the_least_free_suffixes() {
    let taken = Global::Authored(Qualifier::from(["main", "x3"]));
    let binders = (0..10_000)
        .map(|index| Free::local(index, None))
        .collect::<Vec<_>>();
    let names = DisplayNames {
        terms: Vec::new(),
        free: binders
            .iter()
            .copied()
            .chain([Free::Global(taken)])
            .collect(),
    };
    let shorten = build_shorten(std::slice::from_ref(&taken));
    let rename = build_rename(
        &names,
        &Spelling::default().with_short_names(Rc::new(shorten)),
    );
    let spelled = |index: usize| rename.get(&binders[index]).map(String::as_str);

    assert_eq!(spelled(0), Some("x"));
    assert_eq!(spelled(1), Some("x2"));
    // `x3` is the global's.
    assert_eq!(spelled(2), Some("x4"));
    assert_eq!(spelled(9_999), Some("x10001"));
}

/// A sort's own level where instances are suppressed: a concrete one is the node's whole content and prints, and one headed by a metavariable names nothing a reader can act on and does not. Held over terms built by hand, a program's sorts above zero being at levels their declarations leave to a caller, which a report meets still open.
#[test]
fn a_concrete_level_prints_where_a_metavariable_headed_one_is_suppressed() {
    let spelling = Rc::new(Spelling::default().with_erased_universes());
    let printed = |level: Level| Term::type_at(level).spelled(&spelling).to_string();
    let one = |level: Level| level.succ().expect("a successor is a level");
    let open = Level::meta(crate::UniverseMetaId(7));

    assert_eq!(printed(Level::zero()), "Type");
    assert_eq!(printed(one(Level::zero())), "Type.{1}");
    assert_eq!(printed(open.clone()), "Type");
    assert_eq!(printed(one(open)), "Type");
}
