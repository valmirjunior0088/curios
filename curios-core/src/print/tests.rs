use super::*;

#[test]
fn a_binder_hinted_like_a_shortened_global_is_suffixed() {
    let global = Global::Authored(Qualifier::from(["main", "helper"]));
    let shorten = build_shorten(std::slice::from_ref(&global));
    assert_eq!(shorten.get(&global).map(String::as_str), Some("helper"));

    let binder = Free::local(0, Some("helper"));
    let names = BTreeSet::from([Free::Global(global), binder.clone()]);
    let rename = build_rename(&names, &shorten);
    assert_eq!(rename.get(&binder).map(String::as_str), Some("helper2"));
}

/// A reader's own declaration keeps the bare name, and a like-named one from the environment takes the longer spelling that actually reaches it.
///
/// One suffix table over both tiers tied `Holds` between the two, so *neither* shortened: the name the reader had just written reported as `/Holds` while `/std/Bool/Holds` — which no bare `Holds` reaches — reported as `Bool/Holds`.
#[test]
fn a_readers_own_declaration_claims_the_bare_name_before_its_environment() {
    let own = Global::Authored(Qualifier::from(["Holds"]));
    let nested = Global::Authored(Qualifier::from(["std", "Bool", "Holds"]));

    let shorten = build_shorten_layered(std::slice::from_ref(&own), std::slice::from_ref(&nested));

    assert_eq!(shorten.get(&own).map(String::as_str), Some("Holds"));
    assert_eq!(shorten.get(&nested).map(String::as_str), Some("Bool/Holds"));
}

/// A *nested* own declaration gets no such claim. Reaching `/Vec/nil` needs `Vec/nil` or an import, exactly as reaching `/std/Vec/nil` does, so its bare label is no more writable than the environment's and it stays in the shared contest — where a genuine tie leaves both spelled in full.
///
/// Handing it the label instead spelled a goal candidate the reader could not paste: `? \u{2248} nil()` for a constructor only `Vec/nil()` reaches.
#[test]
fn a_nested_own_declaration_does_not_claim_its_bare_label() {
    let own_type = Global::Authored(Qualifier::from(["Vec"]));
    let own_ctor = Global::Authored(Qualifier::from(["Vec", "nil"]));
    let outer = Global::Authored(Qualifier::from(["std", "Vec", "nil"]));

    let own = [own_type.clone(), own_ctor.clone()];
    let shorten = build_shorten_layered(&own, std::slice::from_ref(&outer));

    // The type itself does sit at the unit root, so it takes its label and the environment's twin gives way.
    assert_eq!(shorten.get(&own_type).map(String::as_str), Some("Vec"));
    assert_eq!(shorten.get(&own_ctor), None);
}

/// Building a document descends once per link, so this is what [`sub`]'s guard is for — and the depth a diagnostic's term can reach is the elaborator's, not the writer's. Deep enough that a regression is a stack overflow rather than a slow test. The other two walks over a document, running and freeing it, are fixtured in `curios-utilities` at the same depth.
#[test]
fn a_deep_term_is_printed_without_overflowing() {
    const DEEP: usize = 100_000;

    let argument = Term::free_var(&Free::local(0, None));
    let mut term = Term::free_var(&Free::local(0, None));
    for _ in 0..DEEP {
        term = Term::apply(term, [argument.clone()]);
    }

    assert_eq!(term.to_string().matches('(').count(), DEEP);
}

/// A tuple type's unlabeled positions print as source writes them.
///
/// The rebuild that restores source labels reads them from [`Telescope::labels`], which renders a hintless binder as `""`. Restoring that as a *hint* would make every unlabeled position look labeled to this printer, and the rename map would then disambiguate the shared empty spelling into `2`, `3` — so `{Nat, Bool, Str}` printed as `{: Nat, 2: Bool, 3: Str}` in every report that named one.
#[test]
fn an_unlabeled_tuple_type_prints_without_labels() {
    let telescope = Telescope::build(
        [
            (Free::local(0, None), Term::intrinsic(Intrinsic::NatType)),
            (Free::local(1, None), Term::intrinsic(Intrinsic::BoolType)),
            (Free::local(2, None), Term::intrinsic(Intrinsic::ByteType)),
        ],
        (),
    );
    let labels = telescope.labels();
    let relabelled = telescope.clone().relabel(&labels);

    let tuple: Term = Subterm::TupleType(TupleType {
        telescope: relabelled,
    })
    .into();
    assert_eq!(tuple.to_string(), "{Nat, Bool, Byte}");
}

/// The other half of the same rule: a position the source *did* label keeps it through the identical rebuild.
#[test]
fn a_labeled_tuple_type_keeps_its_labels_through_a_rebuild() {
    let telescope = Telescope::build(
        [
            (
                Free::local(0, Some("fst")),
                Term::intrinsic(Intrinsic::NatType),
            ),
            (Free::local(1, None), Term::intrinsic(Intrinsic::BoolType)),
        ],
        (),
    );
    let labels = telescope.labels();
    let relabelled = telescope.clone().relabel(&labels);

    let tuple: Term = Subterm::TupleType(TupleType {
        telescope: relabelled,
    })
    .into();
    assert_eq!(tuple.to_string(), "{fst: Nat, Bool}");
}
