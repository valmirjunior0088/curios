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
