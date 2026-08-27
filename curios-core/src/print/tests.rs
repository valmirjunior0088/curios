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
