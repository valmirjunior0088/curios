//! A rank's key is one for every spelling of one α-class, and its tie orders two of one key by their identities' ages and positions.

use {
    crate::*,
    curios_algebra::Rank,
    curios_utilities::Qualifier,
    std::{cmp::Ordering, collections::BTreeMap},
};

fn local(index: u64) -> Term {
    Term::free_var(&Free::local(index, None))
}

fn global(name: &str) -> Term {
    Term::free_var(&Free::global(Qualifier::from([name])))
}

fn call(arguments: impl IntoIterator<Item = Term>) -> Term {
    Term::apply(global("f"), arguments)
}

fn ordering(this: &Term, that: &Term) -> Ordering {
    this.rank().cmp(&that.rank())
}

#[test]
fn two_spellings_apart_in_names_alone_share_a_key_and_a_repeated_name_does_not() {
    let (x, y, z) = (local(0), local(1), local(2));
    assert_eq!(
        call([x.clone(), y.clone()]).rank().key(),
        call([y.clone(), z]).rank().key()
    );
    assert_ne!(
        call([x.clone(), y]).rank().key(),
        call([x.clone(), x]).rank().key()
    );
}

#[test]
fn two_atoms_of_one_key_are_ordered_by_their_identities_ages() {
    let (x, y) = (local(0), local(1));
    assert_eq!(
        ordering(&call([x.clone()]), &call([y.clone()])),
        Ordering::Less
    );
    assert_eq!(ordering(&call([y]), &call([x])), Ordering::Greater);
}

#[test]
fn two_spellings_of_one_key_part_at_an_identitys_positions() {
    let (x, y) = (local(0), local(1));
    let (this, that) = (call([x.clone(), y.clone()]), call([y, x]));
    assert_eq!(this.rank().key(), that.rank().key());
    let order = ordering(&this, &that);
    assert_ne!(order, Ordering::Equal);
    assert_eq!(ordering(&that, &this), order.reverse());
}

#[test]
fn a_tie_is_read_to_the_first_entry_held_apart() {
    // `f(x, y, z)` against `f(x, y, w)`: one key, `x` and `y` alike, parted at the third identity by its age.
    let (x, y, z, w) = (local(0), local(1), local(2), local(3));
    let this = call([x.clone(), y.clone(), z]);
    let that = call([x, y, w]);
    assert_eq!(this.rank().key(), that.rank().key());
    assert_eq!(ordering(&this, &that), Ordering::Less);
}

#[test]
fn a_spines_order_is_part_of_a_metavariables_rank() {
    let (x, y) = (local(0), local(1));
    let this = Term::metavar_birthed(
        MetavarId(0),
        MetavarOrigin::Hole,
        vec![x.clone(), y.clone()],
    );
    let that = Term::metavar_birthed(MetavarId(0), MetavarOrigin::Hole, vec![y, x]);
    assert_eq!(this.rank().key(), that.rank().key());
    assert_ne!(ordering(&this, &that), Ordering::Equal);
}

#[test]
fn a_spine_rewritten_ranks_as_one_built_from_its_entries() {
    let (x, y, z) = (local(0), local(1), local(2));
    let rewritten = MetavarSpine::from(vec![x.clone(), z.clone()])
        .rewritten(|_, _| false, |entry| (*entry == z).then(|| y.clone()))
        .expect("an entry is rewritten");
    assert_eq!(
        *rewritten.ranked(),
        *MetavarSpine::from(vec![x, y]).ranked()
    );
}

#[test]
fn a_levels_metavariables_stand_at_their_offsets() {
    let level = |first: usize, second: usize| {
        Term::type_at(Level {
            constant: 0,
            atoms: BTreeMap::from([
                (LevelHead::Meta(UniverseMetaId(first)), 1),
                (LevelHead::Meta(UniverseMetaId(second)), 2),
            ]),
        })
    };
    let (this, that) = (level(0, 1), level(1, 0));
    assert_eq!(this.rank().key(), that.rank().key());
    assert_ne!(ordering(&this, &that), Ordering::Equal);
}

#[test]
fn a_term_holding_no_identity_is_ranked_by_its_hash() {
    let term = call([global("a")]);
    assert!(matches!(term.rank(), Rank::One(hash) if hash == term.structural_hash()));
}
