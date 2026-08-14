//! How a resolver map is archived: as its entries, sorted by key.
//!
//! Sorted because a `HashMap`'s iteration order is not deterministic, and the prelude image is compared byte-for-byte against a second serialization of the same value to prove it is. An unordered archive would fail that check on some runs and pass on others, which is the worst way for it to fail.

use {
    curios_archive::{Proxy, Via},
    std::{borrow::Borrow, collections::HashMap, hash::Hash},
};

pub(crate) struct SortedEntries;

impl<K, V> Proxy<HashMap<K, V>> for SortedEntries
where
    K: Clone + Ord + Eq + Hash,
    V: Clone,
{
    type Archivable = Vec<(K, V)>;

    fn to_archivable(map: &HashMap<K, V>) -> impl Borrow<Vec<(K, V)>> {
        let mut entries = map
            .iter()
            .map(|(key, value)| (key.clone(), value.clone()))
            .collect::<Vec<_>>();
        entries.sort_by(|(left, _), (right, _)| left.cmp(right));

        entries
    }

    /// Infallible, and deliberately not checking for duplicate keys: the entries came from a map, so there are none, and a duplicate would mean the archive was corrupt in a way bytecheck already refuses.
    fn from_archivable(entries: Vec<(K, V)>) -> Result<HashMap<K, V>, String> {
        Ok(entries.into_iter().collect())
    }
}

pub(crate) type OrderedMap = Via<SortedEntries>;
