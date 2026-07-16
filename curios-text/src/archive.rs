//! Deterministic rkyv adapters for resolver maps.

use {
    rkyv::{
        Archive, Archived, Deserialize, Place, Resolver, Serialize,
        rancor::Fallible,
        with::{ArchiveWith, DeserializeWith, SerializeWith},
    },
    std::{collections::HashMap, hash::Hash},
};

pub struct OrderedMap;

fn ordered<K, V>(map: &HashMap<K, V>) -> Vec<(K, V)>
where
    K: Clone + Ord,
    V: Clone,
{
    let mut entries = map
        .iter()
        .map(|(key, value)| (key.clone(), value.clone()))
        .collect::<Vec<_>>();
    entries.sort_by(|(left, _), (right, _)| left.cmp(right));
    entries
}

impl<K, V> ArchiveWith<HashMap<K, V>> for OrderedMap
where
    K: Archive + Clone + Ord,
    V: Archive + Clone,
{
    type Archived = Archived<Vec<(K, V)>>;
    type Resolver = Resolver<Vec<(K, V)>>;

    fn resolve_with(map: &HashMap<K, V>, resolver: Self::Resolver, out: Place<Self::Archived>) {
        ordered(map).resolve(resolver, out);
    }
}

impl<K, V, S> SerializeWith<HashMap<K, V>, S> for OrderedMap
where
    S: Fallible + ?Sized,
    K: Archive + Clone + Ord,
    V: Archive + Clone,
    Vec<(K, V)>: Serialize<S>,
{
    fn serialize_with(map: &HashMap<K, V>, serializer: &mut S) -> Result<Self::Resolver, S::Error> {
        ordered(map).serialize(serializer)
    }
}

impl<K, V, D> DeserializeWith<Archived<Vec<(K, V)>>, HashMap<K, V>, D> for OrderedMap
where
    D: Fallible + ?Sized,
    K: Archive + Eq + Hash,
    V: Archive,
    Archived<Vec<(K, V)>>: Deserialize<Vec<(K, V)>, D>,
{
    fn deserialize_with(
        entries: &Archived<Vec<(K, V)>>,
        deserializer: &mut D,
    ) -> Result<HashMap<K, V>, D::Error> {
        Ok(entries.deserialize(deserializer)?.into_iter().collect())
    }
}
