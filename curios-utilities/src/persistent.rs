//! Persistent maps: a map cloned in constant time that shares its structure with the map it was cloned from, so a state extended a step at a time costs the logarithm of what stands at each step and copies nothing.
//!
//! **One tree under three faces.** Each is a treap. An insert copies the nodes on the path to its key and shares every other node with the version it was made from; a node no other version holds is changed in place, so a map nothing else holds is extended as an owned one is. [`Persistent`] orders its keys by their hash, [`Ordered`] its `i64` positions as numbers, and [`Sorted`] its keys as they order themselves.
//!
//! **A tree's shape is a function of the keys it holds.** A node's priority is its key mixed (splitmix64's finalizer), a [`Sorted`] key's hash mixed, so two versions that hold one set of keys are one tree whatever order the keys arrived in, and a run of consecutive positions is balanced as keys drawn at random are.
//!
//! **A hash is drawn as every table's is.** [`Persistent`] hashes a key with a `RandomState` the map draws where it is made and carries into every version, so which keys stand side by side follows the process, as the standard maps' do, and no answer reads that order: iteration is for a reader that takes every entry. Keys of one hash are held together and told apart by `Eq`.

#[cfg(test)]
mod tests;

use std::{
    collections::hash_map::{DefaultHasher, RandomState},
    hash::{BuildHasher, BuildHasherDefault, Hash},
    rc::Rc,
};

/// A node of the treap: its key, its priority, what it holds, and the two subtrees it orders.
#[derive(Clone)]
struct Node<K, T> {
    key: K,
    priority: u64,
    payload: T,
    left: Option<Rc<Node<K, T>>>,
    right: Option<Rc<Node<K, T>>>,
}

/// A treap by key, every node shared between the versions that hold it.
#[derive(Clone)]
struct Tree<K, T> {
    root: Option<Rc<Node<K, T>>>,
}

impl<K, T> Tree<K, T> {
    const fn new() -> Self {
        Self { root: None }
    }

    /// Every entry, in the order of its key.
    fn iter(&self) -> InOrder<'_, K, T> {
        let mut walk = InOrder {
            pending: Vec::new(),
        };
        walk.descend(self.root.as_deref());
        walk
    }
}

impl<K: Ord, T> Tree<K, T> {
    /// What `key` holds.
    fn get(&self, key: &K) -> Option<&T> {
        let mut node = self.root.as_deref();
        while let Some(current) = node {
            node = match key.cmp(&current.key) {
                std::cmp::Ordering::Equal => return Some(&current.payload),
                std::cmp::Ordering::Less => current.left.as_deref(),
                std::cmp::Ordering::Greater => current.right.as_deref(),
            };
        }
        None
    }

    /// The entry of the greatest key below `key`.
    fn before(&self, key: &K) -> Option<(&K, &T)> {
        let (mut node, mut found) = (self.root.as_deref(), None);
        while let Some(current) = node {
            node = match current.key < *key {
                true => {
                    found = Some((&current.key, &current.payload));
                    current.right.as_deref()
                }
                false => current.left.as_deref(),
            };
        }
        found
    }

    /// The entry of the least key above `key`.
    fn after(&self, key: &K) -> Option<(&K, &T)> {
        let (mut node, mut found) = (self.root.as_deref(), None);
        while let Some(current) = node {
            node = match current.key > *key {
                true => {
                    found = Some((&current.key, &current.payload));
                    current.left.as_deref()
                }
                false => current.right.as_deref(),
            };
        }
        found
    }
}

impl<K: Ord + Clone, T: Clone> Tree<K, T> {
    /// `key`'s payload changed by `update`, which is handed the payload `key` holds and answers the payload to place where it held none, a node of `priority`; `true` where `key` held none.
    fn upsert(
        &mut self,
        key: K,
        priority: u64,
        update: impl FnOnce(Option<&mut T>) -> Option<T>,
    ) -> bool {
        upsert(&mut self.root, key, priority, update)
    }

    /// The payload `key` held, taken out. A key the tree does not hold copies no node.
    fn remove(&mut self, key: &K) -> Option<T> {
        self.get(key)?;
        remove(&mut self.root, key)
    }
}

/// One step of a walk that compares two trees in key order: a subtree still to be taken apart, or a node whose own entry comes next.
enum Pending<'t, K, T> {
    Subtree(&'t Node<K, T>),
    Entry(&'t Node<K, T>),
}

/// The top of a walk, a subtree taken apart into its left subtree, its own entry and its right subtree, the left on top.
fn take_apart<'t, K, T>(stack: &mut Vec<Pending<'t, K, T>>) {
    let Some(Pending::Subtree(node)) = stack.pop() else {
        unreachable!("a subtree is taken apart where one is on top");
    };
    if let Some(right) = node.right.as_deref() {
        stack.push(Pending::Subtree(right));
    }
    stack.push(Pending::Entry(node));
    if let Some(left) = node.left.as_deref() {
        stack.push(Pending::Subtree(left));
    }
}

impl<K: Ord, T> Tree<K, T> {
    /// A walk that compares this tree with another, standing at its root.
    fn pending(&self) -> Vec<Pending<'_, K, T>> {
        self.root
            .as_deref()
            .map(Pending::Subtree)
            .into_iter()
            .collect()
    }

    /// Every key at which this tree and `earlier` differ, in key order, each `report`ed with what `earlier` holds there and what this tree does, `None` where one holds nothing: a key both hold is reported where `same` says the two payloads differ. A subtree the two share is passed over whole, so two versions of one map are compared at the cost of what they do not share.
    ///
    /// **The higher subtree is taken apart first.** Where the two walks stand at subtrees that are not one, the one whose root stands higher, by priority, is taken apart, so a subtree shared below it is met whole rather than taken apart from the other side. Which is taken apart decides what the walk costs and never what it reports: entries are compared in key order either way.
    fn diff(
        &self,
        earlier: &Tree<K, T>,
        same: impl Fn(&T, &T) -> bool,
        mut report: impl FnMut(&K, Option<&T>, Option<&T>),
    ) {
        let (mut now, mut then) = (self.pending(), earlier.pending());
        loop {
            match (now.last(), then.last()) {
                (None, None) => return,
                (Some(Pending::Subtree(this)), Some(Pending::Subtree(that)))
                    if std::ptr::eq(*this, *that) =>
                {
                    now.pop();
                    then.pop();
                }
                (Some(Pending::Subtree(this)), Some(Pending::Subtree(that))) => {
                    match this.priority >= that.priority {
                        true => take_apart(&mut now),
                        false => take_apart(&mut then),
                    }
                }
                (Some(Pending::Subtree(_)), _) => take_apart(&mut now),
                (_, Some(Pending::Subtree(_))) => take_apart(&mut then),
                (Some(Pending::Entry(this)), Some(Pending::Entry(that))) => {
                    match this.key.cmp(&that.key) {
                        std::cmp::Ordering::Equal => {
                            if !same(&that.payload, &this.payload) {
                                report(&this.key, Some(&that.payload), Some(&this.payload));
                            }
                            now.pop();
                            then.pop();
                        }
                        std::cmp::Ordering::Less => {
                            report(&this.key, None, Some(&this.payload));
                            now.pop();
                        }
                        std::cmp::Ordering::Greater => {
                            report(&that.key, Some(&that.payload), None);
                            then.pop();
                        }
                    }
                }
                (Some(Pending::Entry(this)), None) => {
                    report(&this.key, None, Some(&this.payload));
                    now.pop();
                }
                (None, Some(Pending::Entry(that))) => {
                    report(&that.key, Some(&that.payload), None);
                    then.pop();
                }
            }
        }
    }
}

/// A node's priority: its key mixed, so the tree's shape is a function of its keys.
fn priority(key: u64) -> u64 {
    let mut mixed = key.wrapping_add(0x9e37_79b9_7f4a_7c15);
    mixed = (mixed ^ (mixed >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    mixed = (mixed ^ (mixed >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    mixed ^ (mixed >> 31)
}

fn upsert<K: Ord + Clone, T: Clone>(
    link: &mut Option<Rc<Node<K, T>>>,
    key: K,
    priority: u64,
    update: impl FnOnce(Option<&mut T>) -> Option<T>,
) -> bool {
    let Some(node) = link else {
        if let Some(payload) = update(None) {
            *link = Some(Rc::new(Node {
                key,
                priority,
                payload,
                left: None,
                right: None,
            }));
            return true;
        }
        return false;
    };
    let node = Rc::make_mut(node);
    match key.cmp(&node.key) {
        std::cmp::Ordering::Equal => {
            update(Some(&mut node.payload));
            false
        }
        std::cmp::Ordering::Less => {
            let placed = upsert(&mut node.left, key, priority, update);
            if node
                .left
                .as_ref()
                .is_some_and(|left| left.priority > node.priority)
            {
                rotate_right(link);
            }
            placed
        }
        std::cmp::Ordering::Greater => {
            let placed = upsert(&mut node.right, key, priority, update);
            if node
                .right
                .as_ref()
                .is_some_and(|right| right.priority > node.priority)
            {
                rotate_left(link);
            }
            placed
        }
    }
}

/// The payload at `key`, taken out of the tree under `link`, its node's two subtrees merged where it stood.
fn remove<K: Ord + Clone, T: Clone>(link: &mut Option<Rc<Node<K, T>>>, key: &K) -> Option<T> {
    let node = Rc::make_mut(link.as_mut()?);
    match key.cmp(&node.key) {
        std::cmp::Ordering::Less => remove(&mut node.left, key),
        std::cmp::Ordering::Greater => remove(&mut node.right, key),
        std::cmp::Ordering::Equal => {
            let merged = merge(node.left.take(), node.right.take());
            let removed = link.take().expect("the node being removed");
            *link = merged;
            Some(Rc::unwrap_or_clone(removed).payload)
        }
    }
}

/// Two subtrees, every key of `left` below every key of `right`, as one: the root of higher priority stands over the merge of the rest.
fn merge<K: Clone, T: Clone>(
    left: Option<Rc<Node<K, T>>>,
    right: Option<Rc<Node<K, T>>>,
) -> Option<Rc<Node<K, T>>> {
    match (left, right) {
        (None, right) => right,
        (left, None) => left,
        (Some(mut left), Some(mut right)) => match left.priority > right.priority {
            true => {
                let node = Rc::make_mut(&mut left);
                node.right = merge(node.right.take(), Some(right));
                Some(left)
            }
            false => {
                let node = Rc::make_mut(&mut right);
                node.left = merge(Some(left), node.left.take());
                Some(right)
            }
        },
    }
}

/// The node at `link` under its left child, which takes its place.
fn rotate_right<K: Clone, T: Clone>(link: &mut Option<Rc<Node<K, T>>>) {
    let mut node = link.take().expect("a node to rotate");
    let mut left = Rc::make_mut(&mut node)
        .left
        .take()
        .expect("a left child to rotate up");
    Rc::make_mut(&mut node).left = Rc::make_mut(&mut left).right.take();
    Rc::make_mut(&mut left).right = Some(node);
    *link = Some(left);
}

/// The node at `link` under its right child, which takes its place.
fn rotate_left<K: Clone, T: Clone>(link: &mut Option<Rc<Node<K, T>>>) {
    let mut node = link.take().expect("a node to rotate");
    let mut right = Rc::make_mut(&mut node)
        .right
        .take()
        .expect("a right child to rotate up");
    Rc::make_mut(&mut node).right = Rc::make_mut(&mut right).left.take();
    Rc::make_mut(&mut right).left = Some(node);
    *link = Some(right);
}

/// A walk of a tree in key order, on a stack of the nodes whose right subtrees are still to come.
struct InOrder<'t, K, T> {
    pending: Vec<&'t Node<K, T>>,
}

impl<K, T> Clone for InOrder<'_, K, T> {
    fn clone(&self) -> Self {
        Self {
            pending: self.pending.clone(),
        }
    }
}

impl<'t, K, T> InOrder<'t, K, T> {
    fn descend(&mut self, mut node: Option<&'t Node<K, T>>) {
        while let Some(current) = node {
            self.pending.push(current);
            node = current.left.as_deref();
        }
    }
}

impl<'t, K, T> Iterator for InOrder<'t, K, T> {
    type Item = (&'t K, &'t T);

    fn next(&mut self) -> Option<Self::Item> {
        let node = self.pending.pop()?;
        self.descend(node.right.as_deref());
        Some((&node.key, &node.payload))
    }
}

/// A map from keys to values, persistent: a clone is a version that shares every node with the map, and an insert into either leaves the other as it was.
#[derive(Clone)]
pub struct Persistent<K, V> {
    tree: Tree<u64, Vec<(K, V)>>,
    len: usize,
    hasher: RandomState,
}

impl<K, V> Default for Persistent<K, V> {
    fn default() -> Self {
        Self {
            tree: Tree::new(),
            len: 0,
            hasher: RandomState::new(),
        }
    }
}

impl<K: Hash + Eq + Clone, V: Clone> Persistent<K, V> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.tree
            .get(&self.hasher.hash_one(key))?
            .iter()
            .find(|(held, _)| held == key)
            .map(|(_, value)| value)
    }

    pub fn contains_key(&self, key: &K) -> bool {
        self.get(key).is_some()
    }

    /// `value` at `key`, in place of what `key` held.
    pub fn insert(&mut self, key: K, value: V) {
        let mut added = false;
        let hash = self.hasher.hash_one(&key);
        self.tree.upsert(hash, priority(hash), |held| match held {
            None => {
                added = true;
                Some(vec![(key, value)])
            }
            Some(bucket) => {
                match bucket.iter_mut().find(|(held, _)| *held == key) {
                    Some((_, held)) => *held = value,
                    None => {
                        added = true;
                        bucket.push((key, value));
                    }
                }
                None
            }
        });
        self.len += usize::from(added);
    }

    /// The value `key` held, taken out.
    pub fn remove(&mut self, key: &K) -> Option<V> {
        let hash = self.hasher.hash_one(key);
        let bucket = self.tree.get(&hash)?;
        let at = bucket.iter().position(|(held, _)| held == key)?;
        self.len -= 1;
        if bucket.len() == 1 {
            return self
                .tree
                .remove(&hash)
                .map(|mut bucket| bucket.remove(at).1);
        }
        let mut removed = None;
        self.tree.upsert(hash, priority(hash), |held| {
            removed = held.map(|bucket| bucket.remove(at).1);
            None
        });
        removed
    }

    /// Every entry, in an order no reader may depend on.
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        entries(&self.tree)
    }
}

/// Every entry of a hash-keyed tree, bucket by bucket.
fn entries<K, V>(tree: &Tree<u64, Vec<(K, V)>>) -> impl Iterator<Item = (&K, &V)> {
    tree.iter()
        .flat_map(|(_, bucket)| bucket.iter().map(|(key, value)| (key, value)))
}

impl<K: std::fmt::Debug, V: std::fmt::Debug> std::fmt::Debug for Persistent<K, V> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.debug_map().entries(entries(&self.tree)).finish()
    }
}

/// Values at `i64` positions, persistent as [`Persistent`] is, read back in the order of their positions.
#[derive(Clone)]
pub struct Ordered<V> {
    tree: Tree<u64, V>,
    len: usize,
}

impl<V> Default for Ordered<V> {
    fn default() -> Self {
        Self {
            tree: Tree::new(),
            len: 0,
        }
    }
}

/// A position as a key that orders as the position does.
fn order_key(position: i64) -> u64 {
    (position as u64) ^ (1 << 63)
}

/// The position an order key stands for.
fn position_of(key: u64) -> i64 {
    (key ^ (1 << 63)) as i64
}

impl<V: Clone> Ordered<V> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn get(&self, position: i64) -> Option<&V> {
        self.tree.get(&order_key(position))
    }

    /// The greatest position below `position` that holds a value, with the value.
    pub fn before(&self, position: i64) -> Option<(i64, &V)> {
        self.tree
            .before(&order_key(position))
            .map(|(key, value)| (position_of(*key), value))
    }

    /// The least position above `position` that holds a value, with the value.
    pub fn after(&self, position: i64) -> Option<(i64, &V)> {
        self.tree
            .after(&order_key(position))
            .map(|(key, value)| (position_of(*key), value))
    }

    /// `value` at `position`, in place of what `position` held.
    pub fn insert(&mut self, position: i64, value: V) {
        let key = order_key(position);
        let placed = self.tree.upsert(key, priority(key), |held| match held {
            None => Some(value),
            Some(held) => {
                *held = value;
                None
            }
        });
        self.len += usize::from(placed);
    }

    /// The value `position` held, taken out.
    pub fn remove(&mut self, position: i64) -> Option<V> {
        let removed = self.tree.remove(&order_key(position))?;
        self.len -= 1;
        Some(removed)
    }

    /// Every value with its position, the least position first.
    pub fn iter(&self) -> impl Iterator<Item = (i64, &V)> + Clone {
        positioned(&self.tree)
    }

    /// Whether the two are one version, sharing every node: so for a version and its clone until either is written. Two versions may hold the same values and not be one.
    pub fn same_version(&self, other: &Self) -> bool {
        match (&self.tree.root, &other.tree.root) {
            (Some(ours), Some(theirs)) => Rc::ptr_eq(ours, theirs),
            (None, None) => true,
            _ => false,
        }
    }

    /// Every position at which this map and `earlier` differ, the least first, each `report`ed with what `earlier` holds there and what this map does, `None` where one holds nothing: a position both hold is reported where `same` says the two values differ. What two versions of one map share is passed over unread, so comparing them costs what they do not share.
    pub fn diff(
        &self,
        earlier: &Ordered<V>,
        same: impl Fn(&V, &V) -> bool,
        mut report: impl FnMut(i64, Option<&V>, Option<&V>),
    ) {
        self.tree.diff(&earlier.tree, same, |key, then, now| {
            report(position_of(*key), then, now);
        });
    }
}

/// Every value of a position-keyed tree with its position, the least first.
fn positioned<V>(tree: &Tree<u64, V>) -> impl Iterator<Item = (i64, &V)> + Clone {
    tree.iter().map(|(key, value)| (position_of(*key), value))
}

impl<V: std::fmt::Debug> std::fmt::Debug for Ordered<V> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_map()
            .entries(positioned(&self.tree))
            .finish()
    }
}

/// Values by key, persistent as [`Persistent`] is, read back in the order of their keys: what a table a unit stores by name is kept in, the order it is written in being the keys' own.
#[derive(Clone)]
pub struct Sorted<K, V> {
    tree: Tree<K, V>,
    len: usize,
}

impl<K, V> Default for Sorted<K, V> {
    fn default() -> Self {
        Self {
            tree: Tree::new(),
            len: 0,
        }
    }
}

/// A key's priority: its hash under a hasher every map shares, mixed, so a tree's shape is a function of its keys.
fn sorted_priority<K: Hash>(key: &K) -> u64 {
    priority(BuildHasherDefault::<DefaultHasher>::default().hash_one(key))
}

impl<K: Ord + Hash + Clone, V: Clone> Sorted<K, V> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.tree.get(key)
    }

    pub fn contains_key(&self, key: &K) -> bool {
        self.get(key).is_some()
    }

    /// `value` at `key`, in place of what `key` held.
    pub fn insert(&mut self, key: K, value: V) {
        let priority = sorted_priority(&key);
        let placed = self.tree.upsert(key, priority, |held| match held {
            None => Some(value),
            Some(held) => {
                *held = value;
                None
            }
        });
        self.len += usize::from(placed);
    }

    /// The value `key` held, taken out.
    pub fn remove(&mut self, key: &K) -> Option<V> {
        let removed = self.tree.remove(key)?;
        self.len -= 1;
        Some(removed)
    }

    /// Every entry, the least key first.
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.tree.iter()
    }
}

/// Two maps read in key order are one where they hold the same entries, whatever versions they are.
impl<K: PartialEq, V: PartialEq> PartialEq for Sorted<K, V> {
    fn eq(&self, other: &Self) -> bool {
        self.len == other.len && self.tree.iter().eq(other.tree.iter())
    }
}

impl<K: Eq, V: Eq> Eq for Sorted<K, V> {}

impl<K: std::fmt::Debug, V: std::fmt::Debug> std::fmt::Debug for Sorted<K, V> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.debug_map().entries(self.tree.iter()).finish()
    }
}

/// How a [`Sorted`] map is archived: as its entries, the least key first, so a map is stored as the order it is read back in and restored by inserting each.
pub struct KeyOrder;

/// The field adapter: `#[archived_with(InKeyOrder)]`.
pub type InKeyOrder = curios_archive::Via<KeyOrder>;

impl<K: Ord + Hash + Clone, V: Clone> curios_archive::Proxy<Sorted<K, V>> for KeyOrder {
    type Archivable = Vec<(K, V)>;

    fn to_archivable(map: &Sorted<K, V>) -> impl std::borrow::Borrow<Vec<(K, V)>> {
        map.iter()
            .map(|(key, value)| (key.clone(), value.clone()))
            .collect::<Vec<_>>()
    }

    fn from_archivable(entries: Vec<(K, V)>) -> Result<Sorted<K, V>, String> {
        let mut map = Sorted::new();
        for (key, value) in entries {
            map.insert(key, value);
        }

        Ok(map)
    }
}
