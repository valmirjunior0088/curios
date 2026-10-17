//! Persistent maps: a map cloned in constant time that shares its structure with the map it was cloned from, so a state extended a step at a time costs the logarithm of what stands at each step and copies nothing.
//!
//! **One tree under two faces.** Both are a treap ordered by a `u64` key. An insert copies the nodes on the path to its key and shares every other node with the version it was made from; a node no other version holds is changed in place, so a map nothing else holds is extended as an owned one is. [`Persistent`] orders its keys by their hash, and [`Ordered`] its `i64` positions as numbers.
//!
//! **A tree's shape is a function of the keys it holds.** A node's priority is its key mixed (splitmix64's finalizer), so two versions that hold one set of keys are one tree whatever order the keys arrived in, and a run of consecutive positions is balanced as keys drawn at random are.
//!
//! **A hash is drawn as every table's is.** [`Persistent`] hashes a key with a `RandomState` the map draws where it is made and carries into every version, so which keys stand side by side follows the process, as the standard maps' do, and no answer reads that order: iteration is for a reader that takes every entry. Keys of one hash are held together and told apart by `Eq`.

#[cfg(test)]
mod tests;

use std::{
    collections::hash_map::RandomState,
    hash::{BuildHasher, Hash},
    rc::Rc,
};

/// A node of the treap: its key, its priority, what it holds, and the two subtrees it orders.
#[derive(Clone)]
struct Node<T> {
    key: u64,
    priority: u64,
    payload: T,
    left: Option<Rc<Node<T>>>,
    right: Option<Rc<Node<T>>>,
}

/// A treap by `u64` key, every node shared between the versions that hold it.
#[derive(Clone)]
struct Tree<T> {
    root: Option<Rc<Node<T>>>,
}

impl<T> Tree<T> {
    const fn new() -> Self {
        Self { root: None }
    }

    /// What `key` holds.
    fn get(&self, key: u64) -> Option<&T> {
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

    /// Every payload, in the order of its key.
    fn iter(&self) -> InOrder<'_, T> {
        let mut walk = InOrder {
            pending: Vec::new(),
        };
        walk.descend(self.root.as_deref());
        walk
    }
}

impl<T: Clone> Tree<T> {
    /// `key`'s payload changed by `update`, which is handed the payload `key` holds and answers the payload to place where it held none; `true` where `key` held none.
    fn upsert(&mut self, key: u64, update: impl FnOnce(Option<&mut T>) -> Option<T>) -> bool {
        upsert(&mut self.root, key, update)
    }

    /// The payload `key` held, taken out. A key the tree does not hold copies no node.
    fn remove(&mut self, key: u64) -> Option<T> {
        self.get(key)?;
        remove(&mut self.root, key)
    }
}

/// A node's priority: its key mixed, so the tree's shape is a function of its keys.
fn priority(key: u64) -> u64 {
    let mut mixed = key.wrapping_add(0x9e37_79b9_7f4a_7c15);
    mixed = (mixed ^ (mixed >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    mixed = (mixed ^ (mixed >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    mixed ^ (mixed >> 31)
}

fn upsert<T: Clone>(
    link: &mut Option<Rc<Node<T>>>,
    key: u64,
    update: impl FnOnce(Option<&mut T>) -> Option<T>,
) -> bool {
    let Some(node) = link else {
        if let Some(payload) = update(None) {
            *link = Some(Rc::new(Node {
                key,
                priority: priority(key),
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
            let placed = upsert(&mut node.left, key, update);
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
            let placed = upsert(&mut node.right, key, update);
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
fn remove<T: Clone>(link: &mut Option<Rc<Node<T>>>, key: u64) -> Option<T> {
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
fn merge<T: Clone>(left: Option<Rc<Node<T>>>, right: Option<Rc<Node<T>>>) -> Option<Rc<Node<T>>> {
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
fn rotate_right<T: Clone>(link: &mut Option<Rc<Node<T>>>) {
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
fn rotate_left<T: Clone>(link: &mut Option<Rc<Node<T>>>) {
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
struct InOrder<'t, T> {
    pending: Vec<&'t Node<T>>,
}

impl<'t, T> InOrder<'t, T> {
    fn descend(&mut self, mut node: Option<&'t Node<T>>) {
        while let Some(current) = node {
            self.pending.push(current);
            node = current.left.as_deref();
        }
    }
}

impl<'t, T> Iterator for InOrder<'t, T> {
    type Item = (u64, &'t T);

    fn next(&mut self) -> Option<Self::Item> {
        let node = self.pending.pop()?;
        self.descend(node.right.as_deref());
        Some((node.key, &node.payload))
    }
}

/// A map from keys to values, persistent: a clone is a version that shares every node with the map, and an insert into either leaves the other as it was.
#[derive(Clone)]
pub struct Persistent<K, V> {
    tree: Tree<Vec<(K, V)>>,
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
            .get(self.hasher.hash_one(key))?
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
        self.tree
            .upsert(self.hasher.hash_one(&key), |held| match held {
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
        let bucket = self.tree.get(hash)?;
        let at = bucket.iter().position(|(held, _)| held == key)?;
        self.len -= 1;
        if bucket.len() == 1 {
            return self.tree.remove(hash).map(|mut bucket| bucket.remove(at).1);
        }
        let mut removed = None;
        self.tree.upsert(hash, |held| {
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
fn entries<K, V>(tree: &Tree<Vec<(K, V)>>) -> impl Iterator<Item = (&K, &V)> {
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
    tree: Tree<V>,
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
        self.tree.get(order_key(position))
    }

    /// `value` at `position`, in place of what `position` held.
    pub fn insert(&mut self, position: i64, value: V) {
        let placed = self.tree.upsert(order_key(position), |held| match held {
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
        let removed = self.tree.remove(order_key(position))?;
        self.len -= 1;
        Some(removed)
    }

    /// Every value with its position, the least position first.
    pub fn iter(&self) -> impl Iterator<Item = (i64, &V)> {
        positioned(&self.tree)
    }
}

/// Every value of a position-keyed tree with its position, the least first.
fn positioned<V>(tree: &Tree<V>) -> impl Iterator<Item = (i64, &V)> {
    tree.iter().map(|(key, value)| (position_of(key), value))
}

impl<V: std::fmt::Debug> std::fmt::Debug for Ordered<V> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_map()
            .entries(positioned(&self.tree))
            .finish()
    }
}
