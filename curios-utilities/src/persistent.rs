//! Persistent maps: a map cloned in constant time that shares its structure with the map it was cloned from, so a state extended a step at a time costs the logarithm of what stands at each step and copies nothing.
//!
//! **One tree under three faces.** Each is a treap. An insert copies the nodes on the path to its key and shares every other node with the version it was made from; a node no other version holds is changed in place, so a map nothing else holds is extended as an owned one is. [`Persistent`] orders its keys by their hash, [`Ordered`] its `i64` positions as numbers, and [`Sorted`] its keys as they order themselves.
//!
//! **A tree's shape is a function of the keys it holds.** A node's priority is its key mixed (splitmix64's finalizer), a [`Sorted`] key's hash mixed, so two versions that hold one set of keys are one tree whatever order the keys arrived in, and a run of consecutive positions is balanced as keys drawn at random are.
//!
//! **A subtree can carry a summary.** A node keeps the [`Summary`] of its subtree's values beside its own, made again on every path a write copies, so a walk can pass over a subtree its summary rules out ([`Ordered::rewritten_where`]); the unit summarizes nothing, and every map but a summarized [`Ordered`] keeps it.
//!
//! **A hash is drawn as every table's is.** [`Persistent`] hashes a key with a `RandomState` the map draws where it is made and carries into every version, so which keys stand side by side follows the process, as the standard maps' do, and no answer reads that order: iteration is for a reader that takes every entry. Keys of one hash are held together and told apart by `Eq`.

#[cfg(test)]
mod tests;

use std::{
    collections::hash_map::{DefaultHasher, RandomState},
    hash::{BuildHasher, BuildHasherDefault, Hash},
    rc::Rc,
};

/// What a tree keeps of each subtree beside its values: the summary of one value, joined over a subtree in the order of its keys. Kept on every node and made again on each path a write copies, it is what a walk reads to pass over a subtree whole.
pub trait Summary<T>: Clone {
    fn of(value: &T) -> Self;
    fn join(&self, other: &Self) -> Self;
}

/// The summary of nothing, which every map that reads none keeps at no cost.
impl<T> Summary<T> for () {
    fn of(_: &T) -> Self {}

    fn join(&self, _: &Self) -> Self {}
}

/// A node of the treap: its key, its priority, what it holds, the summary of its subtree, and the two subtrees it orders.
#[derive(Clone)]
struct Node<K, T, S = ()> {
    key: K,
    priority: u64,
    payload: T,
    summary: S,
    left: Option<Rc<Node<K, T, S>>>,
    right: Option<Rc<Node<K, T, S>>>,
}

impl<K, T, S: Summary<T>> Node<K, T, S> {
    /// The summary made again of the node's value and its subtrees', after either changed.
    fn resummarize(&mut self) {
        let mut summary = S::of(&self.payload);
        if let Some(left) = &self.left {
            summary = left.summary.join(&summary);
        }
        if let Some(right) = &self.right {
            summary = summary.join(&right.summary);
        }
        self.summary = summary;
    }
}

/// A treap by key, every node shared between the versions that hold it.
#[derive(Clone)]
struct Tree<K, T, S = ()> {
    root: Option<Rc<Node<K, T, S>>>,
}

impl<K, T, S> Tree<K, T, S> {
    const fn new() -> Self {
        Self { root: None }
    }

    /// Every entry, in the order of its key.
    fn iter(&self) -> InOrder<'_, K, T, S> {
        let mut walk = InOrder::new();
        walk.descend(self.root.as_deref());
        walk
    }

    /// Whether the two are one version, sharing every node.
    fn same_version(&self, other: &Self) -> bool {
        match (&self.root, &other.root) {
            (Some(ours), Some(theirs)) => Rc::ptr_eq(ours, theirs),
            (None, None) => true,
            _ => false,
        }
    }

    /// The entry of the greatest key.
    fn last(&self) -> Option<(&K, &T)> {
        let mut node = self.root.as_deref()?;
        while let Some(right) = node.right.as_deref() {
            node = right;
        }
        Some((&node.key, &node.payload))
    }
}

impl<K: Ord, T, S> Tree<K, T, S> {
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

    /// Every entry from the least key at or above `key`, in the order of its key: the walk [`Tree::iter`] would be past every entry below it, found on the path to `key` alone.
    fn iter_from(&self, key: &K) -> InOrder<'_, K, T, S> {
        let mut walk = InOrder::new();
        let mut node = self.root.as_deref();
        while let Some(current) = node {
            node = match current.key < *key {
                true => current.right.as_deref(),
                false => {
                    walk.push(current);
                    current.left.as_deref()
                }
            };
        }
        walk
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

impl<K: Ord + Clone, T: Clone, S: Summary<T>> Tree<K, T, S> {
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
enum Pending<'t, K, T, S> {
    Subtree(&'t Node<K, T, S>),
    Entry(&'t Node<K, T, S>),
}

/// The top of a walk, a subtree taken apart into its left subtree, its own entry and its right subtree, the left on top.
fn take_apart<'t, K, T, S>(stack: &mut Vec<Pending<'t, K, T, S>>) {
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

impl<K: Ord, T, S> Tree<K, T, S> {
    /// A walk that compares this tree with another, standing at its root.
    fn pending(&self) -> Vec<Pending<'_, K, T, S>> {
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
        earlier: &Tree<K, T, S>,
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

fn upsert<K: Ord + Clone, T: Clone, S: Summary<T>>(
    link: &mut Option<Rc<Node<K, T, S>>>,
    key: K,
    priority: u64,
    update: impl FnOnce(Option<&mut T>) -> Option<T>,
) -> bool {
    let Some(node) = link else {
        if let Some(payload) = update(None) {
            *link = Some(Rc::new(Node {
                key,
                priority,
                summary: S::of(&payload),
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
            node.resummarize();
            false
        }
        std::cmp::Ordering::Less => {
            let placed = upsert(&mut node.left, key, priority, update);
            node.resummarize();
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
            node.resummarize();
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
fn remove<K: Ord + Clone, T: Clone, S: Summary<T>>(
    link: &mut Option<Rc<Node<K, T, S>>>,
    key: &K,
) -> Option<T> {
    let node = Rc::make_mut(link.as_mut()?);
    match key.cmp(&node.key) {
        std::cmp::Ordering::Less => {
            let removed = remove(&mut node.left, key);
            node.resummarize();
            removed
        }
        std::cmp::Ordering::Greater => {
            let removed = remove(&mut node.right, key);
            node.resummarize();
            removed
        }
        std::cmp::Ordering::Equal => {
            let merged = merge(node.left.take(), node.right.take());
            let removed = link.take().expect("the node being removed");
            *link = merged;
            Some(Rc::unwrap_or_clone(removed).payload)
        }
    }
}

/// Two subtrees, every key of `left` below every key of `right`, as one: the root of higher priority stands over the merge of the rest.
fn merge<K: Clone, T: Clone, S: Summary<T>>(
    left: Option<Rc<Node<K, T, S>>>,
    right: Option<Rc<Node<K, T, S>>>,
) -> Option<Rc<Node<K, T, S>>> {
    match (left, right) {
        (None, right) => right,
        (left, None) => left,
        (Some(mut left), Some(mut right)) => match left.priority > right.priority {
            true => {
                let node = Rc::make_mut(&mut left);
                node.right = merge(node.right.take(), Some(right));
                node.resummarize();
                Some(left)
            }
            false => {
                let node = Rc::make_mut(&mut right);
                node.left = merge(Some(left), node.left.take());
                node.resummarize();
                Some(right)
            }
        },
    }
}

/// The node at `link` under its left child, which takes its place.
fn rotate_right<K: Clone, T: Clone, S: Summary<T>>(link: &mut Option<Rc<Node<K, T, S>>>) {
    let mut node = link.take().expect("a node to rotate");
    let mut left = Rc::make_mut(&mut node)
        .left
        .take()
        .expect("a left child to rotate up");
    let lowered = Rc::make_mut(&mut node);
    lowered.left = Rc::make_mut(&mut left).right.take();
    lowered.resummarize();
    let raised = Rc::make_mut(&mut left);
    raised.right = Some(node);
    raised.resummarize();
    *link = Some(left);
}

/// The node at `link` under its right child, which takes its place.
fn rotate_left<K: Clone, T: Clone, S: Summary<T>>(link: &mut Option<Rc<Node<K, T, S>>>) {
    let mut node = link.take().expect("a node to rotate");
    let mut right = Rc::make_mut(&mut node)
        .right
        .take()
        .expect("a right child to rotate up");
    let lowered = Rc::make_mut(&mut node);
    lowered.right = Rc::make_mut(&mut right).left.take();
    lowered.resummarize();
    let raised = Rc::make_mut(&mut right);
    raised.left = Some(node);
    raised.resummarize();
    *link = Some(right);
}

/// The subtree at `link` with each value `visit` makes something new of replaced, its paths copied and their summaries made again, or `None` where `visit` made nothing new: a subtree, or a value, whose summary `passes` holds is passed over unvisited. Visited in the order of the keys.
fn rewritten<K: Clone, T: Clone, S: Summary<T>>(
    link: &Option<Rc<Node<K, T, S>>>,
    passes: &impl Fn(&S) -> bool,
    visit: &mut impl FnMut(&K, &T) -> Option<T>,
) -> Option<Option<Rc<Node<K, T, S>>>> {
    let node = link.as_deref()?;
    if passes(&node.summary) {
        return None;
    }
    let left = rewritten(&node.left, passes, visit);
    let own = match passes(&S::of(&node.payload)) {
        true => None,
        false => visit(&node.key, &node.payload),
    };
    let right = rewritten(&node.right, passes, visit);
    if left.is_none() && own.is_none() && right.is_none() {
        return None;
    }
    let mut copy = node.clone();
    if let Some(left) = left {
        copy.left = left;
    }
    if let Some(own) = own {
        copy.payload = own;
    }
    if let Some(right) = right {
        copy.right = right;
    }
    copy.resummarize();
    Some(Some(Rc::new(copy)))
}

/// How many nodes a walk's stack holds in place before it takes an allocation: a path as long as a treap of tens of thousands of keys is expected to have, so a walk of a small map, which a node's free-variable set is, allocates nothing.
const INLINE_DEPTH: usize = 32;

/// A walk of a tree in key order, on a stack of the nodes whose right subtrees are still to come: the first `INLINE_DEPTH` of them in place, the rest spilled past it.
pub struct InOrder<'t, K, T, S = ()> {
    inline: [Option<&'t Node<K, T, S>>; INLINE_DEPTH],
    depth: usize,
    spilled: Vec<&'t Node<K, T, S>>,
}

impl<K, T, S> Clone for InOrder<'_, K, T, S> {
    fn clone(&self) -> Self {
        Self {
            inline: self.inline,
            depth: self.depth,
            spilled: self.spilled.clone(),
        }
    }
}

impl<'t, K, T, S> InOrder<'t, K, T, S> {
    fn new() -> Self {
        Self {
            inline: [None; INLINE_DEPTH],
            depth: 0,
            spilled: Vec::new(),
        }
    }

    /// The newest node on top: in place while there is room, spilled once there is not, so the spilled nodes are always the newest.
    fn push(&mut self, node: &'t Node<K, T, S>) {
        match self.depth < INLINE_DEPTH {
            true => {
                self.inline[self.depth] = Some(node);
                self.depth += 1;
            }
            false => self.spilled.push(node),
        }
    }

    fn pop(&mut self) -> Option<&'t Node<K, T, S>> {
        if let Some(node) = self.spilled.pop() {
            return Some(node);
        }
        self.depth = self.depth.checked_sub(1)?;
        self.inline[self.depth].take()
    }

    fn descend(&mut self, mut node: Option<&'t Node<K, T, S>>) {
        while let Some(current) = node {
            self.push(current);
            node = current.left.as_deref();
        }
    }
}

impl<'t, K, T, S> Iterator for InOrder<'t, K, T, S> {
    type Item = (&'t K, &'t T);

    fn next(&mut self) -> Option<Self::Item> {
        let node = self.pop()?;
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

/// Values at `i64` positions, persistent as [`Persistent`] is, read back in the order of their positions, each subtree keeping the [`Summary`] `S` of its values.
#[derive(Clone)]
pub struct Ordered<V, S = ()> {
    tree: Tree<u64, V, S>,
    len: usize,
}

impl<V, S> Default for Ordered<V, S> {
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
    /// A map that summarizes nothing; a summarized one is made by `default`.
    pub fn new() -> Self {
        Self::default()
    }
}

impl<V: Clone, S: Summary<V>> Ordered<V, S> {
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

    /// The version this is, as an address: one for a version and every clone of it until either is written, as [`Ordered::same_version`] tells two apart, and so a key a walk may remember what it made of one by for as long as it holds the version.
    pub fn version(&self) -> usize {
        self.tree
            .root
            .as_ref()
            .map_or(0, |root| Rc::as_ptr(root) as usize)
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
        earlier: &Ordered<V, S>,
        same: impl Fn(&V, &V) -> bool,
        mut report: impl FnMut(i64, Option<&V>, Option<&V>),
    ) {
        self.tree.diff(&earlier.tree, same, |key, then, now| {
            report(position_of(*key), then, now);
        });
    }

    /// The summary of every value, `None` where the map holds none.
    pub fn summary(&self) -> Option<&S> {
        self.tree.root.as_deref().map(|root| &root.summary)
    }

    /// The map with each value `visit` makes something new of replaced, or `None` where it makes nothing new of any. A subtree, or a value, whose summary `passes` holds is passed over unvisited, and only the paths to the values replaced are copied, so a rewrite costs the subtrees its summaries do not rule out. Values are visited in the order of their positions.
    pub fn rewritten_where(
        &self,
        passes: impl Fn(&S) -> bool,
        mut visit: impl FnMut(i64, &V) -> Option<V>,
    ) -> Option<Self> {
        let root = rewritten(&self.tree.root, &passes, &mut |key: &u64, value: &V| {
            visit(position_of(*key), value)
        })?;
        Some(Self {
            tree: Tree { root },
            len: self.len,
        })
    }
}

/// Every value of a position-keyed tree with its position, the least first.
fn positioned<V, S>(tree: &Tree<u64, V, S>) -> impl Iterator<Item = (i64, &V)> + Clone {
    tree.iter().map(|(key, value)| (position_of(*key), value))
}

impl<V: std::fmt::Debug, S> std::fmt::Debug for Ordered<V, S> {
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

    /// The value `key` holds, changed in place by `change`: the nodes on the path to it are copied where another version holds them, and nothing is done where `key` holds nothing. Whether it held a value.
    pub fn update(&mut self, key: &K, change: impl FnOnce(&mut V)) -> bool {
        if self.tree.get(key).is_none() {
            return false;
        }
        self.tree.upsert(key.clone(), sorted_priority(key), |held| {
            if let Some(held) = held {
                change(held);
            }
            None
        });

        true
    }

    /// The entry of the greatest key below `key`.
    pub fn before(&self, key: &K) -> Option<(&K, &V)> {
        self.tree.before(key)
    }

    /// The entry of the least key above `key`.
    pub fn after(&self, key: &K) -> Option<(&K, &V)> {
        self.tree.after(key)
    }

    /// The entry of the greatest key.
    pub fn last(&self) -> Option<(&K, &V)> {
        self.tree.last()
    }

    /// Every entry, the least key first.
    pub fn iter(&self) -> InOrder<'_, K, V> {
        self.tree.iter()
    }

    /// Every entry from the least key at or above `key`, the least first.
    pub fn iter_from(&self, key: &K) -> InOrder<'_, K, V> {
        self.tree.iter_from(key)
    }

    /// Whether the two are one version, sharing every node: so for a version and its clone until either is written. Two versions may hold the same entries and not be one.
    pub fn same_version(&self, other: &Self) -> bool {
        self.tree.same_version(&other.tree)
    }

    /// Every entry either map holds, an entry both hold at what `combine` makes of the two values, `self`'s first, where it answers one, and at the larger map's where it answers none.
    ///
    /// **The smaller into the larger.** The larger map is taken as it stands and each entry of the smaller is put into it, so a union copies the paths to the keys it adds or changes and shares every other node with the larger: O(m log n) for maps of m ≤ n entries, the larger itself where nothing is added, and either where the two are one version. Its shape is a function of its keys, as every version's is.
    pub fn union_with(
        &self,
        other: &Self,
        mut combine: impl FnMut(&K, &V, &V) -> Option<V>,
    ) -> Self {
        if self.same_version(other) {
            return self.clone();
        }
        let (larger, smaller, self_larger) = match self.len >= other.len {
            true => (self, other, true),
            false => (other, self, false),
        };
        let mut union = larger.clone();
        for (key, value) in smaller.iter() {
            let placed = match union.get(key) {
                None => Some(value.clone()),
                Some(held) => match self_larger {
                    true => combine(key, held, value),
                    false => combine(key, value, held),
                },
            };
            if let Some(placed) = placed {
                union.insert(key.clone(), placed);
            }
        }
        union
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

/// How a [`Sorted`] map whose values are held behind an `Rc` is archived: as [`KeyOrder`] archives one, each value stored as itself, so a value two versions share is stored as one held alone would be.
pub struct SharedKeyOrder;

/// The field adapter: `#[archived_with(InSharedKeyOrder)]`.
pub type InSharedKeyOrder = curios_archive::Via<SharedKeyOrder>;

impl<K: Ord + Hash + Clone, V: Clone> curios_archive::Proxy<Sorted<K, Rc<V>>> for SharedKeyOrder {
    type Archivable = Vec<(K, V)>;

    fn to_archivable(map: &Sorted<K, Rc<V>>) -> impl std::borrow::Borrow<Vec<(K, V)>> {
        map.iter()
            .map(|(key, value)| (key.clone(), (**value).clone()))
            .collect::<Vec<_>>()
    }

    fn from_archivable(entries: Vec<(K, V)>) -> Result<Sorted<K, Rc<V>>, String> {
        let mut map = Sorted::new();
        for (key, value) in entries {
            map.insert(key, Rc::new(value));
        }

        Ok(map)
    }
}
