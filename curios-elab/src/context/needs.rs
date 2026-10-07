//! What an attempt at a declaration read of declarations that had not finished.
//!
//! A unit's items are elaborated on demand, each after the declarations it turns out to read. What makes that work is kept here: which of the unit's declarations have not finished, which one is being attempted, and what the attempt read of the others before they had finished. Such a read voids the attempt whatever it goes on to conclude — the record is what voids it, so no site that turns an error into a decision can make a void attempt count — and the unit's demand elaborates what was needed and attempts again.
//!
//! None of this exists where nothing is being elaborated: a context that only reads finished terms has no unfinished declaration to need.

use {
    curios_core::{Free, Global},
    std::{cell::RefCell, collections::BTreeSet, mem},
};

/// Which of a unit's declarations have not finished, and what the attempt under way needed of them.
#[derive(Debug, Default)]
pub(crate) struct Needs {
    /// The names of the unit's declarations that have not finished elaborating.
    unfinished: BTreeSet<Global>,
    /// The names the attempt under way declares, which it reads as its own.
    attempting: BTreeSet<Global>,
    /// The declarations the attempt under way read before they finished, each by a name it declares. A `RefCell` because a lookup records through a shared borrow.
    needed: RefCell<BTreeSet<Global>>,
}

impl Needs {
    /// Begin a unit's items: the names its own items declare, none finished yet.
    pub(crate) fn begin(&mut self, declared: BTreeSet<Global>) {
        self.unfinished = declared;
    }

    /// Begin an attempt at the declaration declaring `names`, with nothing needed yet.
    pub(crate) fn attempt(&mut self, names: &[&Global]) {
        self.attempting = names.iter().map(|name| **name).collect();
        self.needed.get_mut().clear();
    }

    /// A name the attempt under way declares, where it declares one: the entry's attempt declares none.
    pub(crate) fn attempted(&self) -> Option<&Global> {
        self.attempting.iter().next()
    }

    /// Whether the attempt under way declares `name`, and so reads it as its own.
    pub(crate) fn attempts(&self, name: &Global) -> bool {
        self.attempting.contains(name)
    }

    /// The declaration declaring `names` has finished — kept, refused or withheld — and nothing waits on it any more.
    pub(crate) fn finish(&mut self, names: &[&Global]) {
        for name in names {
            self.unfinished.remove(name);
        }
        self.attempting.clear();
    }

    /// Record that the attempt under way read the declaration declaring `name` before it finished. The attempt is void whatever it goes on to conclude, and is made again once that declaration has elaborated.
    pub(crate) fn need(&self, name: Global) {
        self.needed.borrow_mut().insert(name);
    }

    /// What the attempt under way needed, taken: empty for an attempt that read nothing unfinished, the one whose outcome stands.
    pub(crate) fn take(&mut self) -> BTreeSet<Global> {
        mem::take(self.needed.get_mut())
    }

    /// Whether the attempt under way has read nothing unfinished so far.
    pub(crate) fn nothing(&self) -> bool {
        self.needed.borrow().is_empty()
    }

    /// Record a need where `name` is declared by a declaration of the unit that has not finished, other than the one being attempted. Asked where a lookup of a global comes back empty, for its type or for its body: the answer is then not that the name means nothing or reduces no further, only that its declaration has not elaborated.
    pub(crate) fn missed(&self, name: &Free) {
        if let Free::Global(global) = name {
            self.reads(global);
        }
    }

    /// Record a need where `name` keys a registry entry of a declaration of the unit that has not finished, other than the one being attempted. Every entry of a unit is registered as lowered before its first item, so one read before its declaration has elaborated is not the entry the declaration publishes: the read is answered, and the attempt that made it is void, as one that missed a name is.
    pub(crate) fn reads(&self, name: &Global) {
        if self.unfinished.contains(name) && !self.attempting.contains(name) {
            self.need(*name);
        }
    }
}
