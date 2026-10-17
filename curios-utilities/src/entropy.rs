use std::{cell::Cell, marker::PhantomData};

/// What a tick of [`Entropy`] produces from the raw counter value.
pub trait Mint {
    /// Builds the minted value from the raw counter value. Must be injective — distinct counter values must yield distinct values — or [`Entropy::fresh`]'s freshness guarantee silently breaks; the `name!` macro's prefixed form satisfies this by embedding the counter in the spelling.
    fn mint(entropy: u64) -> Self;
}

impl Mint for u64 {
    fn mint(entropy: u64) -> Self {
        entropy
    }
}

/// An index into something held in memory: narrowed where it is minted, since no store indexed by one holds more entries than the address space has room for.
impl Mint for usize {
    fn mint(entropy: u64) -> Self {
        usize::try_from(entropy).expect("an index is counted within the address space")
    }
}

/// A deterministic gensym source: a monotonic counter, `Cell`-backed so that fresh values can be minted under shared borrows. `T` is what one tick mints — raw `u64` ids by default, or any name type implementing [`Mint`].
///
/// **The counter is a `u64` on every target**, so it counts alike in the playground and at the command line; the crate's README states why.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Entropy<T = u64> {
    counter: Cell<u64>,
    marker: PhantomData<T>,
}

impl<T> Entropy<T> {
    /// An entropy source with its counter at zero — the first [`Entropy::fresh`] mints from raw value `0`.
    pub fn new() -> Self {
        Self {
            counter: Cell::new(0),
            marker: PhantomData,
        }
    }

    /// Raise the minting floor: every value handed out from now on is built from a raw counter `>= floor`.
    pub fn seed(&self, floor: u64) {
        self.counter.set(self.counter.get().max(floor));
    }

    /// Start over from zero. Every value handed out from here on repeats one handed out before, which is the caller's to make meaningless: a counter kept per declaration is reset where one ends and the next begins.
    pub fn reset(&self) {
        self.counter.set(0);
    }

    /// The next raw counter value — i.e. how many ticks have been minted.
    pub fn count(&self) -> u64 {
        self.counter.get()
    }
}

impl<T: Mint> Entropy<T> {
    /// Mints the next value and advances the counter — every call returns a value no previous call has returned. Takes `&self` (the counter is a `Cell`), which is the whole point: freshness stays available while the entropy source is shared immutably through a pass.
    pub fn fresh(&self) -> T {
        let entropy = self.counter.get();
        // Unreachable by one tick at a time; checked, so the counter can never wrap and repeat a value.
        let next = entropy
            .checked_add(1)
            .expect("a counter advanced one tick at a time stays below 2^64");
        self.counter.set(next);

        T::mint(entropy)
    }
}

impl<T> Default for Entropy<T> {
    fn default() -> Self {
        Self::new()
    }
}
