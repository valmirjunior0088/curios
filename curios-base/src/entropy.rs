use std::{cell::Cell, marker::PhantomData};

/// What a tick of [`Entropy`] produces from the raw counter value.
pub trait Mint {
    /// Builds the minted value from the raw counter value. Must be injective — distinct counter values must yield distinct values — or [`Entropy::fresh`]'s freshness guarantee silently breaks; the `name!` macro's prefixed form satisfies this by embedding the counter in the spelling.
    fn mint(entropy: usize) -> Self;
}

impl Mint for usize {
    fn mint(entropy: usize) -> Self {
        entropy
    }
}

/// A deterministic gensym source: a monotonic counter, `Cell`-backed so that
/// fresh values can be minted under shared borrows. `T` is what one tick
/// mints — raw `usize` ids by default, or any name type implementing [`Mint`].
#[derive(Debug)]
pub struct Entropy<T = usize> {
    counter: Cell<usize>,
    marker: PhantomData<T>,
}

impl<T> Default for Entropy<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Entropy<T> {
    /// An entropy source with its counter at zero — the first [`Entropy::fresh`] mints from raw value `0`.
    pub fn new() -> Self {
        Self {
            counter: Cell::new(0),
            marker: PhantomData,
        }
    }

    /// Raise the minting floor: every value handed out from now on is built
    /// from a raw counter `>= floor`.
    pub fn seed(&self, floor: usize) {
        self.counter.set(self.counter.get().max(floor));
    }

    /// The next raw counter value — i.e. how many ticks have been minted.
    pub fn count(&self) -> usize {
        self.counter.get()
    }
}

impl<T: Mint> Entropy<T> {
    /// Mints the next value and advances the counter — every call returns a value no previous call has returned. Takes `&self` (the counter is a `Cell`), which is the whole point: freshness stays available while the entropy source is shared immutably through a pass.
    pub fn fresh(&self) -> T {
        let entropy = self.counter.get();
        self.counter.set(entropy + 1);

        T::mint(entropy)
    }
}
