//! What a construction costs, in units no target can move.
//!
//! A reduction budget that counts transitions prices a `Bool` fold and a half-megabyte concatenation the same, so nothing bounds the memory a reduction allocates. This module is the other half of that budget: the unit a construction is measured in, the formulas that measure one, and the constants those formulas are built from. Spending them is [`Reducer::spend`](crate::Reducer)'s job, and charging *before* allocating is the rule that makes the bound mean anything — a charge taken afterwards is a report, not a limit.
//!
//! # The unit
//!
//! One unit is **eight logical bytes of scalar payload, or one abstract reference slot**. A slot is one unit on every target; that is not a claim that a pointer occupies eight bytes, and no formula here consults `size_of`, an allocator's capacity, a resident-set size, or a bignum's platform-dependent limb layout.
//!
//! That independence is load-bearing rather than fastidious. `curios-js` compiles to wasm32, where `usize` and `num-bigint`'s digit width both differ from the native target, and the shipped budget promises that a program compiling in the playground compiles at the command line. Every figure here is therefore computed in `u64` regardless of host pointer width.
//!
//! Every formula takes a `u64` rather than a `usize`, including the ones whose caller has a `Vec::len()` in hand. A widening cast is safe on both targets and a narrowing one is not, and `usize` narrows on wasm32 — a charge computed through it would silently differ between the playground and the command line, which is the one thing this module exists to prevent.
//!
//! The same independence is what keeps the limit correct while a representation changes underneath it. A carrier that concatenates more cheaply spends fewer units for the same result; the unit itself, the formulas, and the budget are unaffected.
//!
//! # Saturation is refusal
//!
//! Arithmetic here saturates rather than wrapping, and a saturated [`Cost`] is refused outright instead of being compared against the budget. Saturating means the true cost is at least [`u64::MAX`] units — sixteen exabytes — which no budget affords and no allocation should be attempted for. That is what the specification's "an overflow is reported as exhaustion rather than wrapped, truncated, converted unsafely to `usize`, or passed to an allocator" comes to in code, and it is why these operations are total rather than fallible: a formula that cannot fail is a formula no caller can forget to check.
//!
//! The direction is the whole safety argument. A formula may overcharge and must never undercharge, so every rounding here is upward and every saturation lands on refusal.
//!
//! # The constants are conservative and are not layout
//!
//! The headers below are deliberately generous and are **not** claims about Rust's heap layout. What they have to be is an upper bound on the fixed cost of a thing, so that a program building a million small values is charged for a million small values rather than for the payload alone. Each is documented where it is defined, and the tests beside this module pin the formulas rather than the constants — a constant may rise without a test moving.

use std::{fmt, ops::Add};

/// What a refused charge was *for*.
///
/// Attribution without a second budget. One number still decides acceptance — the specification refuses independent transition and construction limits, and states why — and this says what that number was being spent on at the point it ran out, which is the difference between "your program is too big" and a diagnostic somebody can act on.
///
/// **Dominance is deliberately not promised.** This is the category of the *refused charge*, not of whatever consumed the most budget over the declaration; answering the second would need cumulative per-category accounting that has nothing to do with the refusal.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Category {
    /// A reducer transition: the loop moving once.
    Transition,
    /// Packed byte or bit payload.
    Payload,
    /// A big natural's or integer's limbs.
    Limbs,
    /// A list, vector, or argument store's retained slots.
    Slots,
    /// A term node and the children it retains.
    Reconstruction,
    /// A temporary buffer the reducer fills and drops.
    Buffer,
    /// One level of guarded reducer recursion, priced at the native frame it pushes.
    Depth,
    /// Priced construction with no more specific row.
    Construction,
}

impl fmt::Display for Category {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Category::Transition => "reduction steps",
            Category::Payload => "packed payload",
            Category::Limbs => "big-number limbs",
            Category::Slots => "collection slots",
            Category::Reconstruction => "term reconstruction",
            Category::Buffer => "a temporary buffer",
            Category::Depth => "recursion depth",
            Category::Construction => "construction",
        })
    }
}

/// A charge in logical units, and what it is for.
///
/// See the module documentation for the unit, and for why the arithmetic saturates instead of failing. The category rides along so a refusal can name it; it takes no part in the arithmetic beyond [`Cost::saturating_add`]'s rule for which of two survives.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Cost {
    units: u64,
    category: Category,
}

/// The fixed cost of a packed value or a big number: its length, its offset, and the handle to its buffer.
///
/// Four units rather than three because a `PackedBin` carries a buffer handle and two indices, and a charge that exactly matched today's field count would have to move whenever one is added.
const VALUE_HEADER: u64 = 4;

/// The fixed cost of a collection: its handle, its length, and its capacity.
const COLLECTION_HEADER: u64 = 4;

/// The fixed cost of a temporary buffer the reducer builds and drops. The same shape as a collection, priced separately because the price list distinguishes them and because a temporary is the row most likely to need its own constant later.
const BUFFER_HEADER: u64 = 4;

/// The fixed cost of a term node: its refcount, its cached scalars, and its discriminant, before any child slot.
///
/// One constant for every variant rather than a table. The price list says "fixed charge for its variant", and a single value at or above the largest variant satisfies that while giving nothing to forget: a new variant is priced correctly the day it is added, which a table would not be.
const TERM_NODE: u64 = 8;

impl Cost {
    /// No charge at all — the sharing case, where storage was reached again rather than built.
    pub const NOTHING: Self = Self::of(0, Category::Construction);

    /// One unit: the transition itself, which every reducer step already spends.
    pub const STEP: Self = Self::of(1, Category::Transition);

    /// A charge no budget affords, for a size that overflowed before anything was allocated.
    pub const REFUSED: Self = Self::of(u64::MAX, Category::Construction);

    /// One level of guarded reducer recursion, priced at the native frame it pushes.
    ///
    /// This is the row that brings *depth* inside the contract. `curios_utilities::recurse` grows the native stack rather than aborting, and nothing else bounds total depth, so a data-shaped walk takes real memory the transition counter never observed — one unit per level is already spent, and what this adds is a price commensurate with the stack that level takes.
    ///
    /// See `FRAME_UNITS` for the measurement and for which of the two figures it is.
    pub const FRAME: Self = Self::of(FRAME_UNITS, Category::Depth);

    const fn of(units: u64, category: Category) -> Self {
        Self { units, category }
    }

    /// A charge of exactly `units`, with no more specific row than construction.
    pub const fn units(units: u64) -> Self {
        Self::of(units, Category::Construction)
    }

    /// What this charge comes to, for a budget to subtract.
    pub const fn get(self) -> u64 {
        self.units
    }

    /// What this charge is for, for a refusal to name.
    pub const fn category(self) -> Category {
        self.category
    }

    /// Whether this charge saturated, and is therefore a refusal rather than a number.
    pub const fn is_refused(self) -> bool {
        self.units == u64::MAX
    }

    /// A packed byte string of `length` bytes: the value header, plus one unit per eight bytes, rounded up.
    pub fn packed_bytes(length: u64) -> Self {
        Self::of(VALUE_HEADER, Category::Payload)
            .saturating_add(Self::of(units_of(length, 8), Category::Payload))
    }

    /// A packed bit string of `length` bits: the value header, plus one unit per sixty-four bits, rounded up.
    pub fn packed_bits(length: u64) -> Self {
        Self::of(VALUE_HEADER, Category::Payload)
            .saturating_add(Self::of(units_of(length, 64), Category::Payload))
    }

    /// A big natural or integer of `bits` magnitude bits: the value header, plus one unit per base-2^64 logical limb, rounded up.
    ///
    /// Logical limbs, from the magnitude's bit length, rather than whatever `num-bigint` chose for this target — which is the whole reason this takes a bit count and not a limb count.
    pub fn big_number(bits: u64) -> Self {
        Self::of(VALUE_HEADER, Category::Limbs)
            .saturating_add(Self::of(units_of(bits, 64), Category::Limbs))
    }

    /// A list, vector, or argument store of `slots` retained references: the collection header, plus one unit per slot.
    ///
    /// The elements themselves are charged separately where they are *constructed*. A vector of already-existing terms retains references and builds nothing, which is what this row prices.
    pub fn collection(slots: u64) -> Self {
        Self::of(COLLECTION_HEADER.saturating_add(slots), Category::Slots)
    }

    /// A temporary reducer buffer of `slots` logical units of payload or slots: the buffer header, plus its request.
    pub fn buffer(slots: u64) -> Self {
        Self::of(BUFFER_HEADER.saturating_add(slots), Category::Buffer)
    }

    /// A term node retaining `slots` children or scalar fields: the node's fixed charge, plus one unit per slot.
    pub fn term(slots: u64) -> Self {
        Self::of(TERM_NODE.saturating_add(slots), Category::Reconstruction)
    }

    /// This charge and `other` together, saturating into [`Cost::REFUSED`].
    ///
    /// **The larger contributor keeps its category**, ties to the left. A composite charge — a value plus the two temporaries building it takes — should name the row that made it expensive, and "larger of the two" is the only rule here that is both deterministic and says something. It is not a claim about what dominated the *declaration*; see [`Category`].
    pub const fn saturating_add(self, other: Self) -> Self {
        Self {
            units: self.units.saturating_add(other.units),
            category: match other.units > self.units {
                true => other.category,
                false => self.category,
            },
        }
    }

    /// This charge `factor` times over, saturating into [`Cost::REFUSED`], keeping its own category.
    pub const fn saturating_mul(self, factor: u64) -> Self {
        Self {
            units: self.units.saturating_mul(factor),
            category: self.category,
        }
    }
}

/// `ceil(value / divisor)` in `u64`, which is where every rounding in this module happens and the one place it can be got backwards.
const fn units_of(value: u64, divisor: u64) -> u64 {
    value.div_ceil(divisor)
}

/// The measured native frame of one guarded reduction level, rounded up to a power of two.
///
/// **Measured, not guessed.** Taken 2026-08-15 on `aarch64-apple-darwin` by instrumenting `Kernel::spend` to record the address of a stack local, reducing a right-nested `Nat/add` chain at depths 100, 200, 400 and 800, and taking the median descent between consecutive observations inside one stack segment — the min/max span is useless here, because `recurse` moves the walk onto a fresh 32 MiB segment and the addresses stop being comparable across the jump. Every depth reported the same figure to the byte:
///
/// | Profile | Bytes per level | Units |
/// | --- | --- | --- |
/// | release | 7 264 | 908 |
/// | debug | 97 200 | 12 150 |
///
/// **The release figure is the one charged**, rounded up to 1024, and the debug figure is recorded rather than used. A charge has to be a property of the program rather than of the build that checked it — the same reason the unit is target-independent — and release is the compiler that ships. What the divergence means in practice is that a debug build takes about thirteen times the stack this charges for, which `recurse` absorbs by growing rather than aborting.
///
/// **It is a recipe rather than a probe, and that is a deliberate exception.** Retaking it needs six lines of temporary instrumentation inside `Kernel::spend`, and leaving those in the product to keep a probe reproducible would put a measurement hook on the hottest path in the trusted base. The recipe above is complete enough to retype in a few minutes.
const FRAME_UNITS: u64 = 1024;

impl Default for Cost {
    fn default() -> Self {
        Self::NOTHING
    }
}

impl Add for Cost {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        self.saturating_add(other)
    }
}

impl std::iter::Sum for Cost {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Self::NOTHING, Self::saturating_add)
    }
}

#[cfg(test)]
mod tests;
