//! The small-canonical immediate layout: how a packed value short enough to ride an `i31` lays its length and payload out in that i31's thirty-one bits.
//!
//! One representation contract, and five emitters read it — `rope_emitter`'s box and norm helpers, `code_emitter`'s length, element, append and chunk lowerings, and the two compile-time packers in `module_emitter` and `expr_emitter`. They must agree exactly: a site that packed a small value differently would mint the one non-canonical small value in the program, and the immediate equality — one instruction, on the promise that a given packed value has one representation — would answer false against it. The layout was a `match grain` at each of those sites, in two mutually inverted spellings of the same reciprocal, which is a contract held by transcription.
//!
//! **Two constants are free; the rest are consequences.** `envelope` and `len_shift` are decided, and the payload mask, the payload byte count, the element mask and the element stride follow from them and the grain. Deriving them is what makes a disagreement unspellable rather than merely unlikely, and [`ImmediateLayout::of`] asserts the two invariants that relate the free pair: the payload fits below the length field, and the length field is wide enough to count the envelope.

use curios_utilities::Grain;

/// One grain's immediate layout. Every quantity is in the units a Wasm instruction takes them in, so a caller reads fields rather than converting.
pub(crate) struct ImmediateLayout {
    grain: Grain,
    /// The longest value, in grain elements, that still fits an immediate. A value past it is a rope.
    pub(crate) envelope: i32,
    /// Where the length field starts. The payload occupies the bits below it, the length those from it up to bit 30 — the top of the `i31`.
    pub(crate) len_shift: i32,
}

impl ImmediateLayout {
    pub(crate) fn of(grain: Grain) -> Self {
        // The byte grain's length rides the top 2 payload bits over up to 3 bytes, the bit grain's the top 5 over up to 26 bits, both LSB-first.
        let (envelope, len_shift) = match grain {
            Grain::X => (3, 29),
            Grain::B => (26, 26),
        };
        let layout = Self {
            grain,
            envelope,
            len_shift,
        };
        debug_assert!(
            layout.payload_bits() <= len_shift,
            "the payload must fit below the length field"
        );
        debug_assert!(
            envelope < (1 << (31 - len_shift)),
            "the length field must count the whole envelope"
        );
        layout
    }

    /// Bits per element: the distance between one element's slot and the next.
    pub(crate) fn stride(&self) -> i32 {
        self.grain.bits() as i32
    }

    /// Elements per payload byte — the reciprocal of [`ImmediateLayout::stride`], which turns a byte slot back into the length's own units.
    pub(crate) fn unit(&self) -> i32 {
        8 / self.stride()
    }

    /// One element's mask, for the wrap an element is written through.
    pub(crate) fn elem_mask(&self) -> i32 {
        (1 << self.stride()) - 1
    }

    /// The whole payload's mask, which is what keeps the length field from bleeding into an extracted byte.
    pub(crate) fn payload_mask(&self) -> i32 {
        (1 << self.payload_bits()) - 1
    }

    /// How many payload bytes the immediate can occupy — the ceiling, since the bit grain's last byte is partial.
    pub(crate) fn slots(&self) -> i32 {
        (self.payload_bits() as u32).div_ceil(8) as i32
    }

    fn payload_bits(&self) -> i32 {
        self.envelope * self.stride()
    }

    /// Pack a whole constant value, known at compile time, into its immediate: the length in its field, the packed bytes LSB-first below it.
    ///
    /// The compile-time twin of `rope_emitter`'s `norm` helper, and the reason the two compile-time packers do not each spell the fold: they are one instruction sequence apart — a global initializer against an inline literal — and identical in what they compute.
    pub(crate) fn pack(&self, length: usize, bytes: &[u8]) -> i32 {
        debug_assert!(length <= self.envelope as usize, "the value must fit");
        bytes.iter().enumerate().fold(
            (length as i32) << self.len_shift,
            |packed, (index, &byte)| packed | (byte as i32) << (8 * index),
        )
    }

    /// Whether a value of this length rides an immediate rather than a rope.
    pub(crate) fn holds(&self, length: usize) -> bool {
        length <= self.envelope as usize
    }
}

#[cfg(test)]
mod tests;
