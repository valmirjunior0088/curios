use {super::Term, curios_num::Natural};

/// The base a numeric literal was written in (`0x` hex, `0b` binary, or plain decimal), with the number of digits it was written with. Purely presentational: retained through the surface tree so the printer round-trips the written form, dropped at lowering to core.
///
/// **The width is part of the written form, and dropping it narrowed a table.** A padded numeral means something to a reader — `x[0x00, 0x48, 0x69]` is a byte table whose columns line up — and printing every literal at its natural width rewrote it to `x[0x0, 0x48, 0x69]`. The count is of digits after any prefix, and `0` is a literal nobody wrote, which prints at its natural width: [`Radix::synthesized`] is how a desugar spells one.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Radix {
    Dec(usize),
    Hex(usize),
    Bin(usize),
}

impl Radix {
    /// Plain decimal at its natural width — the base of every numeral the compiler mints rather than reads.
    pub(crate) fn synthesized() -> Self {
        Radix::Dec(0)
    }
}

/// The written form of a `Nat` literal's magnitude: a numeral together with its radix. Character literals are syntax-owned [`crate::Syn::Char`] values rather than natural-number spellings.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NatLiteral(pub Natural, pub Radix);

impl NatLiteral {
    pub(crate) fn number(n: impl Into<Natural>) -> Self {
        NatLiteral(n.into(), Radix::synthesized())
    }
}

/// A `Nat` intrinsic value in the literal-stride shape core uses: `Zero`, or `Succ(k, t)` — `k` successors applied to a base term `t`. A plain numeral is `Succ(n, Zero)`; the prelude's `Nat/succ(a)` body is `Succ(1, a)`. Mirrors `curios_core::Nat`, with the stride kept as a [`NatLiteral`] so the printer round-trips the written radix.
#[derive(Debug, Clone, PartialEq)]
pub enum Nat {
    Zero,
    Succ(NatLiteral, Term),
}
