use {super::Term, curios_num::Natural};

/// The base a numeric literal was written in (`0x` hex, `0b` binary, or plain decimal). Purely presentational: retained through the surface tree so the printer round-trips the written form, dropped at lowering to core.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Radix {
    Dec,
    Hex,
    Bin,
}

/// The written form of a `Nat` literal's magnitude: a numeral together with its radix. Character literals are syntax-owned [`crate::Syn::Char`] values rather than natural-number spellings.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NatLiteral(pub Natural, pub Radix);

impl NatLiteral {
    pub(crate) fn number(n: impl Into<Natural>) -> Self {
        NatLiteral(n.into(), Radix::Dec)
    }
}

/// A `Nat` intrinsic value in the literal-stride shape core uses: `Zero`, or `Succ(k, t)` — `k` successors applied to a base term `t`. A plain numeral is `Succ(n, Zero)`; the prelude's `Nat/succ(a)` body is `Succ(1, a)`. Mirrors `curios_elab::Nat`, with the stride kept as a [`NatLiteral`] so the printer round-trips the written radix.
#[derive(Debug, Clone, PartialEq)]
pub enum Nat {
    Zero,
    Succ(NatLiteral, Term),
}
