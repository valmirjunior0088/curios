use {super::Term, num_bigint::BigUint};

/// The base a numeric literal was written in (`0x` hex, `0b` binary, or plain decimal). Purely presentational: retained through the surface tree so the printer round-trips the written form, dropped at lowering to core.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Radix {
    Dec,
    Hex,
    Bin,
}

/// The written form of a `Nat` literal's magnitude: a numeral with its radix, or a character literal `'c'` (a fixed codepoint). The distinction is purely presentational — lowering takes the numeric value either way — but keeping it lets the printer round-trip `0xC2` and `'x'` as written.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NatLiteral {
    Number(BigUint, Radix),
    Char(char),
}

impl NatLiteral {
    pub(crate) fn number(n: impl Into<BigUint>) -> Self {
        NatLiteral::Number(n.into(), Radix::Dec)
    }
}

/// A `Nat` primitive value in the literal-stride shape core uses: `Zero`, or `Succ(k, t)` — `k` successors applied to a base term `t`. A plain numeral is `Succ(n, Zero)`; the prelude's `Nat/succ(a)` body is `Succ(1, a)`. Mirrors `curios_core::Nat`, with the stride kept as a [`NatLiteral`] so the printer round-trips the written radix or character form.
#[derive(Debug, Clone, PartialEq)]
pub enum Nat {
    Zero,
    Succ(NatLiteral, Term),
}
