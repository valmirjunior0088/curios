use curios_num::Natural;

/// The base a numeric literal was written in (`0x` hex, `0b` binary, or plain decimal), with the number of digits it was written with. Purely presentational: retained through the surface tree so the printer round-trips the written form, dropped at lowering to core.
///
/// **The width is part of the written form, and dropping it would narrow a table.** A padded numeral means something to a reader — `x[0x00, 0x48, 0x69]` is a byte table whose columns line up — and printing every literal at its natural width would rewrite it to `x[0x0, 0x48, 0x69]`. The count is of digits after any prefix, and `0` is a literal nobody wrote, which prints at its natural width: `Radix::synthesized` — crate-internal, so named here rather than linked — is how a desugar spells one.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Radix {
    Dec(usize),
    Hex(usize),
    Bin(usize),
}

/// The written form of a `Nat` literal's magnitude: a numeral together with its radix. Character literals are syntax-owned [`crate::ProofLiteral::Char`] values rather than natural-number spellings.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NatLiteral(pub Natural, pub Radix);
