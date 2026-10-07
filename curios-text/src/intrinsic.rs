use {
    super::Term,
    curios_num::{Floating, Grain},
};

/// One entry of a list literal `[a, ..xs, b]` — a plain element, or a `..`-spread whose term contributes a whole `List` run. Lowering groups consecutive elements into literal chunks and splices the spreads with the n-ary `ListConcat` intrinsic; a spread-free literal lowers to a plain `List`.
#[derive(Debug, Clone, PartialEq)]
pub enum ListEntry {
    Elem(Term),
    Spread(Term),
}

/// One entry of a `Bytes` literal `x[\00, byte, ..bytes, \01]` — a run of escaped literal bytes, a term contributing one atom, or a `..`-spread whose term contributes a whole `Bytes` run. Entries are comma-delimited, so an atom or spread operand is an arbitrary term.
#[derive(Debug, Clone, PartialEq)]
pub enum BinSegment {
    /// A single atom given by a term: a `Bool` under `Grain::B`, a `Byte` under `Grain::X`. The free monoid's generator, where [`BinSegment::Spread`] carries a whole element. A constant atom is an ordinary numeral term — `Byte` and `Bool` both realize numerals — and lowering folds adjacent constant atoms into one packed run, so the AST needs no constant-run variant of its own.
    Atom(Term),
    Spread(Term),
}

/// The literals the parser writes down: the two truth values, a decimal, and the packed and list literals with their segments and spreads as written. Every other intrinsic — a type former, an operation, a literal no source spells — is core's own [`curios_core::Intrinsic`] over surface operands, [`Subterm::Operation`](super::Subterm::Operation), which only the `/sys` roster builds.
#[derive(Debug, Clone, PartialEq)]
pub enum Intrinsic {
    Bool(bool),
    Flt(Floating),
    Bin(Grain, Vec<BinSegment>),
    List(Vec<ListEntry>),
}
