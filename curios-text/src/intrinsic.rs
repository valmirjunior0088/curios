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

/// A literal as the parser writes it down: the two truth values, a decimal, and the packed and list literals with their segments and spreads as written, so each prints back as it was spelled. Lowering folds a packed literal's constant atoms into runs and splices its spreads; a numeral is a [`NumLit`](super::NumLit), whose carrier elaboration chooses.
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Bool(bool),
    Flt(Floating),
    Bin(Grain, Vec<BinSegment>),
    List(Vec<ListEntry>),
}

/// One of core's intrinsics over surface operands: a type former, an operation, or a literal no source spells. The same enum the rest of the pipeline holds, at this stage's term — so no variant is restated here, and lowering one is lowering its operands.
///
/// Never parsed. The `/sys` roster plants one in a declaration's body, and reads the types core's table gives it into the declaration's signature, so a program reaches every intrinsic as an ordinary named binding rather than as syntax.
pub type Intrinsic = curios_core::Intrinsic<Term>;
