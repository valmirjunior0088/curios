use {
    super::{Dialect, IntrinsicOperand, Program, Subterm, Term},
    curios_num::Floating,
};

/// One entry of a list literal `[a, ..xs, b]` — a plain element, or a `..`-spread whose term contributes a whole `List` run. Lowering groups consecutive elements into literal chunks and splices the spreads with the n-ary `ListConcat` intrinsic; a spread-free literal lowers to a plain `List`.
#[derive(Debug, Clone, PartialEq)]
pub enum ListEntry<D: Dialect = Program> {
    Elem(Term<D>),
    Spread(Term<D>),
}

/// One entry of a `Bytes` literal `x[\00, byte, ..bytes, \01]` — a run of escaped literal bytes, a term contributing one atom, or a `..`-spread whose term contributes a whole `Bytes` run. Entries are comma-delimited, so an atom or spread operand is an arbitrary term.
#[derive(Debug, Clone, PartialEq)]
pub enum BytesSegment<D: Dialect = Program> {
    /// A single atom given by a term, a `Byte`: the free monoid's generator, where [`BytesSegment::Spread`] carries a whole element. A constant atom is an ordinary numeral term, which `Byte` realizes, and lowering folds adjacent constant atoms into one packed run, so the AST needs no constant-run variant of its own.
    Atom(Term<D>),
    Spread(Term<D>),
}

/// A literal as the parser writes it down: the two truth values, a decimal, and the packed and list literals with their segments and spreads as written, so each prints back as it was spelled. Lowering folds a packed literal's constant atoms into runs and splices its spreads; a numeral is a [`NumLit`](super::NumLit), whose carrier elaboration chooses.
#[derive(Debug, Clone, PartialEq)]
pub enum Literal<D: Dialect = Program> {
    Bool(bool),
    Flt(Floating),
    Bytes(Vec<BytesSegment<D>>),
    List(Vec<ListEntry<D>>),
}

/// One of core's intrinsics over surface operands: a type former, an operation, or a literal no source spells. The same enum the rest of the pipeline holds, at this stage's term — so no variant is restated here, and lowering one is lowering its operands.
///
/// Never parsed. The `/sys` roster plants one in a declaration's body, and reads the types core's table gives it into the declaration's signature, so a program reaches every intrinsic as an ordinary named binding rather than as syntax.
pub type Intrinsic<D = Program> = curios_core::Intrinsic<Term<D>>;

// What core's one table of intrinsics looks into, so `/sys`'s own trees are written by it as a report's terms are.
impl<D: Dialect> IntrinsicOperand for Term<D> {
    fn as_intrinsic(&self) -> Option<&Intrinsic<D>> {
        match self.as_subterm() {
            Subterm::Intrinsic(node) => Some(node),
            _ => None,
        }
    }

    fn into_intrinsic(self) -> Option<Intrinsic<D>> {
        match self.into_subterm() {
            Subterm::Intrinsic(node) => Some(node),
            _ => None,
        }
    }
}
