//! What the suites of `totality` share: the matrices and shapes they compose.

use {
    crate::totality::*,
    curios_core::{
        Applications, CallMatrix, Free, Global, InductDecl, Polarity, Size, StructDecl, Term,
    },
};

/// An [`Driver`] that reduces nothing, for the two probes below that build a [`Walk`] by hand.
///
/// Not a checker and not a mock of one. Those probes assert *traversal* structure — that a peeled prefix keeps its binder tail, and that a deep body walks to its bottom on the default test stack — over fixtures already in normal form, so "reduce to weak-head normal form" is the identity on every term they contain. Anything that needed real reduction would be asking a typing question, and those live under `tests/driven/`, driven by the actual certifier.
///
/// They cannot be integration tests: [`Walk`] is an internal whose fields they set directly, and making it constructible from outside would mean publishing every one of them.
#[derive(Default)]
pub struct Unreducing {
    pub minted: std::cell::Cell<u32>,
}

impl Driver for Unreducing {
    type Error = std::convert::Infallible;

    fn force(&mut self, term: &Term) -> Result<Term, Self::Error> {
        Ok(term.clone())
    }

    fn assumption(&self, _: &Free) -> Option<&Term> {
        None
    }

    fn is_local(&self, _: &Free) -> bool {
        false
    }

    fn fresh(&mut self, hint: Option<&str>) -> Free {
        let index = self.minted.get();
        self.minted.set(index + 1);
        Free::local(1_000 + u64::from(index), hint)
    }

    fn unfold(&self, _: &Free) -> Option<&Term> {
        None
    }

    fn body(&self, _: &Global) -> Option<&Term> {
        None
    }

    fn induct_decl(&self, _: &Global) -> Option<&InductDecl> {
        None
    }

    fn struct_decl(&self, _: &Global) -> Option<&StructDecl> {
        None
    }

    fn polarity(&self, _: &Global, _: usize) -> Polarity {
        Polarity::Mixed
    }

    fn judged(&self, _: &Global) -> bool {
        false
    }

    fn applications(&self, _: &Global) -> Option<Applications> {
        None
    }

    fn refined(&self) -> Vec<(Term, Term)> {
        Vec::new()
    }
}

/// One graded call, from member `caller` to member `callee`.
pub fn call(caller: usize, callee: usize, matrix: CallMatrix) -> Call {
    Call {
        caller,
        callee,
        matrix,
    }
}

/// Build a matrix from a row-major grid of size grades.
pub fn matrix(rows: &[&[Size]]) -> CallMatrix {
    let columns = rows.first().map_or(0, |row| row.len());
    let mut matrix = CallMatrix::unknown(rows.len(), columns);
    for (row, grades) in rows.iter().enumerate() {
        for (column, grade) in grades.iter().enumerate() {
            matrix.set(row, column, *grade);
        }
    }

    matrix
}

pub const LESS: Size = Size::Less;

pub const SAME: Size = Size::Same;

pub const NONE: Size = Size::Unknown;
