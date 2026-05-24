use {
    super::{Atom, Term},
    std::fmt,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Preempted;

#[derive(Debug)]
pub enum Error {
    ReducePreempted { term: Term },
    ConvertPreempted { this: Term, that: Term },
    TypeMismatch { term: Term, inferred: Term, expected: Term },
    NotAFunction { term: Term, head_type: Term },
    NotATuple { term: Term, head_type: Term },
    TupleIndexOutOfBounds { term: Term, index: usize, arity: usize },
    NotAnAtomType { term: Term, head_type: Term },
    NotNatType { term: Term, head_type: Term },
    NotBlnType { term: Term, head_type: Term },
    MatchArityMismatch { term: Term, expected: usize, got: usize },
    MatchCaseMissing { term: Term, atom: Atom },
    CannotInferLiteral { term: Term },
    UnboundVariable { term: Term },
    CannotInfer { term: Term },
}

impl Error {
    pub fn reduce_preempted<T: Into<Term>>(term: T) -> Self {
        Self::ReducePreempted { term: term.into() }
    }

    pub fn convert_preempted<T: Into<Term>, U: Into<Term>>(this: T, that: U) -> Self {
        Self::ConvertPreempted { this: this.into(), that: that.into() }
    }

    pub fn type_mismatch<T: Into<Term>, U: Into<Term>, V: Into<Term>>(
        term: T,
        inferred: U,
        expected: V,
    ) -> Self {
        Self::TypeMismatch {
            term: term.into(),
            inferred: inferred.into(),
            expected: expected.into(),
        }
    }

    pub fn not_a_function<T: Into<Term>, U: Into<Term>>(term: T, head_type: U) -> Self {
        Self::NotAFunction { term: term.into(), head_type: head_type.into() }
    }

    pub fn not_a_tuple<T: Into<Term>, U: Into<Term>>(term: T, head_type: U) -> Self {
        Self::NotATuple { term: term.into(), head_type: head_type.into() }
    }

    pub fn tuple_index_out_of_bounds<T: Into<Term>>(term: T, index: usize, arity: usize) -> Self {
        Self::TupleIndexOutOfBounds { term: term.into(), index, arity }
    }

    pub fn not_an_atom_type<T: Into<Term>, U: Into<Term>>(term: T, head_type: U) -> Self {
        Self::NotAnAtomType { term: term.into(), head_type: head_type.into() }
    }

    pub fn not_nat_type<T: Into<Term>, U: Into<Term>>(term: T, head_type: U) -> Self {
        Self::NotNatType { term: term.into(), head_type: head_type.into() }
    }

    pub fn not_bln_type<T: Into<Term>, U: Into<Term>>(term: T, head_type: U) -> Self {
        Self::NotBlnType { term: term.into(), head_type: head_type.into() }
    }

    pub fn match_arity_mismatch<T: Into<Term>>(term: T, expected: usize, got: usize) -> Self {
        Self::MatchArityMismatch { term: term.into(), expected, got }
    }

    pub fn match_case_missing<T: Into<Term>, A: Into<Atom>>(term: T, atom: A) -> Self {
        Self::MatchCaseMissing { term: term.into(), atom: atom.into() }
    }

    pub fn cannot_infer_literal<T: Into<Term>>(term: T) -> Self {
        Self::CannotInferLiteral { term: term.into() }
    }

    pub fn unbound_variable<T: Into<Term>>(var: T) -> Self {
        Self::UnboundVariable { term: var.into() }
    }

    pub fn cannot_infer<T: Into<Term>>(term: T) -> Self {
        Self::CannotInfer { term: term.into() }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::ReducePreempted { term } => {
                write!(f, "reduction preempted on: {term}")
            }
            Error::ConvertPreempted { this, that } => {
                write!(f, "conversion preempted between {this} and {that}")
            }
            Error::TypeMismatch { term, inferred, expected } => {
                write!(
                    f,
                    "type mismatch in {term}\n  inferred: {inferred}\n  expected: {expected}"
                )
            }
            Error::NotAFunction { term, head_type } => {
                write!(
                    f,
                    "applied a non-function: {term}\n  head has type: {head_type}"
                )
            }
            Error::NotATuple { term, head_type } => {
                write!(
                    f,
                    "projected from a non-tuple: {term}\n  head has type: {head_type}"
                )
            }
            Error::TupleIndexOutOfBounds { term, index, arity } => {
                write!(f, "tuple index {index} out of bounds (arity {arity}): {term}")
            }
            Error::NotAnAtomType { term, head_type } => {
                write!(
                    f,
                    "matched on a non-atom type: {term}\n  head has type: {head_type}"
                )
            }
            Error::NotNatType { term, head_type } => {
                write!(f, "expected Nat but got {head_type}\n  in: {term}")
            }
            Error::NotBlnType { term, head_type } => {
                write!(f, "expected Bool but got {head_type}\n  in: {term}")
            }
            Error::MatchArityMismatch { term, expected, got } => {
                write!(
                    f,
                    "match has {got} case(s) but atom type has {expected}: {term}"
                )
            }
            Error::MatchCaseMissing { term, atom } => {
                write!(f, "missing match case for atom '{atom}': {term}")
            }
            Error::CannotInferLiteral { term } => {
                write!(
                    f,
                    "cannot infer type of literal (add an annotation): {term}"
                )
            }
            Error::UnboundVariable { term } => {
                write!(f, "unbound variable: {term}")
            }
            Error::CannotInfer { term } => {
                write!(f, "cannot infer type of: {term}")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::{Prim, Term};

    #[test]
    fn display_unbound_variable() {
        let err = Error::unbound_variable(Term::Prim(Prim::NatType));
        assert_eq!(err.to_string(), "unbound variable: Nat");
    }

    #[test]
    fn display_not_a_function() {
        let err = Error::not_a_function(Term::Prim(Prim::Nat(0)), Term::Prim(Prim::NatType));
        assert_eq!(
            err.to_string(),
            "applied a non-function: 0\n  head has type: Nat"
        );
    }

    #[test]
    fn display_type_mismatch_shows_both_types() {
        let err = Error::type_mismatch(
            Term::Prim(Prim::Nat(5)),
            Term::Prim(Prim::NatType),
            Term::Prim(Prim::BlnType),
        );
        let s = err.to_string();
        assert!(s.contains("Nat"), "should contain inferred Nat: {s}");
        assert!(s.contains("Bln"), "should contain expected Bln: {s}");
    }
}
