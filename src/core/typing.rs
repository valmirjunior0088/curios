use {
    super::{Atom, Context, Proj, Subterm, Term, Type},
    crate::Span,
    std::fmt,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Preempted;

#[derive(Debug)]
pub enum Error {
    ReducePreempted {
        term: Box<Subterm>,
    },
    ConvertPreempted {
        this: Box<Subterm>,
        that: Box<Subterm>,
    },
    TypeMismatch {
        term: Box<Subterm>,
        inferred: Box<Subterm>,
        expected: Box<Subterm>,
    },
    NotAFunction {
        term: Box<Subterm>,
        head_type: Box<Subterm>,
    },
    NotATuple {
        term: Box<Subterm>,
        head_type: Box<Subterm>,
    },
    TupleIndexOutOfBounds {
        term: Box<Subterm>,
        index: usize,
        arity: usize,
    },
    NotAnAtomType {
        term: Box<Subterm>,
        head_type: Box<Subterm>,
    },
    NotNatType {
        term: Box<Subterm>,
        head_type: Box<Subterm>,
    },
    NotBlnType {
        term: Box<Subterm>,
        head_type: Box<Subterm>,
    },
    WrongNumberOfArguments {
        term: Box<Subterm>,
        expected: usize,
        got: usize,
    },
    MatchArityMismatch {
        term: Box<Subterm>,
        expected: usize,
        got: usize,
    },
    MatchCaseMissing {
        term: Box<Subterm>,
        atom: Atom,
    },
    CannotInferLiteral {
        term: Box<Subterm>,
    },
    UnboundVariable {
        term: Box<Subterm>,
    },
    CannotInfer {
        term: Box<Subterm>,
    },
    Located {
        span: Span,
        error: Box<Error>,
    },
}

impl Error {
    pub fn reduce_preempted<T: Into<Subterm>>(term: T) -> Self {
        Self::ReducePreempted {
            term: Box::new(term.into()),
        }
    }

    pub fn convert_preempted<T: Into<Subterm>, U: Into<Subterm>>(this: T, that: U) -> Self {
        Self::ConvertPreempted {
            this: Box::new(this.into()),
            that: Box::new(that.into()),
        }
    }

    pub fn type_mismatch<T: Into<Subterm>, U: Into<Subterm>, V: Into<Subterm>>(
        term: T,
        inferred: U,
        expected: V,
    ) -> Self {
        Self::TypeMismatch {
            term: Box::new(term.into()),
            inferred: Box::new(inferred.into()),
            expected: Box::new(expected.into()),
        }
    }

    pub fn not_a_function<T: Into<Subterm>, U: Into<Subterm>>(term: T, head_type: U) -> Self {
        Self::NotAFunction {
            term: Box::new(term.into()),
            head_type: Box::new(head_type.into()),
        }
    }

    pub fn not_a_tuple<T: Into<Subterm>, U: Into<Subterm>>(term: T, head_type: U) -> Self {
        Self::NotATuple {
            term: Box::new(term.into()),
            head_type: Box::new(head_type.into()),
        }
    }

    pub fn tuple_index_out_of_bounds<T: Into<Subterm>>(
        term: T,
        index: usize,
        arity: usize,
    ) -> Self {
        Self::TupleIndexOutOfBounds {
            term: Box::new(term.into()),
            index,
            arity,
        }
    }

    pub fn not_an_atom_type<T: Into<Subterm>, U: Into<Subterm>>(term: T, head_type: U) -> Self {
        Self::NotAnAtomType {
            term: Box::new(term.into()),
            head_type: Box::new(head_type.into()),
        }
    }

    pub fn not_nat_type<T: Into<Subterm>, U: Into<Subterm>>(term: T, head_type: U) -> Self {
        Self::NotNatType {
            term: Box::new(term.into()),
            head_type: Box::new(head_type.into()),
        }
    }

    pub fn not_bln_type<T: Into<Subterm>, U: Into<Subterm>>(term: T, head_type: U) -> Self {
        Self::NotBlnType {
            term: Box::new(term.into()),
            head_type: Box::new(head_type.into()),
        }
    }

    pub fn wrong_number_of_arguments<T: Into<Subterm>>(
        term: T,
        expected: usize,
        got: usize,
    ) -> Self {
        Self::WrongNumberOfArguments {
            term: Box::new(term.into()),
            expected,
            got,
        }
    }

    pub fn match_arity_mismatch<T: Into<Subterm>>(term: T, expected: usize, got: usize) -> Self {
        Self::MatchArityMismatch {
            term: Box::new(term.into()),
            expected,
            got,
        }
    }

    pub fn match_case_missing<T: Into<Subterm>, A: Into<Atom>>(term: T, atom: A) -> Self {
        Self::MatchCaseMissing {
            term: Box::new(term.into()),
            atom: atom.into(),
        }
    }

    pub fn cannot_infer_literal<T: Into<Subterm>>(term: T) -> Self {
        Self::CannotInferLiteral {
            term: Box::new(term.into()),
        }
    }

    pub fn unbound_variable<T: Into<Subterm>>(var: T) -> Self {
        Self::UnboundVariable {
            term: Box::new(var.into()),
        }
    }

    pub fn cannot_infer<T: Into<Subterm>>(term: T) -> Self {
        Self::CannotInfer {
            term: Box::new(term.into()),
        }
    }

    pub fn at(self, span: Span) -> Self {
        match self {
            Self::Located { .. } => self,
            error => Self::Located {
                span,
                error: Box::new(error),
            },
        }
    }

    pub fn format(&self, source: &str) -> String {
        match self {
            Self::Located { span, error } => {
                format!("{error}\n\n{}", span.render_snippet(source))
            }
            error => error.to_string(),
        }
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
            Error::TypeMismatch {
                inferred, expected, ..
            } => {
                write!(
                    f,
                    "type mismatch\n  inferred: {inferred}\n  expected: {expected}"
                )
            }
            Error::NotAFunction { head_type, .. } => {
                write!(f, "applied a non-function\n  head has type: {head_type}")
            }
            Error::NotATuple { head_type, .. } => {
                write!(
                    f,
                    "projected from a non-tuple\n  head has type: {head_type}"
                )
            }
            Error::TupleIndexOutOfBounds { index, arity, .. } => {
                write!(f, "tuple index {index} out of bounds (arity {arity})")
            }
            Error::NotAnAtomType { head_type, .. } => {
                write!(
                    f,
                    "matched on a non-atom type\n  head has type: {head_type}"
                )
            }
            Error::NotNatType { head_type, .. } => {
                write!(f, "expected Nat but got {head_type}")
            }
            Error::NotBlnType { head_type, .. } => {
                write!(f, "expected Bool but got {head_type}")
            }
            Error::WrongNumberOfArguments { expected, got, .. } => {
                write!(
                    f,
                    "wrong number of arguments: expected {expected}, got {got}"
                )
            }
            Error::MatchArityMismatch { expected, got, .. } => {
                write!(f, "match has {got} case(s) but atom type has {expected}")
            }
            Error::MatchCaseMissing { term, atom } => {
                write!(f, "missing match case for atom '{atom}': {term}")
            }
            Error::CannotInferLiteral { .. } => {
                write!(f, "cannot infer type of literal (add an annotation)")
            }
            Error::UnboundVariable { term } => {
                write!(f, "unbound variable: {term}")
            }
            Error::CannotInfer { .. } => {
                write!(f, "cannot infer type of expression")
            }
            Error::Located { error, .. } => {
                write!(f, "{error}")
            }
        }
    }
}

pub fn reduce_with(context: &mut Context, term: &Term) -> Result<Term, Error> {
    super::reduce(context, term.clone()).map_err(|Preempted| Error::reduce_preempted(term.clone()))
}

pub fn convert_with(context: &mut Context, this: &Term, that: &Term) -> Result<bool, Error> {
    super::convert(context, &Type.into(), this, that)
        .map_err(|Preempted| Error::convert_preempted(this.clone(), that.clone()))
}

pub fn expect(
    context: &mut Context,
    term: &Term,
    inferred: &Term,
    expected: &Term,
) -> Result<(), Error> {
    match convert_with(context, inferred, expected)? {
        true => Ok(()),
        false => Err(Error::type_mismatch(
            term.clone(),
            inferred.clone(),
            expected.clone(),
        )),
    }
}

pub fn refine_head(context: &mut Context, head: &Term, value: &Term) -> Result<(), Error> {
    let canonical = reduce_with(context, head)?;

    match &*canonical {
        Subterm::Var(var) => context.define(var.unwrap(), value),
        Subterm::Proj(Proj { head: base, index }) => {
            context.define_proj(base.clone(), *index, value.clone())
        }
        _ => {}
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::{Nat, Prim, Subterm};

    #[test]
    fn display_unbound_variable() {
        let err = Error::unbound_variable(Subterm::Prim(Prim::NatType));
        assert_eq!(err.to_string(), "unbound variable: Nat");
    }

    #[test]
    fn display_not_a_function() {
        let err = Error::not_a_function(
            Subterm::Prim(Prim::Nat(Nat::new(0))),
            Subterm::Prim(Prim::NatType),
        );
        assert_eq!(
            err.to_string(),
            "applied a non-function\n  head has type: Nat"
        );
    }

    #[test]
    fn display_type_mismatch_shows_both_types() {
        let err = Error::type_mismatch(
            Subterm::Prim(Prim::Nat(Nat::new(5))),
            Subterm::Prim(Prim::NatType),
            Subterm::Prim(Prim::BlnType),
        );
        let s = err.to_string();
        assert!(s.contains("Nat"), "should contain inferred Nat: {s}");
        assert!(s.contains("Bln"), "should contain expected Bln: {s}");
    }
}
