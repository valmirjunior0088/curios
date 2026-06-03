use {
    super::{Atom, Context, Proj, Subterm, Term},
    crate::Span,
    num_bigint::BigUint,
    std::fmt,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Preempted;

#[derive(Debug)]
pub enum Error {
    ReducePreempted {
        term: Box<Term>,
    },
    ConvertPreempted {
        this: Box<Term>,
        that: Box<Term>,
    },
    TypeMismatch {
        term: Box<Term>,
        inferred: Box<Term>,
        expected: Box<Term>,
    },
    NotAFunction {
        term: Box<Term>,
        head_type: Box<Term>,
    },
    NotAFunctionType {
        term: Box<Term>,
        expected: Box<Term>,
    },
    NotATuple {
        term: Box<Term>,
        head_type: Box<Term>,
    },
    NotATupleType {
        term: Box<Term>,
        expected: Box<Term>,
    },
    NotAnArrayType {
        term: Box<Term>,
        expected: Box<Term>,
    },
    NotArrType {
        term: Box<Term>,
        head_type: Box<Term>,
    },
    TupleArityMismatch {
        term: Box<Term>,
        expected: usize,
        got: usize,
    },
    TupleIndexOutOfBounds {
        term: Box<Term>,
        index: usize,
        arity: usize,
    },
    NotAnAtomType {
        term: Box<Term>,
        head_type: Box<Term>,
    },
    NotNatType {
        term: Box<Term>,
        head_type: Box<Term>,
    },
    NotBlnType {
        term: Box<Term>,
        head_type: Box<Term>,
    },
    WrongNumberOfArguments {
        term: Box<Term>,
        expected: usize,
        got: usize,
    },
    MatchArityMismatch {
        term: Box<Term>,
        expected: usize,
        got: usize,
    },
    MatchCaseMissing {
        term: Box<Term>,
        atom: Atom,
    },
    CannotInferLiteral {
        term: Box<Term>,
    },
    UnboundVariable {
        term: Box<Term>,
    },
    CannotInfer {
        term: Box<Term>,
    },
    NatOverflow {
        value: BigUint,
    },
    Located {
        span: Span,
        error: Box<Error>,
    },
}

impl Error {
    pub fn reduce_preempted<T: Into<Term>>(term: T) -> Self {
        Self::ReducePreempted {
            term: Box::new(term.into()),
        }
    }

    pub fn convert_preempted<T: Into<Term>, U: Into<Term>>(this: T, that: U) -> Self {
        Self::ConvertPreempted {
            this: Box::new(this.into()),
            that: Box::new(that.into()),
        }
    }

    pub fn type_mismatch<T: Into<Term>, U: Into<Term>, V: Into<Term>>(
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

    pub fn not_a_function<T: Into<Term>, U: Into<Term>>(term: T, head_type: U) -> Self {
        Self::NotAFunction {
            term: Box::new(term.into()),
            head_type: Box::new(head_type.into()),
        }
    }

    pub fn not_a_function_type<T: Into<Term>, U: Into<Term>>(term: T, expected: U) -> Self {
        Self::NotAFunctionType {
            term: Box::new(term.into()),
            expected: Box::new(expected.into()),
        }
    }

    pub fn not_a_tuple<T: Into<Term>, U: Into<Term>>(term: T, head_type: U) -> Self {
        Self::NotATuple {
            term: Box::new(term.into()),
            head_type: Box::new(head_type.into()),
        }
    }

    pub fn not_a_tuple_type<T: Into<Term>, U: Into<Term>>(term: T, expected: U) -> Self {
        Self::NotATupleType {
            term: Box::new(term.into()),
            expected: Box::new(expected.into()),
        }
    }

    pub fn not_an_array_type<T: Into<Term>, U: Into<Term>>(term: T, expected: U) -> Self {
        Self::NotAnArrayType {
            term: Box::new(term.into()),
            expected: Box::new(expected.into()),
        }
    }

    pub fn not_arr_type<T: Into<Term>, U: Into<Term>>(term: T, head_type: U) -> Self {
        Self::NotArrType {
            term: Box::new(term.into()),
            head_type: Box::new(head_type.into()),
        }
    }

    pub fn tuple_arity_mismatch<T: Into<Term>>(term: T, expected: usize, got: usize) -> Self {
        Self::TupleArityMismatch {
            term: Box::new(term.into()),
            expected,
            got,
        }
    }

    pub fn tuple_index_out_of_bounds<T: Into<Term>>(term: T, index: usize, arity: usize) -> Self {
        Self::TupleIndexOutOfBounds {
            term: Box::new(term.into()),
            index,
            arity,
        }
    }

    pub fn not_an_atom_type<T: Into<Term>, U: Into<Term>>(term: T, head_type: U) -> Self {
        Self::NotAnAtomType {
            term: Box::new(term.into()),
            head_type: Box::new(head_type.into()),
        }
    }

    pub fn not_nat_type<T: Into<Term>, U: Into<Term>>(term: T, head_type: U) -> Self {
        Self::NotNatType {
            term: Box::new(term.into()),
            head_type: Box::new(head_type.into()),
        }
    }

    pub fn not_bln_type<T: Into<Term>, U: Into<Term>>(term: T, head_type: U) -> Self {
        Self::NotBlnType {
            term: Box::new(term.into()),
            head_type: Box::new(head_type.into()),
        }
    }

    pub fn wrong_number_of_arguments<T: Into<Term>>(term: T, expected: usize, got: usize) -> Self {
        Self::WrongNumberOfArguments {
            term: Box::new(term.into()),
            expected,
            got,
        }
    }

    pub fn match_arity_mismatch<T: Into<Term>>(term: T, expected: usize, got: usize) -> Self {
        Self::MatchArityMismatch {
            term: Box::new(term.into()),
            expected,
            got,
        }
    }

    pub fn match_case_missing<T: Into<Term>, A: Into<Atom>>(term: T, atom: A) -> Self {
        Self::MatchCaseMissing {
            term: Box::new(term.into()),
            atom: atom.into(),
        }
    }

    pub fn cannot_infer_literal<T: Into<Term>>(term: T) -> Self {
        Self::CannotInferLiteral {
            term: Box::new(term.into()),
        }
    }

    pub fn unbound_variable<T: Into<Term>>(var: T) -> Self {
        Self::UnboundVariable {
            term: Box::new(var.into()),
        }
    }

    pub fn cannot_infer<T: Into<Term>>(term: T) -> Self {
        Self::CannotInfer {
            term: Box::new(term.into()),
        }
    }

    pub fn nat_overflow(value: BigUint) -> Self {
        Self::NatOverflow { value }
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

    pub fn format(&self) -> String {
        match self {
            Self::Located { span, error } => {
                format!("{error}\n\n{}", span.render_snippet())
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
            Error::NotAFunctionType { expected, .. } => {
                write!(
                    f,
                    "introduced a lambda where the expected type is not a function type\n  expected: {expected}"
                )
            }
            Error::NotATuple { head_type, .. } => {
                write!(
                    f,
                    "projected from a non-tuple\n  head has type: {head_type}"
                )
            }
            Error::NotATupleType { expected, .. } => {
                write!(
                    f,
                    "introduced a tuple where the expected type is not a tuple type\n  expected: {expected}"
                )
            }
            Error::NotAnArrayType { expected, .. } => {
                write!(
                    f,
                    "introduced an array where the expected type is not an array type\n  expected: {expected}"
                )
            }
            Error::NotArrType { head_type, .. } => {
                write!(f, "expected Arr but got {head_type}")
            }
            Error::TupleArityMismatch { expected, got, .. } => {
                write!(
                    f,
                    "tuple has {got} field(s) but expected type has {expected}"
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
            Error::NatOverflow { value } => {
                write!(f, "Nat literal {value} overflows u32 at the erase boundary")
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
    super::convert(context, &Term::type_(), this, that)
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
    // Register the refinement on the raw head when it's a Var or Proj. Without this,
    // a scrutinee whose canonical form reduces past the projection (e.g. when the
    // scrutinee is a literal tagged tuple) would record nothing, and the type checker
    // would type dead-arm bodies against the actual variant's payload rather than the
    // arm-assumed variant's payload. The frame containing this refinement is scoped to
    // the arm, so the (possibly counterfactual) assumption does not leak.
    match &**head {
        Subterm::Var(var) => {
            context.define(var.unwrap(), value);
        }
        Subterm::Proj(Proj { head, index }) => {
            context.define_projection(head.clone(), *index, value.clone());
        }
        _ => {}
    }

    // Also register on the canonical form. Handles let-chain canonicalization where
    // body references in the arm use the underlying base term rather than the raw
    // expression appearing at the match site.
    let canonical = reduce_with(context, head)?;

    match &*canonical {
        Subterm::Var(var) => {
            context.define(var.unwrap(), value);
        }
        Subterm::Proj(Proj { head, index }) => {
            context.define_projection(head.clone(), *index, value.clone());
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
            Subterm::Prim(Prim::Nat(Nat::new(0usize))),
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
            Subterm::Prim(Prim::Nat(Nat::new(5usize))),
            Subterm::Prim(Prim::NatType),
            Subterm::Prim(Prim::BlnType),
        );
        let s = err.to_string();
        assert!(s.contains("Nat"), "should contain inferred Nat: {s}");
        assert!(s.contains("Bln"), "should contain expected Bln: {s}");
    }
}
