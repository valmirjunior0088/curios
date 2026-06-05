use {
    super::{Atom, Term},
    crate::Span,
    num_bigint::BigUint,
    std::fmt,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReduceError {
    Preempted,
    BinGetOutOfBounds {
        len: usize,
        index: usize,
        span: Option<Span>,
    },
    BinSliceOutOfRange {
        len: usize,
        start: usize,
        end: usize,
        span: Option<Span>,
    },
    ArrGetOutOfBounds {
        len: usize,
        index: usize,
        span: Option<Span>,
    },
    ArrSliceOutOfRange {
        len: usize,
        start: usize,
        end: usize,
        span: Option<Span>,
    },
    IoAtTypeLevel {
        kind: &'static str,
        span: Option<Span>,
    },
}

#[derive(Debug)]
pub enum Error {
    ReducePreempted {
        term: Box<Term>,
    },
    ConvertPreempted {
        this: Box<Term>,
        that: Box<Term>,
    },
    BinGetOutOfBounds {
        len: usize,
        index: usize,
    },
    BinSliceOutOfRange {
        len: usize,
        start: usize,
        end: usize,
    },
    ArrGetOutOfBounds {
        len: usize,
        index: usize,
    },
    ArrSliceOutOfRange {
        len: usize,
        start: usize,
        end: usize,
    },
    IoAtTypeLevel {
        kind: &'static str,
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

    pub fn at_opt(self, span: Option<Span>) -> Self {
        match span {
            Some(span) => self.at(span),
            None => self,
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
            Error::BinGetOutOfBounds { len, index } => {
                write!(f, "Bin.get index {index} out of bounds (length {len})")
            }
            Error::BinSliceOutOfRange { len, start, end } => {
                write!(
                    f,
                    "Bin.slice range {start}..{end} out of range (length {len})"
                )
            }
            Error::ArrGetOutOfBounds { len, index } => {
                write!(f, "Arr.get index {index} out of bounds (length {len})")
            }
            Error::ArrSliceOutOfRange { len, start, end } => {
                write!(
                    f,
                    "Arr.slice range {start}..{end} out of range (length {len})"
                )
            }
            Error::IoAtTypeLevel { kind } => {
                write!(f, "{kind} cannot appear at the type level")
            }
            Error::Located { error, .. } => {
                write!(f, "{error}")
            }
        }
    }
}

impl ReduceError {
    pub fn into_error(self, preempted: impl FnOnce() -> Error) -> Error {
        match self {
            Self::Preempted => preempted(),
            Self::BinGetOutOfBounds { len, index, span } => {
                Error::BinGetOutOfBounds { len, index }.at_opt(span)
            }
            Self::BinSliceOutOfRange {
                len,
                start,
                end,
                span,
            } => Error::BinSliceOutOfRange { len, start, end }.at_opt(span),
            Self::ArrGetOutOfBounds { len, index, span } => {
                Error::ArrGetOutOfBounds { len, index }.at_opt(span)
            }
            Self::ArrSliceOutOfRange {
                len,
                start,
                end,
                span,
            } => Error::ArrSliceOutOfRange { len, start, end }.at_opt(span),
            Self::IoAtTypeLevel { kind, span } => Error::IoAtTypeLevel { kind }.at_opt(span),
        }
    }
}
