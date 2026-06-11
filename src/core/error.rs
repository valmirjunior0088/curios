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
    UnknownTupleLabel {
        term: Box<Term>,
        label: String,
        available: Vec<String>,
    },
    DuplicateTupleLabel {
        term: Box<Term>,
        label: String,
    },
    TupleFieldNameMismatch {
        term: Box<Term>,
        written: String,
        expected: String,
        position: usize,
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
    CtorArityMismatch {
        term: Box<Term>,
        atom: Atom,
        expected: usize,
        got: usize,
    },
    NotAUnionType {
        term: Box<Term>,
        head_type: Box<Term>,
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
    /// An inserted implicit argument that unification never pinned. Carries
    /// the insertion provenance (the applied function and the binder it
    /// filled) so the report names the hole instead of a bare metavar id.
    UninferredImplicit {
        term: Box<Term>,
        func: String,
        binder: String,
    },
    /// A call supplies more `@`-arguments than the function has implicit
    /// binders (the explicit-slot counterpart is `WrongNumberOfArguments`).
    TooManyImplicits {
        term: Box<Term>,
        expected: usize,
        got: usize,
    },
    NatOverflow {
        value: BigUint,
    },
    /// A union match's annotated motive names a different union than the
    /// scrutinee's type.
    MotiveWrongUnion {
        term: Box<Term>,
        written: String,
        actual: String,
    },
    /// The annotated motive's slot count differs from the union's flat
    /// argument list (parameters then indices).
    MotivePatternArity {
        term: Box<Term>,
        expected: usize,
        got: usize,
    },
    /// A verbatim term written in a parameter slot of an annotated motive is
    /// not convertible with the scrutinee's actual parameter.
    MotiveParamMismatch {
        term: Box<Term>,
        written: Box<Term>,
        actual: Box<Term>,
    },
    /// An index slot of an annotated motive must bind (a fresh name or `_`)
    /// — index constraints belong to the declaration's case targets.
    MotiveIndexSlotNotBinder {
        term: Box<Term>,
        slot: Box<Term>,
    },
    /// An arm of an indexed-union match was omitted, but inversion could not
    /// prove the case impossible at the scrutinee's indices.
    MissingArmNotImpossible {
        term: Box<Term>,
        tag: Atom,
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

    pub fn unknown_tuple_label<T: Into<Term>>(
        term: T,
        label: String,
        available: Vec<String>,
    ) -> Self {
        Self::UnknownTupleLabel {
            term: Box::new(term.into()),
            label,
            available,
        }
    }

    pub fn duplicate_tuple_label<T: Into<Term>>(term: T, label: String) -> Self {
        Self::DuplicateTupleLabel {
            term: Box::new(term.into()),
            label,
        }
    }

    pub fn tuple_field_name_mismatch<T: Into<Term>>(
        term: T,
        written: String,
        expected: String,
        position: usize,
    ) -> Self {
        Self::TupleFieldNameMismatch {
            term: Box::new(term.into()),
            written,
            expected,
            position,
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

    pub fn not_a_union_type<T: Into<Term>, U: Into<Term>>(term: T, head_type: U) -> Self {
        Self::NotAUnionType {
            term: Box::new(term.into()),
            head_type: Box::new(head_type.into()),
        }
    }

    pub fn ctor_arity_mismatch<T: Into<Term>, A: Into<Atom>>(
        term: T,
        atom: A,
        expected: usize,
        got: usize,
    ) -> Self {
        Self::CtorArityMismatch {
            term: Box::new(term.into()),
            atom: atom.into(),
            expected,
            got,
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

    pub fn uninferred_implicit<T: Into<Term>>(term: T, func: String, binder: String) -> Self {
        Self::UninferredImplicit {
            term: Box::new(term.into()),
            func,
            binder,
        }
    }

    pub fn too_many_implicits<T: Into<Term>>(term: T, expected: usize, got: usize) -> Self {
        Self::TooManyImplicits {
            term: Box::new(term.into()),
            expected,
            got,
        }
    }

    pub fn nat_overflow(value: BigUint) -> Self {
        Self::NatOverflow { value }
    }

    pub fn motive_wrong_union<T: Into<Term>>(term: T, written: String, actual: String) -> Self {
        Self::MotiveWrongUnion {
            term: Box::new(term.into()),
            written,
            actual,
        }
    }

    pub fn motive_pattern_arity<T: Into<Term>>(term: T, expected: usize, got: usize) -> Self {
        Self::MotivePatternArity {
            term: Box::new(term.into()),
            expected,
            got,
        }
    }

    pub fn motive_param_mismatch<T: Into<Term>, U: Into<Term>>(
        term: T,
        written: U,
        actual: U,
    ) -> Self {
        Self::MotiveParamMismatch {
            term: Box::new(term.into()),
            written: Box::new(written.into()),
            actual: Box::new(actual.into()),
        }
    }

    pub fn motive_index_slot_not_binder<T: Into<Term>, U: Into<Term>>(term: T, slot: U) -> Self {
        Self::MotiveIndexSlotNotBinder {
            term: Box::new(term.into()),
            slot: Box::new(slot.into()),
        }
    }

    pub fn missing_arm_not_impossible<T: Into<Term>>(term: T, tag: Atom) -> Self {
        Self::MissingArmNotImpossible {
            term: Box::new(term.into()),
            tag,
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
            Error::UnknownTupleLabel {
                label, available, ..
            } => {
                if available.is_empty() {
                    write!(f, "no field named '{label}' (the tuple type has no labeled fields)")
                } else {
                    write!(
                        f,
                        "no field named '{label}' (available: {})",
                        available.join(", ")
                    )
                }
            }
            Error::DuplicateTupleLabel { label, .. } => {
                write!(f, "duplicate field label '{label}' in tuple type")
            }
            Error::TupleFieldNameMismatch {
                written,
                expected,
                position,
                ..
            } => {
                if expected.is_empty() {
                    write!(
                        f,
                        "field {position} is named '{written}' but the expected type has no label there"
                    )
                } else {
                    write!(
                        f,
                        "field {position} is named '{written}' but the expected type calls it '{expected}'"
                    )
                }
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
            Error::NotAUnionType { head_type, .. } => {
                write!(
                    f,
                    "matched union constructors on a non-union type\n  head has type: {head_type}"
                )
            }
            Error::CtorArityMismatch {
                atom,
                expected,
                got,
                ..
            } => {
                write!(
                    f,
                    "constructor '{atom}' takes {expected} argument(s) but the match arm binds {got}"
                )
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
            Error::UninferredImplicit { func, binder, .. } => {
                write!(
                    f,
                    "implicit argument '{binder}' of '{func}' was not inferred; supply it explicitly: {func}(@...)"
                )
            }
            Error::TooManyImplicits { expected, got, .. } => {
                write!(
                    f,
                    "call supplies {got} '@' argument(s) but the function has only {expected} implicit parameter(s)"
                )
            }
            Error::NatOverflow { value } => {
                write!(f, "Nat literal {value} overflows u32 at the erase boundary")
            }
            Error::MotiveWrongUnion {
                written, actual, ..
            } => {
                write!(
                    f,
                    "motive annotation names '{written}', but the scrutinee is a '{actual}'"
                )
            }
            Error::MotivePatternArity { expected, got, .. } => {
                write!(
                    f,
                    "motive annotation has {got} argument slot(s), but the union takes {expected} (parameters, then indices)"
                )
            }
            Error::MotiveParamMismatch {
                written, actual, ..
            } => {
                write!(
                    f,
                    "motive annotation fixes a parameter to\n  {written}\nbut the scrutinee's is\n  {actual}"
                )
            }
            Error::MotiveIndexSlotNotBinder { slot, .. } => {
                write!(
                    f,
                    "an index slot of a motive annotation must bind a fresh name (or `_`), but `{slot}` was written; indices are constrained by the declaration's case targets, not the motive"
                )
            }
            Error::MissingArmNotImpossible { tag, .. } => {
                write!(
                    f,
                    "missing arm '{tag}': its index target is not provably impossible at the scrutinee's indices — write the arm"
                )
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
