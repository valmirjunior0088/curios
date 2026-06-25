use {
    super::{Atom, Int, Module, Term},
    crate::Span,
    num_bigint::BigUint,
    std::{collections::BTreeSet, fmt, rc::Rc},
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
    /// A `Nat`/`Int` division whose divisor reduced to literal zero —
    /// mathematically undefined, so reported like
    /// [`ReduceError::BinGetOutOfBounds`] rather than panicking the fold.
    /// (Runtime *range* limits, by contrast, never error at the type level:
    /// `Nat`/`Int` folds are unbounded there.)
    DivisionByZero {
        kind: &'static str,
        span: Option<Span>,
    },
}

/// Source-location anchoring is the [`Error::Located`] wrapper's job — the
/// elaborate/erase/zonk drivers attach the offending term's span as the error
/// propagates. Variants therefore carry only what their message displays; a
/// variant carries a `Term` only when the message prints it.
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
    DivisionByZero {
        kind: &'static str,
    },
    TypeMismatch {
        inferred: Box<Term>,
        expected: Box<Term>,
    },
    NotAFunction {
        head_type: Box<Term>,
    },
    NotAFunctionType {
        expected: Box<Term>,
    },
    NotATuple {
        head_type: Box<Term>,
    },
    NotATupleType {
        expected: Box<Term>,
    },
    TupleArityMismatch {
        expected: usize,
        got: usize,
    },
    TupleIndexOutOfBounds {
        index: usize,
        arity: usize,
    },
    UnknownTupleLabel {
        label: String,
        available: Vec<String>,
    },
    DuplicateTupleLabel {
        label: String,
    },
    TupleFieldNameMismatch {
        written: String,
        expected: String,
        position: usize,
    },
    NotNatType {
        head_type: Box<Term>,
    },
    NotBlnType {
        head_type: Box<Term>,
    },
    NotArrType {
        head_type: Box<Term>,
    },
    NotBinType {
        head_type: Box<Term>,
    },
    WrongNumberOfArguments {
        expected: usize,
        got: usize,
    },
    UnknownMatchConstructor {
        type_name: String,
        tag: String,
    },
    MatchCaseMissing {
        term: Box<Term>,
        atom: Atom,
    },
    CtorArityMismatch {
        atom: Atom,
        expected: usize,
        got: usize,
    },
    NotAInductiveType {
        head_type: Box<Term>,
    },
    /// A strict proposition was eliminated into a relevant (data) result — a
    /// large elimination that would observe which inhabitant it was, breaking
    /// proof irrelevance. Permitted only for an empty or singleton proposition.
    LargeElimOfProp {
        name: String,
    },
    /// A struct literal's (or struct type's) head names a binding that is not a
    /// struct; `found` is that binding's type.
    NotAStructType {
        found: Box<Term>,
    },
    /// Wrong number of type arguments applied to a struct's type-former.
    StructArityMismatch {
        name: String,
        expected: usize,
        got: usize,
    },
    /// A struct literal supplies the wrong number of fields.
    WrongNumberOfFields {
        name: String,
        expected: usize,
        got: usize,
    },
    /// A written field label does not match the declared label at its position
    /// (fields are given in declaration order — no reordering).
    UnknownStructField {
        name: String,
        label: String,
        available: Vec<String>,
    },
    /// Projecting a field of a struct whose representation is private, from
    /// outside the declaring module (§7).
    PrivateField {
        name: String,
        field: String,
    },
    /// Building a struct whose representation is private, from outside the
    /// declaring module (§7) — the construction counterpart of `PrivateField`.
    PrivateRepresentation {
        name: String,
    },
    CannotInferLiteral,
    UnboundVariable {
        term: Box<Term>,
    },
    CannotInfer,
    /// An overloaded infix operator applied at an operand type with no matching
    /// scalar primitive — `%` on `Flt`, `!=` on `Bln`, `+` on `Bln`, etc. The
    /// `symbol` is the operator's spelling; `type_` is the resolved operand type.
    OperatorUndefined {
        symbol: String,
        type_: Box<Term>,
    },
    /// An inserted implicit argument that unification never pinned. Carries
    /// the insertion provenance (the applied function and the binder it
    /// filled) so the report names the hole instead of a bare metavar id.
    UninferredImplicit {
        func: String,
        binder: String,
    },
    /// A call supplies more `@`-arguments than the function has implicit
    /// binders (the explicit-slot counterpart is `WrongNumberOfArguments`).
    TooManyImplicits {
        expected: usize,
        got: usize,
    },
    NatOverflow {
        value: BigUint,
    },
    /// An `Int` literal that survived to `erase` but does not fit `ersd`'s
    /// `i32` carrier — the type level is unbounded, so the representation
    /// narrowing lives at the erase boundary, like [`Error::NatOverflow`]'s
    /// u32. (The runtime's own i31 limit is enforced where it appears:
    /// `cont` → wasm lowering.)
    IntOverflow {
        value: Box<Int>,
    },
    /// An inductive match's annotated motive names a different inductive than the
    /// scrutinee's type.
    MotiveWrongInductive {
        written: String,
        actual: String,
    },
    /// The annotated motive's slot count differs from the inductive's flat
    /// argument list (parameters then indices).
    MotivePatternArity {
        expected: usize,
        got: usize,
    },
    /// A verbatim term written in a parameter slot of an annotated motive is
    /// not convertible with the scrutinee's actual parameter.
    MotiveParamMismatch {
        written: Box<Term>,
        actual: Box<Term>,
    },
    /// An index slot of an annotated motive must bind (a fresh name or `_`)
    /// — index constraints belong to the declaration's case targets.
    MotiveIndexSlotNotBinder {
        slot: Box<Term>,
    },
    /// An arm of an indexed-inductive match was omitted, but inversion could not
    /// prove the case impossible at the scrutinee's indices.
    MissingArmNotImpossible {
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

    pub fn type_mismatch<U: Into<Term>, V: Into<Term>>(inferred: U, expected: V) -> Self {
        Self::TypeMismatch {
            inferred: Box::new(inferred.into()),
            expected: Box::new(expected.into()),
        }
    }

    pub fn not_a_function<U: Into<Term>>(head_type: U) -> Self {
        Self::NotAFunction {
            head_type: Box::new(head_type.into()),
        }
    }

    pub fn not_a_function_type<U: Into<Term>>(expected: U) -> Self {
        Self::NotAFunctionType {
            expected: Box::new(expected.into()),
        }
    }

    pub fn not_a_tuple<U: Into<Term>>(head_type: U) -> Self {
        Self::NotATuple {
            head_type: Box::new(head_type.into()),
        }
    }

    pub fn not_a_tuple_type<U: Into<Term>>(expected: U) -> Self {
        Self::NotATupleType {
            expected: Box::new(expected.into()),
        }
    }

    pub fn tuple_arity_mismatch(expected: usize, got: usize) -> Self {
        Self::TupleArityMismatch { expected, got }
    }

    pub fn tuple_index_out_of_bounds(index: usize, arity: usize) -> Self {
        Self::TupleIndexOutOfBounds { index, arity }
    }

    pub fn unknown_tuple_label(label: String, available: Vec<String>) -> Self {
        Self::UnknownTupleLabel { label, available }
    }

    pub fn duplicate_tuple_label(label: String) -> Self {
        Self::DuplicateTupleLabel { label }
    }

    pub fn tuple_field_name_mismatch(written: String, expected: String, position: usize) -> Self {
        Self::TupleFieldNameMismatch {
            written,
            expected,
            position,
        }
    }

    pub fn not_nat_type<U: Into<Term>>(head_type: U) -> Self {
        Self::NotNatType {
            head_type: Box::new(head_type.into()),
        }
    }

    pub fn not_bln_type<U: Into<Term>>(head_type: U) -> Self {
        Self::NotBlnType {
            head_type: Box::new(head_type.into()),
        }
    }

    pub fn not_arr_type<U: Into<Term>>(head_type: U) -> Self {
        Self::NotArrType {
            head_type: Box::new(head_type.into()),
        }
    }

    pub fn not_bin_type<U: Into<Term>>(head_type: U) -> Self {
        Self::NotBinType {
            head_type: Box::new(head_type.into()),
        }
    }

    pub fn wrong_number_of_arguments(expected: usize, got: usize) -> Self {
        Self::WrongNumberOfArguments { expected, got }
    }

    pub fn unknown_match_constructor(type_name: String, tag: String) -> Self {
        Self::UnknownMatchConstructor { type_name, tag }
    }

    pub fn match_case_missing<T: Into<Term>, A: Into<Atom>>(term: T, atom: A) -> Self {
        Self::MatchCaseMissing {
            term: Box::new(term.into()),
            atom: atom.into(),
        }
    }

    pub fn not_a_inductive_type<U: Into<Term>>(head_type: U) -> Self {
        Self::NotAInductiveType {
            head_type: Box::new(head_type.into()),
        }
    }

    pub fn large_elim_of_prop<N: Into<String>>(name: N) -> Self {
        Self::LargeElimOfProp { name: name.into() }
    }

    pub fn not_a_struct_type<U: Into<Term>>(found: U) -> Self {
        Self::NotAStructType {
            found: Box::new(found.into()),
        }
    }

    pub fn struct_arity_mismatch<N: Into<String>>(name: N, expected: usize, got: usize) -> Self {
        Self::StructArityMismatch {
            name: name.into(),
            expected,
            got,
        }
    }

    pub fn wrong_number_of_fields<N: Into<String>>(name: N, expected: usize, got: usize) -> Self {
        Self::WrongNumberOfFields {
            name: name.into(),
            expected,
            got,
        }
    }

    pub fn unknown_struct_field<N: Into<String>>(
        name: N,
        label: String,
        available: Vec<String>,
    ) -> Self {
        Self::UnknownStructField {
            name: name.into(),
            label,
            available,
        }
    }

    pub fn private_field<N: Into<String>, F: Into<String>>(name: N, field: F) -> Self {
        Self::PrivateField {
            name: name.into(),
            field: field.into(),
        }
    }

    pub fn private_representation<N: Into<String>>(name: N) -> Self {
        Self::PrivateRepresentation { name: name.into() }
    }

    pub fn ctor_arity_mismatch<A: Into<Atom>>(atom: A, expected: usize, got: usize) -> Self {
        Self::CtorArityMismatch {
            atom: atom.into(),
            expected,
            got,
        }
    }

    pub fn unbound_variable<T: Into<Term>>(var: T) -> Self {
        Self::UnboundVariable {
            term: Box::new(var.into()),
        }
    }

    pub fn operator_undefined<T: Into<Term>>(symbol: String, type_: T) -> Self {
        Self::OperatorUndefined {
            symbol,
            type_: Box::new(type_.into()),
        }
    }

    pub fn uninferred_implicit(func: String, binder: String) -> Self {
        Self::UninferredImplicit { func, binder }
    }

    pub fn too_many_implicits(expected: usize, got: usize) -> Self {
        Self::TooManyImplicits { expected, got }
    }

    pub fn nat_overflow(value: BigUint) -> Self {
        Self::NatOverflow { value }
    }

    pub fn int_overflow(value: Int) -> Self {
        Self::IntOverflow {
            value: Box::new(value),
        }
    }

    pub fn motive_wrong_inductive(written: String, actual: String) -> Self {
        Self::MotiveWrongInductive { written, actual }
    }

    pub fn motive_pattern_arity(expected: usize, got: usize) -> Self {
        Self::MotivePatternArity { expected, got }
    }

    pub fn motive_param_mismatch<U: Into<Term>>(written: U, actual: U) -> Self {
        Self::MotiveParamMismatch {
            written: Box::new(written.into()),
            actual: Box::new(actual.into()),
        }
    }

    pub fn motive_index_slot_not_binder<U: Into<Term>>(slot: U) -> Self {
        Self::MotiveIndexSlotNotBinder {
            slot: Box::new(slot.into()),
        }
    }

    pub fn missing_arm_not_impossible(tag: Atom) -> Self {
        Self::MissingArmNotImpossible { tag }
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
        // Render with source-style names (axis (a)): collect the names appearing
        // across every term this error displays, build one collision-aware
        // rename map for them, and install it for the duration of the render so
        // `inferred`/`expected` agree on what each name means.
        let mut terms = Vec::new();
        self.collect_terms(&mut terms);

        let mut names = BTreeSet::new();
        for term in terms {
            names.extend(super::display_names(term));
        }

        let rename = Rc::new(super::build_rename(&names));
        super::with_pretty_names(rename, || self.render())
    }

    /// Like [`format`], additionally shortening global names against `module`'s
    /// symbol table (axis (b)) — the qualified-name universe an error's globals
    /// are spelled relative to. Used on the core error paths, where the lowered
    /// module is in scope.
    pub fn format_with(&self, module: &Module) -> String {
        super::with_short_names(
            Rc::new(super::build_shorten(&super::module_symbols(module))),
            || self.format(),
        )
    }

    fn render(&self) -> String {
        match self {
            Self::Located { span, error } => {
                format!("{error}\n\n{}", span.render_snippet())
            }
            error => error.to_string(),
        }
    }

    /// The terms this error embeds in its message, gathered so [`format`] can
    /// pretty-print their names consistently. Recurses through the `Located`
    /// wrapper; variants carrying no term contribute nothing.
    fn collect_terms<'a>(&'a self, out: &mut Vec<&'a Term>) {
        match self {
            Self::Located { error, .. } => error.collect_terms(out),
            Self::ReducePreempted { term } => out.push(term),
            Self::ConvertPreempted { this, that } => {
                out.push(this);
                out.push(that);
            }
            Self::TypeMismatch { inferred, expected } => {
                out.push(inferred);
                out.push(expected);
            }
            Self::NotAFunction { head_type }
            | Self::NotATuple { head_type }
            | Self::NotNatType { head_type }
            | Self::NotBlnType { head_type }
            | Self::NotArrType { head_type }
            | Self::NotBinType { head_type }
            | Self::NotAInductiveType { head_type } => out.push(head_type),
            Self::NotAFunctionType { expected } | Self::NotATupleType { expected } => {
                out.push(expected)
            }
            Self::NotAStructType { found } => out.push(found),
            Self::MatchCaseMissing { term, .. } => out.push(term),
            Self::UnboundVariable { term } => out.push(term),
            Self::MotiveParamMismatch { written, actual } => {
                out.push(written);
                out.push(actual);
            }
            Self::MotiveIndexSlotNotBinder { slot } => out.push(slot),
            _ => {}
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
            Error::TypeMismatch { inferred, expected } => {
                write!(
                    f,
                    "type mismatch\n  inferred: {inferred}\n  expected: {expected}"
                )
            }
            Error::NotAFunction { head_type } => {
                write!(f, "applied a non-function\n  head has type: {head_type}")
            }
            Error::NotAFunctionType { expected } => {
                write!(
                    f,
                    "introduced a lambda where the expected type is not a function type\n  expected: {expected}"
                )
            }
            Error::NotATuple { head_type } => {
                write!(
                    f,
                    "projected from a non-tuple\n  head has type: {head_type}"
                )
            }
            Error::NotATupleType { expected } => {
                write!(
                    f,
                    "introduced a tuple where the expected type is not a tuple type\n  expected: {expected}"
                )
            }
            Error::TupleArityMismatch { expected, got } => {
                write!(
                    f,
                    "tuple has {got} field(s) but expected type has {expected}"
                )
            }
            Error::TupleIndexOutOfBounds { index, arity } => {
                write!(f, "tuple index {index} out of bounds (arity {arity})")
            }
            Error::UnknownTupleLabel { label, available } => {
                if available.is_empty() {
                    write!(
                        f,
                        "no field named '{label}' (the tuple type has no labeled fields)"
                    )
                } else {
                    write!(
                        f,
                        "no field named '{label}' (available: {})",
                        available.join(", ")
                    )
                }
            }
            Error::DuplicateTupleLabel { label } => {
                write!(f, "duplicate field label '{label}' in tuple type")
            }
            Error::TupleFieldNameMismatch {
                written,
                expected,
                position,
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
            Error::NotNatType { head_type } => {
                write!(f, "expected Nat but got {head_type}")
            }
            Error::NotBlnType { head_type } => {
                write!(f, "expected Bool but got {head_type}")
            }
            Error::NotArrType { head_type } => {
                write!(f, "expected Arr but got {head_type}")
            }
            Error::NotBinType { head_type } => {
                write!(f, "expected Bin but got {head_type}")
            }
            Error::WrongNumberOfArguments { expected, got } => {
                write!(
                    f,
                    "wrong number of arguments: expected {expected}, got {got}"
                )
            }
            Error::UnknownMatchConstructor { type_name, tag } => {
                write!(f, "match arm '{tag}' is not a constructor of '{type_name}'")
            }
            Error::MatchCaseMissing { term, atom } => {
                write!(f, "missing match case for constructor '{atom}': {term}")
            }
            Error::NotAInductiveType { head_type } => {
                write!(
                    f,
                    "matched inductive constructors on a non-inductive type\n  head has type: {head_type}"
                )
            }
            Error::LargeElimOfProp { name } => {
                write!(
                    f,
                    "cannot eliminate the proposition '{name}' into a relevant result\n  a strict proposition admits large elimination only when empty or singleton"
                )
            }
            Error::NotAStructType { found } => {
                write!(f, "expected a struct type here\n  found: {found}")
            }
            Error::StructArityMismatch {
                name,
                expected,
                got,
            } => {
                write!(
                    f,
                    "struct '{name}' takes {expected} type argument(s) but got {got}"
                )
            }
            Error::WrongNumberOfFields {
                name,
                expected,
                got,
            } => {
                write!(
                    f,
                    "struct '{name}' has {expected} field(s) but the literal supplies {got}"
                )
            }
            Error::UnknownStructField {
                name,
                label,
                available,
            } => {
                write!(
                    f,
                    "struct '{name}' has no field '{label}' at that position (fields in order: {})",
                    available.join(", ")
                )
            }
            Error::PrivateField { name, field } => {
                write!(
                    f,
                    "field '{field}' of struct '{name}' is private to its declaring module"
                )
            }
            Error::PrivateRepresentation { name } => {
                write!(
                    f,
                    "the representation of struct '{name}' is private to its declaring module"
                )
            }
            Error::CtorArityMismatch {
                atom,
                expected,
                got,
            } => {
                write!(
                    f,
                    "constructor '{atom}' takes {expected} argument(s) but the match arm binds {got}"
                )
            }
            Error::CannotInferLiteral => {
                write!(f, "cannot infer type of literal (add an annotation)")
            }
            Error::UnboundVariable { term } => {
                write!(f, "unbound variable: {term}")
            }
            Error::CannotInfer => {
                write!(f, "cannot infer type of expression")
            }
            Error::OperatorUndefined { symbol, type_ } => {
                write!(f, "operator '{symbol}' is not defined for type {type_}")
            }
            Error::UninferredImplicit { func, binder } => {
                write!(
                    f,
                    "implicit argument '{binder}' of '{func}' was not inferred; supply it explicitly: {func}(@...)"
                )
            }
            Error::TooManyImplicits { expected, got } => {
                write!(
                    f,
                    "call supplies {got} '@' argument(s) but the function has only {expected} implicit parameter(s)"
                )
            }
            Error::NatOverflow { value } => {
                write!(f, "Nat literal {value} overflows u32 at the erase boundary")
            }
            Error::IntOverflow { value } => {
                write!(
                    f,
                    "Int literal {value:+} overflows i32 at the erase boundary"
                )
            }
            Error::MotiveWrongInductive { written, actual } => {
                write!(
                    f,
                    "motive annotation names '{written}', but the scrutinee is a '{actual}'"
                )
            }
            Error::MotivePatternArity { expected, got } => {
                write!(
                    f,
                    "motive annotation has {got} argument slot(s), but the inductive takes {expected} (parameters, then indices)"
                )
            }
            Error::MotiveParamMismatch { written, actual } => {
                write!(
                    f,
                    "motive annotation fixes a parameter to\n  {written}\nbut the scrutinee's is\n  {actual}"
                )
            }
            Error::MotiveIndexSlotNotBinder { slot } => {
                write!(
                    f,
                    "an index slot of a motive annotation must bind a fresh name (or `_`), but `{slot}` was written; indices are constrained by the declaration's case targets, not the motive"
                )
            }
            Error::MissingArmNotImpossible { tag } => {
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
            Error::DivisionByZero { kind } => {
                write!(f, "division by zero in {kind}")
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
            Self::DivisionByZero { kind, span } => Error::DivisionByZero { kind }.at_opt(span),
        }
    }
}
