mod intrinsic;
pub use intrinsic::*;

use {
    super::{Subterm, Term, UniverseError},
    curios_abi::ForeignFunction,
    curios_base::{Int, Span},
    std::sync::Arc,
};

/// Reduce a host call's operands and rebuild the node.
///
/// There is nothing to fold: a foreign call denotes an inert description, so reduction does here exactly what it does for an `Io`-returning intrinsic — evaluate the operands, rebuild, and stop. It sits beside [`reduce_intrinsic`] rather than within it because [`Subterm::Foreign`] is a term former of its own, and every consumer that folds intrinsics must fold this too or leave a host call's operands unevaluated.
pub fn reduce_foreign(
    reducer: &mut impl Reducer,
    function: &Arc<ForeignFunction>,
    args: &[Term],
) -> Result<Subterm, ReduceError> {
    let mut reduced = Vec::with_capacity(args.len());
    for arg in args {
        reduced.push(reducer.reduce(arg.clone())?);
    }
    Ok(Subterm::Foreign(Arc::clone(function), reduced))
}

/// The failure mode of type-level evaluation: either the declaration's step budget ran out (`Exhausted`) or a partial intrinsic was folded outside its domain, carrying the offending redex's span. It is deliberately free of any elaboration vocabulary — a reducer reports what the *term* did, and the driver that owns the user-facing diagnostic decides how to phrase it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReduceError {
    /// The declaration's step budget ran out. Deterministic: the same program spends the same steps on every machine, so this is a fact about the program rather than about the host that compiled it.
    Exhausted,
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
    ListGetOutOfBounds {
        len: usize,
        index: usize,
        span: Option<Span>,
    },
    ListSliceOutOfRange {
        len: usize,
        start: usize,
        end: usize,
        span: Option<Span>,
    },
    /// A `Nat`/`Int` division whose divisor reduced to literal zero — mathematically undefined, so reported like [`ReduceError::BinGetOutOfBounds`] rather than panicking the fold. (Runtime *range* limits, by contrast, never error at the type level: `Nat`/`Int` folds are unbounded there.)
    DivisionByZero {
        kind: &'static str,
        span: Option<Span>,
    },
    /// An `Int/to_nat` whose operand reduced to a negative literal — a value no natural holds, so it is reported like [`ReduceError::DivisionByZero`] rather than folded by bit reinterpretation.
    IntToNatNegative {
        value: Int,
        span: Option<Span>,
    },
    Universe(UniverseError),
}

/// The evaluator an intrinsic fold calls back into for its operands.
///
/// Intrinsic folding is arithmetic on the representation and belongs here; deciding *how far* a term reduces — which definitions unfold, what a budget costs, which refinements are in scope, whether a `rec` is forced — is a strategy, and a strategy is a judgment. This trait is the seam between them: [`reduce_intrinsic`] states only that it needs its operands' values, and each consumer supplies the strategy it is entitled to. The elaborator's `Context` implements it with metavariable resolution and scrutinee refinement; a kernel implements it without either, and folds the same intrinsics.
///
/// The two methods differ in what they do with a `rec` head: [`Reducer::reduce`] stops at one, treating the folded spelling as the normal form, while [`Reducer::reduce_forced`] unfolds it because an eliminator demands a value.
pub trait Reducer {
    /// Reduce to weak-head normal form.
    fn reduce(&mut self, term: Term) -> Result<Term, ReduceError>;

    /// Reduce to weak-head normal form and then force a `rec` head, for a position that demands a value rather than a normal form.
    fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError>;
}
