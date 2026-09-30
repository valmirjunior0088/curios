mod intrinsic;
pub use intrinsic::*;

use {
    super::{Category, Cost, Free, Subterm, Term, UniverseError},
    curios_abi::ForeignFunction,
    curios_num::{Integer, Natural},
    curios_utilities::Span,
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
    reducer.spend(Cost::collection(args.len() as u64))?;

    let mut reduced = Vec::with_capacity(args.len());
    for arg in args {
        reduced.push(reducer.reduce(arg.clone())?);
    }
    Ok(Subterm::Foreign(Arc::clone(function), reduced))
}

/// The failure mode of type-level evaluation: either the declaration's step budget ran out (`Exhausted`) or a partial intrinsic was folded outside its domain, carrying the offending redex's span. It is deliberately free of any elaboration vocabulary — a reducer reports what the *term* did, and the driver that owns the user-facing diagnostic decides how to phrase it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReduceError {
    /// The declaration's work budget ran out. Deterministic: the same program spends the same units on every machine, so this is a fact about the program rather than about the host that compiled it.
    ///
    /// The three fields are attribution without a second budget, captured *before* the refusal and from bounded metadata alone — so no error path attempts the allocation that was refused. One number still decides acceptance; these say what it was being spent on when it ran out.
    Exhausted {
        /// What the refused charge was for.
        category: Category,
        /// What the budget had left when the charge was refused.
        remaining: u64,
        /// What the refused charge came to. [`u64::MAX`] is a size that overflowed while being computed, which is refused without being compared.
        attempted: u64,
    },
    BinGetOutOfBounds {
        len: usize,
        index: usize,
        span: Option<Span>,
    },
    BinSliceOutOfRange {
        len: usize,
        start: usize,
        length: usize,
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
        length: usize,
        span: Option<Span>,
    },
    /// A `Nat`/`Int` division whose divisor reduced to literal zero — mathematically undefined, so reported like [`ReduceError::BinGetOutOfBounds`] rather than panicking the fold. (Runtime *range* limits, by contrast, never error at the type level: `Nat`/`Int` folds are unbounded there.)
    DivisionByZero {
        kind: &'static str,
        span: Option<Span>,
    },
    /// An `Int/to_nat` whose operand reduced to a negative literal — a value no natural holds, so it is reported like [`ReduceError::DivisionByZero`] rather than folded by bit reinterpretation.
    IntToNatNegative {
        value: Integer,
        span: Option<Span>,
    },
    /// A `Nat/to_byte` whose operand reduced past the carrier — reported rather than masked, for the reason [`ReduceError::IntToNatNegative`] is reported rather than reinterpreted. The operation's `below` field states this cannot happen, so reaching here means a proof was admitted that should not have been.
    NatToByteAbove {
        value: Natural,
        span: Option<Span>,
    },
    Universe(UniverseError),
}

impl ReduceError {
    /// The refusal a budget of `remaining` gives when it cannot afford `cost`.
    pub fn exhausted(remaining: u64, cost: Cost) -> Self {
        Self::Exhausted {
            category: cost.category(),
            remaining,
            attempted: cost.get(),
        }
    }
}

/// A failure that may be a spent budget rather than a verdict on the term the budget was spent on — the one distinction a [`Probe`] reads, stated by each checker's own error: [`ReduceError`] here, the elaborator's diagnostic and the kernel's error where each is defined.
pub trait Exhaustion {
    /// The budget's refusal this failure is or carries, or `None` for a verdict on the term — so a failure of one checker's vocabulary can hand the refusal on in reduction's ([`Probe::probed_refusal`]).
    fn refusal(&self) -> Option<&ReduceError>;

    /// Whether this failure is a spent budget.
    fn is_exhausted(&self) -> bool {
        self.refusal().is_some()
    }
}

/// A failure that cannot happen carries no refusal — the error of a driver that never spends.
impl Exhaustion for std::convert::Infallible {
    fn refusal(&self) -> Option<&ReduceError> {
        match *self {}
    }
}

impl Exhaustion for ReduceError {
    /// Read rather than matched at every call site: the payload exists to be *read* by a diagnostic, and every other consumer only wants to know which of the two kinds of failure this is.
    fn refusal(&self) -> Option<&ReduceError> {
        matches!(self, Self::Exhausted { .. }).then_some(self)
    }
}

/// A reduction a probe asked for, read by the one rule every probe keeps.
///
/// **A probe asks for what its judgment can do without**: a spelling that only widens what the judgment sees — a refinement key's canonical form, the reduced spelling an inversion also abstracts, a candidate proof. A *demand* needs the value instead, and whatever reduction answers is its answer, which is why the folds propagate every failure.
///
/// **The rule.** The budget's exhaustion propagates: it is no verdict on the term but the declaration's own, it never refunds, and the judgment the probe serves has to see it. Every other failure is the term having no value at the type level — an access out of range, an effect — and the probe answers `None`, falling back to the spelling it was handed. A site that absorbed exhaustion instead would answer a question it could not afford — not convertible, not a proposition, no witness — and elaboration would go ahead on that answer with nothing left to spend.
pub trait Probe<T, E> {
    /// The value, `None` where the term has none at the type level, and the budget's exhaustion alone as a failure.
    fn probed(self) -> Result<Option<T>, E>;

    /// [`Probe::probed`] for a judgment that reports in reduction's terms while its probe reports in another's — a conversion asking an elaboration: the refusal the failure carries, handed on as the [`ReduceError`] it is.
    fn probed_refusal(self) -> Result<Option<T>, ReduceError>;
}

impl<T, E: Exhaustion> Probe<T, E> for Result<T, E> {
    fn probed(self) -> Result<Option<T>, E> {
        match self {
            Ok(value) => Ok(Some(value)),
            Err(error) if error.is_exhausted() => Err(error),
            Err(_) => Ok(None),
        }
    }

    fn probed_refusal(self) -> Result<Option<T>, ReduceError> {
        match self {
            Ok(value) => Ok(Some(value)),
            Err(error) => match error.refusal() {
                Some(refusal) => Err(refusal.clone()),
                None => Ok(None),
            },
        }
    }
}

/// The evaluator an intrinsic fold calls back into for its operands.
///
/// Intrinsic folding is arithmetic on the representation and belongs here; deciding *how far* a term reduces — which definitions unfold, what a budget costs, which refinements are in scope, whether a `rec` is forced — is a strategy, and a strategy is a judgment. This trait is the seam between them: [`reduce_intrinsic`] states only that it needs its operands' values, and each consumer supplies the strategy it is entitled to. The elaborator's `Context` implements it with metavariable resolution and scrutinee refinement; a kernel implements it without either, and folds the same intrinsics.
///
/// The two reduction methods differ in what they do with a `rec` head: [`Reducer::reduce`] stops at one, treating the folded spelling as the normal form, while [`Reducer::reduce_forced`] unfolds it because an eliminator demands a value.
///
/// The third is what makes the budget bound memory rather than only steps. [`Reducer::spend`] charges a [`Cost`] against the same counter a transition spends from, and every fold that can allocate reducer-owned storage calls it *before* allocating — a charge taken afterwards is a report rather than a limit. See [`cost`](crate::Cost) for the unit and the formulas.
pub trait Reducer {
    /// Reduce to weak-head normal form.
    fn reduce(&mut self, term: Term) -> Result<Term, ReduceError>;

    /// Reduce to weak-head normal form and then force a `rec` head, for a position that demands a value rather than a normal form.
    fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError>;

    /// Charge `cost` before constructing what it prices, failing when the budget cannot afford it.
    ///
    /// A saturated cost is refused outright rather than compared, so a size that overflowed can never be handed to an allocator; [`Cost`]'s module documentation carries the argument. Charging nothing is still a call — a site that computes [`Cost::NOTHING`] because it shares rather than builds is saying so, and saying so is what the audit checks.
    fn spend(&mut self, cost: Cost) -> Result<(), ReduceError>;

    /// A fresh binder identity, for a fold that must look under a binder to decide — `List/map`'s identity test opens the mapped function's body on one, and the closed machine's eta probe does the same. The identity space is the strategy's, since a binder minted here must alias none the lowerer, the elaborator or the archived prelude minted; it is assumed at no type, because what is asked of it is a weak-head reduct and never a judgment.
    fn fresh_binder(&mut self, hint: Option<&str>) -> Free;
}

#[cfg(test)]
mod tests;
