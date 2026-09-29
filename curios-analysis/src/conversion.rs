//! What conversion does when two intrinsics meet, stated once for both checkers.
//!
//! Two intrinsics are equal when they are the same operation applied to convertible operands — a congruence, since both sides arrived reduced and a foldable operation would have folded. Before that congruence, the carriers' algebra decides what no operand-by-operand comparison can: a commuted sum, a De Morgan dual, a window of a window. [`convert_intrinsics`] runs that chain in one order for both checkers:
//!
//! 1. the driver's preparation of each side ([`Driver::prepare`]);
//! 2. a stuck product distributed, where neither side is a literal;
//! 3. the truth table over two `Bool` terms;
//! 4. two connective trees flattened, their leaves forced;
//! 5. two comparisons read through their linear views — one proposition where the views agree, and otherwise both respelled alike;
//! 6. the peels, the product-factor pairing first;
//! 7. the `Nat` peel retried once with each summand's arguments forced;
//! 8. the elaborator's packed-literal view ([`Driver::packed_view`]);
//! 9. two numbers of one operation compared as numbers;
//! 10. the congruence, each operand at the type its operation declares.
//!
//! **The chain decides; each checker discharges.** What it cannot settle comes back as an [`Outcome`] — a residual pair, or the congruence's levels and operands — rather than being compared here, because the two checkers compare differently and must: the elaborator enqueues and may solve, while the kernel compares in order under its active recursion history, whose keys include the goal and so must stay on the path that entered it. Nothing here calls a judgment.
//!
//! **Shared on the terms `curios-analysis` already states for itself.** The chain is algebra over the representation — the peels, the laws and the views are `curios-core`'s and `curios-algebra`'s, trusted by both checkers alike — and a second copy of it would be a second transcription of one function rather than a second opinion. What stays each checker's own is what does differ between them: the terms they are handed, how a residual is compared, and how levels are.
//!
//! **Two numbers of one operation are one number up to universe instances** (step 9). A number never depends on a level — Core offers no elimination from a type or a level into one — so `len(xs)` at two instances is one number whether it stands alone or inside a sum, as the cancellation already reads it inside one. Before this chain was shared, a bare pair fell to the congruence, which compared the operands' levels and refused where they differed.

use {
    curios_algebra::{Conclusion, Deduction},
    curios_core::{
        Aligned, Intrinsic, Level, Nat, Operand, Produced, ReduceError, Reducer, Subterm, Term,
        Var, Visit, align_comparisons, decide_bool, int_has_stuck_product, int_normalize, int_same,
        is_bool_connective, normalize_bool, peel_bin, peel_bool, peel_int_pair, peel_list,
        peel_monomial, peel_nat_pair, peel_position, peel_symmetric,
    },
    curios_utilities::SyntaxRegistry,
};

/// What a checker supplies to [`convert_intrinsics`] beyond its reduction.
pub trait Driver: Reducer {
    /// An intrinsic as this checker reads it before any peel does. The elaborator substitutes the metavariables it has solved, since two occurrences of one sum pair only once their witness metavariables are spliced; the kernel, handed finished terms, reads it as it is.
    fn prepare(&mut self, intrinsic: Intrinsic) -> Intrinsic;

    /// The elaborator's packed-literal view: a `Bin` literal against an `append` or `concat` spine, split at the spine's known lengths into goals the driver holds. `Some(true)` once those goals are the driver's, `Some(false)` on a length clash no solution can repair, and `None` where the view does not apply. The kernel has no such view, since it compares what the elaborator's solutions already made agree by reduction.
    fn packed_view(&mut self, this: &Intrinsic, that: &Intrinsic) -> Option<bool>;

    /// The registry an operation's signature is read through.
    fn syntax(&self) -> SyntaxRegistry;
}

/// What [`convert_intrinsics`] concluded, for the checker to act on.
pub enum Outcome {
    Equal,
    Unequal,
    /// Two terms to compare at `Type`: a residual a peel left, or a pair a demand took out of intrinsic form.
    Residual(Term, Term),
    /// The two sides as one operation: what is left is its congruence.
    Congruence(Congruence),
}

/// A congruence to discharge: the two sides' result levels first, then each operand pair in order.
pub struct Congruence {
    pub this_levels: Vec<Level>,
    pub that_levels: Vec<Level>,
    /// `None` where the two sides are not one operation, which is unequal once the levels are compared — they are compared first, as the checkers always compared them, since the elaborator's comparison may constrain them.
    pub operands: Option<Vec<Obligation>>,
}

/// One operand pair of a congruence, at the type its operation declares for it — `None` for a type operand or a function, which compare at `Type`: the first is compared *as* a type, and the second would need a binder minted to state its own.
///
/// A proof operand is therefore compared at its proposition, where proof irrelevance discharges it without reading either side, which is what lets two differently derived proofs of one bound convert. Nothing here is a rule about bounds: the demand comes off `Intrinsic::signature`.
pub struct Obligation {
    pub type_: Option<Term>,
    pub this: Term,
    pub that: Term,
}

/// Whether `this` and `that`, two intrinsics both checkers reached conversion with, are the same value — the chain the module documentation lays out, up to what only the driver can discharge.
pub fn convert_intrinsics(
    driver: &mut impl Driver,
    this: Intrinsic,
    that: Intrinsic,
) -> Result<Outcome, ReduceError> {
    let this = driver.prepare(this);
    let that = driver.prepare(that);

    // **A pair of `Nat`s decides how much of itself to build.** Both sides arrived head-forced, not merged. A literal against a sum with nothing left to force clashes from the head — a stuck symbolic summand is not definitionally a literal — and that is the answer a ten-definition web used to build 1 222 222 monomials to reach. Anything else is forced to its linear combination first, and the peels read the pair that produced.
    // **Two symbolic `Nat`s are distributed before they are peeled.** The fold leaves a product of two symbolic sums as a stuck node, so `(a + b) · (c + d)` and its expansion arrive as two shapes the peel cannot cancel against each other; normalizing both sides is the one demand that relates them. `Int` draws the same line at its own product, and each normalizer leaves the other carrier's terms untouched. A literal on either side needs nothing: sums and differences are already merged and cancelled by the fold, so a side with a symbolic summand is never a literal, and distributing it would build the polynomial to answer what the first summand settles.
    let (this, that) = match !(literal(&this) || literal(&that)) && (stuck(&this) || stuck(&that)) {
        false => (this, that),
        true => {
            let this = Nat::normalize(driver, Term::intrinsic(this))?;
            let that = Nat::normalize(driver, Term::intrinsic(that))?;
            let this = int_normalize(driver, this)?;
            let that = int_normalize(driver, that)?;
            match (as_intrinsic(&this), as_intrinsic(&that)) {
                (Some(this), Some(that)) => (this, that),
                _ => return Ok(Outcome::Residual(this, that)),
            }
        }
    };

    // **Two `Bool` terms, one of them a connective, are first put to the truth table over their atoms**, which decides what no leaf set or local law relates — De Morgan, absorption, distribution — and changes no spelling. A metavariable among the leaves is an atom like any other, since agreement at every assignment holds whatever it is solved to. Undecided is not unequal, so everything below runs as it did.
    if decide_bool(
        driver,
        &Term::intrinsic(this.clone()),
        &Term::intrinsic(that.clone()),
    )? {
        return Ok(Outcome::Equal);
    }

    // **Two `&&` trees, or two `||` trees, are flattened with their leaves forced before they are peeled** — the same demand by name as the stuck product's, because the fold leaves a stuck connective's right operand as written and the peel reads leaves without reducing. A tree against a `Bool` literal is flattened the same way: what decides it against `true` or `false` is a law on its leaves — an operand beside its own negation — and the fold left those leaves as written.
    let (this, that) = match (
        normalize_bool(driver, &this)?,
        normalize_bool(driver, &that)?,
    ) {
        (Some(this_tree), Some(that_tree)) => {
            match (as_intrinsic(&this_tree), as_intrinsic(&that_tree)) {
                (Some(this), Some(that)) => (this, that),
                _ => return Ok(Outcome::Residual(this_tree, that_tree)),
            }
        }
        (Some(this_tree), None) if matches!(that, Intrinsic::Bool(_)) => {
            match as_intrinsic(&this_tree) {
                Some(this) => (this, that),
                None => return Ok(Outcome::Residual(this_tree, Term::intrinsic(that))),
            }
        }
        (None, Some(that_tree)) if matches!(this, Intrinsic::Bool(_)) => {
            match as_intrinsic(&that_tree) {
                Some(that) => (this, that),
                None => return Ok(Outcome::Residual(Term::intrinsic(this), that_tree)),
            }
        }
        _ => (this, that),
    };

    // A negated comparison is read as its dual, and two `Nat` or `Int` comparisons through their linear views: one proposition where those agree, and otherwise both respelled in the one spelling their views give, so the congruence meets aligned operands. Probe-side, as the `&&`/`||` trees were, so no recorded refinement key is respelled.
    let (this, that) = match align_comparisons(driver, &this, &that)? {
        Some(Aligned::Same) => return Ok(Outcome::Equal),
        Some(Aligned::Respelled(pair)) => *pair,
        None => (this, that),
    };

    // `Nat`, `Bin` and `List` are free monoids, so two values of one are equal exactly when they agree after their longest common prefix is peeled off; `&&` and `||` are semilattices, so two of one are equal when they hold one set of leaves; and two stuck `get`s are one element when they read one position of one root. This decides `x + 2 ≡ y + 2` by comparing `x` with `y` rather than two opaque literals. `Undecided` falls through to the congruence, which still compares like-shaped operands, so a peel can only strengthen conversion. A sufficient residual is compared exactly as an equivalent one is: conversion establishes the equation by establishing it, and a residual that fails leaves the pair to the refusal it would have met anyway.
    if let Some(conclusion) = peel_monomial(&this, &that).or_else(|| {
        peel_nat_pair(&this, &that)
            .or_else(|| peel_int_pair(&this, &that))
            .or_else(|| peel_bin(&this, &that))
            .or_else(|| peel_list(&this, &that))
            .or_else(|| peel_bool(&this, &that))
            .or_else(|| peel_symmetric(&this, &that))
            .or_else(|| peel_position(&this, &that))
            .map(Conclusion::from)
    }) {
        match conclusion {
            Conclusion::Equal => return Ok(Outcome::Equal),
            Conclusion::Impossible => return Ok(Outcome::Unequal),
            Conclusion::Equivalent((left, right)) | Conclusion::Sufficient((left, right)) => {
                return Ok(Outcome::Residual(left, right));
            }
            // **The `Nat` peel's one unforced shape**, retried once with each summand's own arguments forced. The fold leaves a stuck application's arguments as written, so two summands that differ only inside their heads never pair; forcing them is `normalize_bool`'s demand for the other carrier, and it costs nothing on a pair that already decided. A retry that still finds nothing falls through to the congruence on the *original* spelling, so nothing downstream meets a respelled sum.
            Conclusion::Undecided => {
                if peel_nat_pair(&this, &that).is_some() {
                    let forced_this = Nat::normalize_atoms(driver, Term::intrinsic(this.clone()))?;
                    let forced_that = Nat::normalize_atoms(driver, Term::intrinsic(that.clone()))?;

                    if let (Some(forced_this), Some(forced_that)) =
                        (as_intrinsic(&forced_this), as_intrinsic(&forced_that))
                        && (forced_this != this || forced_that != that)
                        && let Some(peel) = peel_nat_pair(&forced_this, &forced_that)
                    {
                        match peel {
                            Deduction::Equal => return Ok(Outcome::Equal),
                            Deduction::Impossible => return Ok(Outcome::Unequal),
                            Deduction::Equivalent((left, right)) => {
                                return Ok(Outcome::Residual(left, right));
                            }
                            Deduction::Undecided => {}
                        }
                    }
                }
            }
        }
    }

    if let Some(view) = driver.packed_view(&this, &that) {
        return Ok(match view {
            true => Outcome::Equal,
            false => Outcome::Unequal,
        });
    }

    // The shapes carry everything that is not a term — which operation, which grain, which literal, which successor floor, which foreign row — so comparing them settles the operation's identity in one derived equality. Their result levels are the driver's to compare.
    let (mut this_shape, this_operands) = decompose(&this);
    let (mut that_shape, that_operands) = decompose(&that);
    if let Some(levels) = this_shape.result_universes_mut() {
        levels.clear();
    }
    if let Some(levels) = that_shape.result_universes_mut() {
        levels.clear();
    }
    let one_operation = this_shape == that_shape && this_operands.len() == that_operands.len();

    let operands = match one_operation {
        false => None,
        true => {
            let signature = this.signature(&driver.syntax());
            if same_number(&signature.produced, &this, &that) {
                return Ok(Outcome::Equal);
            }
            Some(
                this_operands
                    .into_iter()
                    .zip(that_operands)
                    .enumerate()
                    .map(|(index, (this, that))| Obligation {
                        type_: match signature.operands.get(index) {
                            Some(Operand::At(type_)) => Some(type_.clone()),
                            _ => None,
                        },
                        this,
                        that,
                    })
                    .collect(),
            )
        }
    };

    Ok(Outcome::Congruence(Congruence {
        this_levels: this.result_universes().to_vec(),
        that_levels: that.result_universes().to_vec(),
        operands,
    }))
}

/// Whether a `Bool` connective on either side agrees with the other side at every assignment of their atoms — the truth table both checkers put a connective to when the other side is no intrinsic at all, absorption's shape: `b || (b && c)` against the bare `b`, which the intrinsic chain never sees. `false` says nothing, and leaves the pair where it was. Where each checker asks it is its own: the elaborator before its dispatch, the kernel in its fallback ahead of its unfolding retry.
pub fn connectives_agree(
    reducer: &mut impl Reducer,
    this: &Term,
    that: &Term,
) -> Result<bool, ReduceError> {
    match is_bool_connective(this) || is_bool_connective(that) {
        true => decide_bool(reducer, this, that),
        false => Ok(false),
    }
}

/// Whether two applications of one operation that produces a `Nat` or an `Int` are one number by the carrier's identity — up to universe instances, as the cancellation reads its summands.
fn same_number(produced: &Produced, this: &Intrinsic, that: &Intrinsic) -> bool {
    let Produced::Fixed(type_) = produced else {
        return false;
    };
    let same: fn(&Term, &Term) -> bool = match &**type_ {
        Subterm::Intrinsic(Intrinsic::NatType) => Nat::same,
        Subterm::Intrinsic(Intrinsic::IntType) => int_same,
        _ => return false,
    };
    same(
        &Term::intrinsic(this.clone()),
        &Term::intrinsic(that.clone()),
    )
}

fn as_intrinsic(term: &Term) -> Option<Intrinsic> {
    match &**term {
        Subterm::Intrinsic(intrinsic) => Some(intrinsic.clone()),
        _ => None,
    }
}

fn stuck(intrinsic: &Intrinsic) -> bool {
    let term = Term::intrinsic(intrinsic.clone());
    Nat::has_stuck_product(&term) || int_has_stuck_product(&term)
}

fn literal(intrinsic: &Intrinsic) -> bool {
    matches!(intrinsic, Intrinsic::Nat(value) if value.to_natural().is_some())
        || matches!(intrinsic, Intrinsic::Int(_))
}

/// Split an intrinsic into its shape — itself, with every term operand stood down to one placeholder — and those operands in traversal order. Both halves come from `Intrinsic::traverse`, the single definition of what an intrinsic's operands are, so nothing here enumerates operations and nothing can forget one.
fn decompose(intrinsic: &Intrinsic) -> (Intrinsic, Vec<Term>) {
    let mut visit = Visit::masking(|_, _: &Var| None, Term::type_ground());
    let shape = intrinsic.traverse(&mut visit);

    (shape, visit.take_masked_children())
}
