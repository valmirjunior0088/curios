//! What conversion does when two intrinsics meet, stated once for both checkers.
//!
//! Two intrinsics are equal when they are the same operation applied to convertible operands — a congruence, since both sides arrived reduced and a foldable operation would have folded. Before that congruence, the carriers' algebra decides what no operand-by-operand comparison can: a commuted sum, a De Morgan dual, a window of a window. [`convert_intrinsics`] runs that chain in one order for both checkers:
//!
//! 1. the driver's preparation of each side ([`Driver::prepare`]);
//! 2. a stuck product distributed, where neither side is a literal;
//! 3. the truth table over two `Bool` terms;
//! 4. two connective trees flattened, their leaves forced;
//! 5. two comparisons read through their linear views — one proposition where the views agree, and otherwise both respelled alike;
//! 6. the peels, the product-factor and comparison-side pairings first;
//! 7. where steps 3 to 6 decided nothing, the pair's atoms handed back to be classed ([`Pass::Atoms`]), and steps 3 to 6 read once more with each atom spelled as its class's representative ([`convert_classed`]);
//! 8. the elaborator's packed-literal view ([`Driver::packed_view`]);
//! 9. two numbers of one operation compared as numbers;
//! 10. the congruence, each operand at the type its operation declares.
//!
//! **The chain decides; each checker discharges.** What it cannot settle comes back — a residual pair, the congruence's levels and operands, or the atoms it needs classed before it can read on — rather than being compared here, because the two checkers compare differently and must: the elaborator enqueues and may solve, while the kernel compares in order under its active recursion history, whose keys include the goal and so must stay on the path that entered it. Nothing here calls a judgment.
//!
//! **Shared on the terms `curios-analysis` already states for itself.** The chain is algebra over the representation — the peels, the laws and the views are `curios-core`'s and `curios-algebra`'s, trusted by both checkers alike — and a second copy of it would be a second transcription of one function rather than a second opinion. What stays each checker's own is what does differ between them: the terms they are handed, how a residual is compared, and how levels are.
//!
//! **Two numbers of one operation are one number up to universe instances** (step 9). A number never depends on a level — Core offers no elimination from a type or a level into one — so `len(xs)` at two instances is one number whether it stands alone or inside a sum, as the cancellation already reads it inside one. Left to the congruence, a bare pair would have its operands' levels compared and be refused where they differ.

use {
    curios_algebra::Conclusion,
    curios_core::{
        Aligned, Classes, Intrinsic, Level, Nat, Operand, Probe, Produced, ReduceError, Reducer,
        Subterm, Term, Var, Visit, align_comparisons, atoms_of, classable, classed, decide_bool,
        int_has_stuck_product, int_normalize, int_same, is_bool_connective, normalize_bool,
        peel_bin, peel_bool, peel_comparison, peel_int_pair, peel_list, peel_monomial,
        peel_nat_pair, peel_position, peel_symmetric,
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

/// What the chain's first pass over a pair came to.
pub enum Pass {
    /// Settled as far as the chain settles anything: the outcome to discharge.
    Settled(Outcome),
    /// Nothing decided, and some two of the pair's atoms may be one: the checker classes them by its own conversion ([`Classes::of`]) and enters [`convert_classed`].
    Atoms(Vec<Term>),
}

/// What the chain concluded, for the checker to act on.
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
    /// `None` where the two sides are not one operation, which is unequal once the levels are compared — they are compared first, since the elaborator's comparison may constrain them.
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

/// Whether `this` and `that`, two intrinsics both checkers reached conversion with, are the same value — the chain the module documentation lays out, up to what only the driver can discharge, and stopping at step 7 where the pair has atoms to class.
pub fn convert_intrinsics(
    driver: &mut impl Driver,
    this: Intrinsic,
    that: Intrinsic,
) -> Result<Pass, ReduceError> {
    chain(driver, this, that, None)
}

/// [`convert_intrinsics`] entered again with the partition its atoms were classed into: the whole chain, step 7 reading the pair with each atom spelled as its class's representative.
pub fn convert_classed(
    driver: &mut impl Driver,
    this: Intrinsic,
    that: Intrinsic,
    classes: &Classes,
) -> Result<Outcome, ReduceError> {
    match chain(driver, this, that, Some(classes))? {
        Pass::Settled(outcome) => Ok(outcome),
        // The chain hands its atoms back only where it was given no partition.
        Pass::Atoms(_) => Ok(Outcome::Unequal),
    }
}

/// The chain over one pair: with no partition it stops at step 7 where there are atoms to class, and with one it reads through.
fn chain(
    driver: &mut impl Driver,
    this: Intrinsic,
    that: Intrinsic,
    classes: Option<&Classes>,
) -> Result<Pass, ReduceError> {
    let this = driver.prepare(this);
    let that = driver.prepare(that);

    // **A pair of `Nat`s decides how much of itself to build.** Both sides arrived head-forced, not merged. A literal against a sum with nothing left to force clashes from the head — a stuck symbolic summand is not definitionally a literal — where distributing first can build over a million monomials, over a ten-definition web, to reach the same answer. Anything else is forced to its linear combination first, and the peels read the pair that produced.
    // **Two symbolic `Nat`s are distributed before they are peeled.** The fold leaves a product of two symbolic sums as a stuck node, so `(a + b) · (c + d)` and its expansion arrive as two shapes the peel cannot cancel against each other; normalizing both sides is the one demand that relates them. `Int` draws the same line at its own product, and each normalizer leaves the other carrier's terms untouched. A literal on either side needs nothing: sums and differences are already merged and cancelled by the fold, so a side with a symbolic summand is never a literal, and distributing it would build the polynomial to answer what the first summand settles.
    let (this, that) = match !(literal(&this) || literal(&that)) && (stuck(&this) || stuck(&that)) {
        false => (this, that),
        true => {
            // Both normalizers are demands where the fold's comparison asks them and probes here, where the chain can compare a side as it arrived: one with no value at the type level is left undistributed.
            let this = Term::intrinsic(this);
            let that = Term::intrinsic(that);
            let this = Nat::normalize(driver, this.clone())
                .probed()?
                .unwrap_or(this);
            let that = Nat::normalize(driver, that.clone())
                .probed()?
                .unwrap_or(that);
            let this = int_normalize(driver, this.clone())
                .probed()?
                .unwrap_or(this);
            let that = int_normalize(driver, that.clone())
                .probed()?
                .unwrap_or(that);
            match (as_intrinsic(&this), as_intrinsic(&that)) {
                (Some(this), Some(that)) => (this, that),
                _ => return Ok(Pass::Settled(Outcome::Residual(this, that))),
            }
        }
    };

    // Steps 3 to 6 read the pair through the carriers' algebra ([`read`]); what they leave undecided is handed back for its atoms to be classed, or read once more as its partition spells it (step 7, [`read_classed`]), and what that leaves undecided goes on to the congruence as the first reading left it.
    let (this, that) = match read(driver, this.clone(), that.clone())? {
        Read::Decided(outcome) => return Ok(Pass::Settled(outcome)),
        Read::Undecided(read) => {
            match classes {
                None => {
                    let atoms = atoms_of(
                        driver,
                        &Term::intrinsic(this.clone()),
                        &Term::intrinsic(that.clone()),
                    )?;
                    if classable(&atoms) {
                        return Ok(Pass::Atoms(atoms));
                    }
                }
                Some(classes) => {
                    if let Some(outcome) = read_classed(driver, &this, &that, classes)? {
                        return Ok(Pass::Settled(outcome));
                    }
                }
            }
            *read
        }
    };

    if let Some(view) = driver.packed_view(&this, &that) {
        return Ok(Pass::Settled(match view {
            true => Outcome::Equal,
            false => Outcome::Unequal,
        }));
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
                return Ok(Pass::Settled(Outcome::Equal));
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

    Ok(Pass::Settled(Outcome::Congruence(Congruence {
        this_levels: this.result_universes().to_vec(),
        that_levels: that.result_universes().to_vec(),
        operands,
    })))
}

/// What reading a pair through the carriers' algebra came to.
enum Read {
    Decided(Outcome),
    /// Nothing decided: the pair as the reading respelled it, which is what the congruence reads.
    Undecided(Box<(Intrinsic, Intrinsic)>),
}

/// Steps 3 to 6 over one pair: the truth table, the connective trees, the comparisons' views and the peels.
fn read(driver: &mut impl Driver, this: Intrinsic, that: Intrinsic) -> Result<Read, ReduceError> {
    // **Two `Bool` terms, one of them a connective, are first put to the truth table over their atoms**, which decides what no leaf set or local law relates — De Morgan, absorption, distribution — and changes no spelling. A metavariable among the leaves is an atom like any other, since agreement at every assignment holds whatever it is solved to. Undecided is not unequal, so a pair the table leaves undecided goes on to everything below.
    if decide_bool(
        driver,
        &Term::intrinsic(this.clone()),
        &Term::intrinsic(that.clone()),
    )? {
        return Ok(Read::Decided(Outcome::Equal));
    }

    // **Two `&&` trees, or two `||` trees, are flattened with their leaves forced before they are peeled** — the same demand by name as the stuck product's, because the fold leaves a stuck connective's right operand as written and the peel reads leaves without reducing. A tree against a `Bool` literal is flattened the same way: what decides it against `true` or `false` is a law on its leaves — an operand beside its own negation — and the fold left those leaves as written.
    let (this, that) = match (
        normalize_bool(driver, &this)?,
        normalize_bool(driver, &that)?,
    ) {
        (Some(this_tree), Some(that_tree)) => {
            match (as_intrinsic(&this_tree), as_intrinsic(&that_tree)) {
                (Some(this), Some(that)) => (this, that),
                _ => return Ok(Read::Decided(Outcome::Residual(this_tree, that_tree))),
            }
        }
        (Some(this_tree), None) if matches!(that, Intrinsic::Bool(_)) => {
            match as_intrinsic(&this_tree) {
                Some(this) => (this, that),
                None => {
                    return Ok(Read::Decided(Outcome::Residual(
                        this_tree,
                        Term::intrinsic(that),
                    )));
                }
            }
        }
        (None, Some(that_tree)) if matches!(this, Intrinsic::Bool(_)) => {
            match as_intrinsic(&that_tree) {
                Some(that) => (this, that),
                None => {
                    return Ok(Read::Decided(Outcome::Residual(
                        Term::intrinsic(this),
                        that_tree,
                    )));
                }
            }
        }
        _ => (this, that),
    };

    // A negated comparison is read as its dual, and two `Nat` or `Int` comparisons through their linear views: one proposition where those agree, and otherwise both respelled in the one spelling their views give, so the congruence meets aligned operands. Probe-side, as the `&&`/`||` trees were, so no recorded refinement key is respelled.
    let (this, that) = match align_comparisons(driver, &this, &that)? {
        Some(Aligned::Same) => return Ok(Read::Decided(Outcome::Equal)),
        Some(Aligned::Respelled(pair)) => *pair,
        None => (this, that),
    };

    // `Nat`, `Bin` and `List` are free monoids, so two values of one are equal exactly when they agree after their longest common prefix is peeled off; `&&` and `||` are semilattices, so two of one are equal when they hold one set of leaves; and two stuck `get`s are one element when they read one position of one root. This decides `x + 2 ≡ y + 2` by comparing `x` with `y` rather than two opaque literals. `Undecided` falls through to the congruence, which still compares like-shaped operands, so a peel can only strengthen conversion. A sufficient residual is compared exactly as an equivalent one is: conversion establishes the equation by establishing it, and a residual that fails leaves the pair to the refusal it would have met anyway.
    if let Some(conclusion) = peel_monomial(&this, &that)
        .or_else(|| peel_comparison(&this, &that))
        .or_else(|| {
            peel_nat_pair(&this, &that)
                .or_else(|| peel_int_pair(&this, &that))
                .or_else(|| peel_bin(&this, &that))
                .or_else(|| peel_list(&this, &that))
                .or_else(|| peel_bool(&this, &that))
                .or_else(|| peel_symmetric(&this, &that))
                .or_else(|| peel_position(&this, &that))
                .map(Conclusion::from)
        })
    {
        match conclusion {
            Conclusion::Equal => return Ok(Read::Decided(Outcome::Equal)),
            Conclusion::Impossible => return Ok(Read::Decided(Outcome::Unequal)),
            Conclusion::Equivalent((left, right)) | Conclusion::Sufficient((left, right)) => {
                return Ok(Read::Decided(Outcome::Residual(left, right)));
            }
            Conclusion::Undecided => {}
        }
    }

    Ok(Read::Undecided(Box::new((this, that))))
}

/// Step 7: `this` and `that` read once more with each atom spelled as its class's representative — `None` where the partition moved neither side, or the classed pair decided nothing either.
///
/// **Every reader above keys an atom on its spelling**, so two atoms that convert without being identical — `f(a + b)` and `f(b + a)`, or one call under two proofs of its bound — are two atoms to all of them, though the checker's conversion decides that pair the moment it compares it. Left there, such a pair falls to the congruence, which compares operands in the order the term holds them, so one equation would hold or fail with the order its operands were written in or its binders declared in. The partition is the checker's answer to which atoms are one, and it is asked for only where the pair as it stood decided nothing.
///
/// **The congruence still meets the spelling it was handed.** A classed pair that decides nothing is dropped, so no respelled term reaches a checker's comparison; one that decides hands on an outcome whose residuals are the classed spelling's, a pair definitionally equal to the one asked about.
fn read_classed(
    driver: &mut impl Driver,
    this: &Intrinsic,
    that: &Intrinsic,
    classes: &Classes,
) -> Result<Option<Outcome>, ReduceError> {
    let this = Term::intrinsic(this.clone());
    let that = Term::intrinsic(that.clone());
    let Some((this, that)) = classed(driver, &this, &that, classes)? else {
        return Ok(None);
    };
    // A node the readers read through is rebuilt as the same node, so both sides are still intrinsics.
    let (Some(this), Some(that)) = (as_intrinsic(&this), as_intrinsic(&that)) else {
        return Ok(None);
    };
    Ok(match read(driver, this, that)? {
        Read::Decided(outcome) => Some(outcome),
        Read::Undecided(..) => None,
    })
}

/// What [`connectives_agree`] came to.
pub enum Agreement {
    /// The two sides agree at every assignment of their atoms.
    Agree,
    /// Nothing decided, which says nothing of the pair and leaves it where it was.
    Silent,
    /// Nothing decided, and some two of the pair's atoms may be one: the checker classes them and asks [`connectives_agree_classed`].
    Atoms(Vec<Term>),
}

/// Whether a `Bool` connective on either side agrees with the other side at every assignment of their atoms — the truth table both checkers put a connective to when the other side is no intrinsic at all, absorption's shape: `b || (b && c)` against the bare `b`, which the intrinsic chain never sees. Where each checker asks it is its own: the elaborator before its dispatch, the kernel in its fallback ahead of its unfolding retry.
///
/// The table keys its atoms on spelling, as every reader in the chain does, so a pair it does not decide hands its atoms back to be classed, as the chain's step 7 hands its own: `p(a + b) || (p(b + a) && q)` against `p(a + b)` is absorption once the two leaves are one atom.
pub fn connectives_agree(
    reducer: &mut impl Reducer,
    this: &Term,
    that: &Term,
) -> Result<Agreement, ReduceError> {
    if !(is_bool_connective(this) || is_bool_connective(that)) {
        return Ok(Agreement::Silent);
    }
    if decide_bool(reducer, this, that)? {
        return Ok(Agreement::Agree);
    }
    let atoms = atoms_of(reducer, this, that)?;
    Ok(match classable(&atoms) {
        true => Agreement::Atoms(atoms),
        false => Agreement::Silent,
    })
}

/// [`connectives_agree`] asked again of a pair it handed [`Agreement::Atoms`] back for, read with each atom spelled as its class's representative. `false` says nothing.
pub fn connectives_agree_classed(
    reducer: &mut impl Reducer,
    this: &Term,
    that: &Term,
    classes: &Classes,
) -> Result<bool, ReduceError> {
    match classed(reducer, this, that, classes)? {
        Some((this, that)) => decide_bool(reducer, &this, &that),
        None => Ok(false),
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
