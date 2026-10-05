//! What both checkers share about an arm's case equation: which spellings of a scrutinee carry one, how far a dispatch is opened to resolve its spelling, which recorded equation a probe could reach, and which terms an equation answers.

#[cfg(test)]
mod tests;

use {
    crate::{Driver, intrinsics_agree},
    curios_algebra::{Carrier, Operation},
    curios_core::{
        Classes, Cost, Declaration, Free, Intrinsic, ReduceError, Reducer, Subterm, Term, atoms_of,
        classable, classed,
    },
    curios_utilities::SyntaxRegistry,
    std::mem,
};

/// How many application layers a dispatched scrutinee is opened through to reach the spelling it resolves to: each step consumes one layer of an elaborated dispatch, and a spine that has not settled in this many is not a dispatch.
///
/// One number because the checkers answer the same occurrences at the same point only while they resolve the same spellings: the kernel's `resolved_spelling`, the elaborator's `spine_whnf` and its `open_until` all stop here.
pub const RESOLVED_SPELLING_LAYERS: usize = 16;

/// Whether an arm's case equation may be recorded under `spelling` — the scrutinee as the kernel is handed it, local definitions substituted, or a spelling a dispatch resolves it to: only where it mentions a local.
///
/// **The kernel's evaluation memos are why.** A local-free term's reduct is remembered for the whole declaration, so an equation about one would leave an entry resting on an equation the arm's exit retracts; a local-bearing term's entry is cleared with the equations in force. The rule costs nothing a program needs: a local-free scrutinee reduces to its case value rather than sticking, so an arm under one is either the arm reduction takes or dead.
///
/// **Both checkers call it, and the elaborator on the kernel's spelling.** An equation one checker records and the other does not reads one arm two ways, and the arm is always a dead one: a proof the elaborator accepted there, under a guard over a top-level name, a global's projection, a local definition of a closed term or a dispatch whose method ignores its locals, is one the kernel refuses.
pub fn records_case_equation(spelling: &Term) -> bool {
    spelling.has_local_free()
}

/// Whether `candidate` could be the term `key` reduces to, as conversion reads a term — tested without reducing anything. `proofs` is the binders `candidate` names that are themselves proofs.
///
/// Reduction substitutes only closed definition bodies and subterms of the term it is reducing, so it can introduce a *global* name and can drop a local, but can never introduce a local the term did not already mention. A candidate naming a binder the key does not is therefore one no reduct of the key will ever equal, whatever the key reduces to.
///
/// **A binder that is a proof is passed over.** An equation answers what conversion holds equal to its scrutinee, and a term that converts with a reduct of the key without being one can name a binder the key does not, where conversion does not read it. What conversion does not read is a proof: `w(a, p2) < 5` is the scrutinee `w(a, p1) < 5` under another proof of one bound. So the binders counted are the ones that are no proofs, and each checker says which those are.
///
/// **What it still gives up.** A binder standing inside a proof term that is no variable, or in an argument reduction would erase — `f(b + 0 * c)` beside `f(b)` — is counted though conversion would not read it. The filter fails toward refusal there, and both checkers read it on one spelling with one account of what a proof is, so they fail together.
///
/// That makes this a filter and not a rule: every candidate an eager key would match still passes it, because such a candidate *is* a reduct of the key. What it excludes is the traffic — every stuck form produced under a binder some other judgment opened, which is most of what a probe at a stuck reduct sees.
///
/// Globals are deliberately not tested, and the asymmetry is the point: a reduct's globals are not bounded by the key's, so testing them would exclude exactly the unfoldings a settlement exists to perform.
///
/// Relaxing it to admit everything refuses nothing that holds, and admits the respellings named above — what it costs is `curios`' `scrutinee_refinement_measurements`, from flat back to the exponential this whole key exists to remove. Tightening it does change verdicts, silently, by dropping refinements; `curios-cert`'s `whnf::equations_tests::a_reduct_that_drops_a_local_is_still_reached` is the guard on that direction.
pub fn could_reduce_to(key: &Term, candidate: &Term, proofs: &[Free]) -> bool {
    let allowed = key.free_vars_shared();

    candidate
        .free_vars_shared()
        .iter()
        .filter(|name| name.is_local())
        .all(|name| allowed.contains(name) || proofs.contains(name))
}

/// What an arm's equation says of a term.
#[derive(Debug, PartialEq)]
pub enum Answered {
    /// The equation gives the term this value.
    Value(Term),
    /// It says nothing of the term: nothing the carriers' readers can see, and nothing a checker's conversion could.
    Silent,
    /// The readers decide nothing of the term against the key, and some two of the pair's atoms may be one: the checker classes them by its own conversion ([`Classes::of`]) and asks [`answers_classed`].
    Atoms(Vec<Term>),
    /// The term and the key are no two operations of the algebra, and may be one term, being one former over heads that may be one: the checker asks its own conversion whether they are, with the equation and every equation inside it withheld, as the key's reduced spelling was settled — under its own equation the key is its case value, and nothing would be the key.
    Whole,
}

/// What an arm's equation says of `term`, where the equation assumes `value` of a scrutinee whose reduct is `key`. Both terms are as reduction leaves a stuck form, weak-head with their operands reduced.
///
/// **One rule, for both reducers.** An equation is a claim about one term, and which terms are that term is answered here once. The term is the key; or the carriers' readers hold the two equal outright ([`intrinsics_agree`]), as conversion holds them wherever no arm stands — a sum commuted, a comparison with both sides moved, an `==` swapped; or, where the equation's value is a `Bool`, the readers hold the term equal to the key negated, and the value is negated on the way — a guard's dual, however it is spelled. Past that the readers pair atoms by identity, and which atoms are one is the checker's conversion's to say: the rule hands the question back ([`Answered::Atoms`], [`Answered::Whole`]) and is asked again with the answer ([`answers_classed`]). A reduction that asks nothing reads either as [`Answered::Silent`].
///
/// **It calls no judgment**, since it runs inside reduction.
///
/// **Nothing is reduced.** Both terms are reduced already, so the readers are handed a driver that takes every term as it stands (`Reduced`): forcing the term would ask the reducer for the stuck form it is in the middle of probing, forcing the key would meet the key's own equation and read the case value in its place, and forcing an operand again is work the kernel's memo replays the identities of on every read, which a lookup made at every stuck form would compound. A node the fold left as written below an operand — a connective's right operand behind a stuck left — is read as the atom it spells, which declines where reducing it might have answered.
///
/// **Only two terms of one kind are put to the readers.** The chain is written for the two sides of one comparison, which have one type, and a lookup presents whatever stuck form reduction reached: a pair is read only where both produce a `Bool` or both produce a value of one carrier.
pub fn answers(
    driver: &mut impl Driver,
    term: &Term,
    key: &Term,
    value: &Term,
) -> Result<Answered, ReduceError> {
    if term == key {
        return Ok(Answered::Value(value.clone()));
    }
    let operations = match (&**term, &**key) {
        (Subterm::Intrinsic(this), Subterm::Intrinsic(that)) => {
            let produced = produces(this);
            (produced.is_some() && produced == produces(that)).then_some((this, that))
        }
        _ => None,
    };
    let Some((this, that)) = operations else {
        // No two operations of one kind: one term only where the two are one former whose heads say they may be — which a variable, a metavariable and two operations of different kinds never are, and a spelling still to be unfolded is not until it is reduced.
        let whole = mem::discriminant(&**term) == mem::discriminant(&**key)
            && classable(&[term.clone(), key.clone()]);
        return Ok(match whole {
            true => Answered::Whole,
            false => Answered::Silent,
        });
    };

    let negated = negation(key);
    let mut reduced = Reduced(driver);
    if let Some(value) = decided(&mut reduced, this, that, &negated, value)? {
        return Ok(Answered::Value(value));
    }
    let atoms = atoms_of(&mut reduced, term, key)?;
    Ok(match classable(&atoms) {
        true => Answered::Atoms(atoms),
        false => Answered::Silent,
    })
}

/// [`answers`] asked again of a pair it handed [`Answered::Atoms`] back for, read with each atom spelled as its class's representative: the value the equation gives the term, or `None` where the classed pair decides nothing either.
///
/// The classed spellings are read as they stand, as the pair was: each is the term it was built from with atoms replaced by reduced terms they convert with.
pub fn answers_classed(
    driver: &mut impl Driver,
    term: &Term,
    key: &Term,
    value: &Term,
    classes: &Classes,
) -> Result<Option<Term>, ReduceError> {
    let mut reduced = Reduced(driver);
    let Some((this, that)) = classed(&mut reduced, term, key, classes)? else {
        return Ok(None);
    };
    let (Subterm::Intrinsic(this_operation), Subterm::Intrinsic(that_operation)) = (&*this, &*that)
    else {
        return Ok(None);
    };

    let negated = negation(&that);
    decided(
        &mut reduced,
        this_operation,
        that_operation,
        &negated,
        value,
    )
}

/// The value the readers give `this` against `that`, an equation's key assumed to be `value`: `value` where they hold the two equal outright, and where `value` is a `Bool`, its negation where they hold `this` equal to `negated`, the key negated.
fn decided(
    driver: &mut impl Driver,
    this: &Intrinsic,
    that: &Intrinsic,
    negated: &Term,
    value: &Term,
) -> Result<Option<Term>, ReduceError> {
    if intrinsics_agree(driver, this.clone(), that.clone())? {
        return Ok(Some(value.clone()));
    }
    let (Some(literal), Subterm::Intrinsic(negation)) = (value.as_bool(), &**negated) else {
        return Ok(None);
    };
    Ok(intrinsics_agree(driver, this.clone(), negation.clone())?
        .then(|| Term::intrinsic(Intrinsic::Bool(!literal))))
}

/// `key` negated, as `Bool/not` unfolds: the spelling the readers take a comparison's dual from.
fn negation(key: &Term) -> Term {
    Term::intrinsic(Intrinsic::BoolXor(
        key.clone(),
        Term::intrinsic(Intrinsic::Bool(true)),
    ))
}

/// What an operation produces, as far as its declaration says: a `Bool` for a comparison and for every operation on `Bool`, a `Nat` for a length, and otherwise a value of the carrier it is declared at. `None` for an intrinsic the algebra does not declare.
fn produces(intrinsic: &Intrinsic) -> Option<Carrier> {
    match intrinsic.algebra() {
        Declaration::Operation {
            carrier, operation, ..
        } => Some(match operation {
            Operation::Equal | Operation::Unequal | Operation::Less | Operation::AtMost => {
                Carrier::Boolean
            }
            Operation::Length => Carrier::Natural,
            _ => carrier,
        }),
        Declaration::Opaque => None,
    }
}

/// A driver that takes every term as reduced already: asked to reduce one it hands it back, and what the readers build is still charged to the checker's own driver.
struct Reduced<'a, D>(&'a mut D);

impl<D: Driver> Reducer for Reduced<'_, D> {
    fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
        Ok(term)
    }

    fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
        Ok(term)
    }

    fn spend(&mut self, cost: Cost) -> Result<(), ReduceError> {
        self.0.spend(cost)
    }

    fn fresh_binder(&mut self, hint: Option<&str>) -> Free {
        self.0.fresh_binder(hint)
    }
}

impl<D: Driver> Driver for Reduced<'_, D> {
    fn prepare(&mut self, intrinsic: Intrinsic) -> Intrinsic {
        self.0.prepare(intrinsic)
    }

    fn packed_view(&mut self, this: &Intrinsic, that: &Intrinsic) -> Option<bool> {
        self.0.packed_view(this, that)
    }

    fn syntax(&self) -> SyntaxRegistry {
        self.0.syntax()
    }
}
