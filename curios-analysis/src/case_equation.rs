//! What both checkers share about an arm's case equation: which spellings of a scrutinee carry one, how far a dispatch is opened to resolve its spelling, which recorded equation a probe could reach, and which terms an equation answers.

#[cfg(test)]
mod tests;

use {
    crate::{Driver, intrinsics_agree},
    curios_algebra::{Carrier, Operation},
    curios_core::{Cost, Declaration, Free, Intrinsic, ReduceError, Reducer, Subterm, Term},
    curios_utilities::SyntaxRegistry,
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

/// Whether reducing `key` could possibly produce `candidate` — a necessary condition, tested without reducing anything.
///
/// Reduction substitutes only closed definition bodies and subterms of the term it is reducing, so it can introduce a *global* name and can drop a local, but can never introduce a local the term did not already mention. A candidate naming a binder the key does not is therefore one no reduct of the key will ever equal, whatever the key reduces to.
///
/// That makes this a filter and not a rule: every candidate an eager key would match still passes it, because such a candidate *is* a reduct of the key. What it excludes is the traffic — every stuck form produced under a binder some other judgment opened, which is most of what a probe at a stuck reduct sees.
///
/// Globals are deliberately not tested, and the asymmetry is the point: a reduct's globals are not bounded by the key's, so testing them would exclude exactly the unfoldings a settlement exists to perform.
///
/// Being a filter, relaxing it to admit everything changes no verdict and moves no fixture — what it moves is `curios`' `scrutinee_refinement_measurements`, from flat back to the exponential this whole key exists to remove. Tightening it does change verdicts, silently, by dropping refinements; `curios-cert`'s `whnf::equations_tests::a_reduct_that_drops_a_local_is_still_reached` is the guard on that direction.
pub fn could_reduce_to(key: &Term, candidate: &Term) -> bool {
    let allowed = key.free_vars_shared();

    candidate
        .free_vars_shared()
        .iter()
        .filter(|name| name.is_local())
        .all(|name| allowed.contains(name))
}

/// The value an arm's equation gives `term`, where the equation assumes `value` of a scrutinee whose reduct is `key`: `None` where the equation says nothing of the term. Both terms are as reduction leaves a stuck form, weak-head with their operands reduced.
///
/// **One rule, for both reducers.** An equation is a claim about one term, and which terms are that term was answered by each checker in its own code, by the spellings a key is held under and a list of the other ways one comparison is written. Here it is answered once. The term is the key; or the carriers' readers hold the two equal outright ([`intrinsics_agree`]), as conversion holds them wherever no arm stands — a sum commuted, a comparison with both sides moved, an `==` swapped; or, where the equation's value is a `Bool`, the readers hold the term equal to the key negated, and the value is negated on the way — a guard's dual, however it is spelled.
///
/// **It calls no judgment**, since it runs inside reduction: the readers pair atoms by identity, so two atoms that convert without being identical are two atoms here.
///
/// **Nothing is reduced.** Both terms are reduced already, so the readers are handed a driver that takes every term as it stands (`Reduced`): forcing the term would ask the reducer for the stuck form it is in the middle of probing, forcing the key would meet the key's own equation and read the case value in its place, and forcing an operand again is work the kernel's memo replays the identities of on every read, which a lookup made at every stuck form would compound. A node the fold left as written below an operand — a connective's right operand behind a stuck left — is read as the atom it spells, which declines where reducing it might have answered.
///
/// **Only two terms of one kind are put to the readers.** The chain is written for the two sides of one comparison, which have one type, and a lookup presents whatever stuck form reduction reached: a pair is asked only where both produce a `Bool` or both produce a value of one carrier.
pub fn answers(
    driver: &mut impl Driver,
    term: &Term,
    key: &Term,
    value: &Term,
) -> Result<Option<Term>, ReduceError> {
    if term == key {
        return Ok(Some(value.clone()));
    }
    let (Subterm::Intrinsic(this), Subterm::Intrinsic(that)) = (&**term, &**key) else {
        return Ok(None);
    };
    let produced = produces(this);
    if produced.is_none() || produced != produces(that) {
        return Ok(None);
    }

    let negated = Term::intrinsic(Intrinsic::BoolXor(
        key.clone(),
        Term::intrinsic(Intrinsic::Bool(true)),
    ));
    let mut reduced = Reduced(driver);
    if intrinsics_agree(&mut reduced, this.clone(), that.clone())? {
        return Ok(Some(value.clone()));
    }
    let (Some(literal), Subterm::Intrinsic(negation)) = (value.as_bool(), &*negated) else {
        return Ok(None);
    };
    Ok(
        intrinsics_agree(&mut reduced, this.clone(), negation.clone())?
            .then(|| Term::intrinsic(Intrinsic::Bool(!literal))),
    )
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
