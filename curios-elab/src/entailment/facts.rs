//! The facts a bound may be proved from, and the goal it states: what the hole's scope says about `Nat` and `Int` comparisons, each read through the canonical linear view `curios-core` publishes and paired with a term that proves it.
//!
//! **Where a fact comes from.**
//!
//! - A hypothesis in scope whose type reduces to a decided comparison, a conjunction of comparisons — `&&`, and any `Bool` function unfolding to one as `in_range` and `Char/is_upper` do — or an equation at `Nat` or `Int`.
//! - A proof field of a tuple- or struct-typed hypothesis, one level down, where the item may open the representation: `v.counted`, `hi.ok`.
//! - A hypothesis or a field whose statement the arm's refinements reduce to an empty proposition — `in_range(n, 128, 191)` where a guard holds `n <= 127` — which refutes the scope by itself: read as `1 <= 0`, proved by its own zero-arm match.
//! - A guard of an arm the hole sits in — the arm's at insertion, and at a retry the ones the hole was born under: a comparison scrutinee with the case it took, an `==` scrutinee's true arm, and a range check's true arm, through `le/of_in_range`, since a split in the guard's own arm meets the guard's key, which answers `true` for the whole check and never unfolds it. Its proof is stated over the operands the guard spells ([`Reader::guards`]), and a guard recorded under two spellings is one fact.
//! - What an operation a fact or the goal is read over defines: a quotient's bounds, a truncated subtraction's cases, a `min`'s and a `max`'s, an `abs`'s and a `sign`'s, and that a power of a base conversion holds at least one is at least one ([`Reader::definitions`]).
//! - Where linear arithmetic over those finds an assignment, the product of each pair of them and the negated goal ([`products`]): what `nlinarith` adds, and what a multiplier that is no literal needs. A product of two sums, which the fold leaves standing, is read distributed ([`Reader::side`]).
//! - That every monomial over naturals is at least zero, which conversion decides.
//!
//! A local definition and a variable a match refined need no reading of their own: every statement is read reduced, so a fact stated over a `let` meets a goal stated over its value, and one over `k` meets one over the successor an arm refined `k` to.
//!
//! **A remainder by a literal is lifted away.** Conversion reads `k * (x / k) + x % k` as `x` wherever one side of a comparison holds both, so a sum of facts that holds a remainder beside its quotient's multiple is not the sum of their views. Every fact over `x % k` is therefore raised by the multiple on both sides, which conversion reads as a fact over `x` ([`Lift`]), and the remainder stays out of every form the search reads.
//!
//! **A statement is written as it is stated and read as it reduces.** A hypothesis, a field, a guard and the goal are opened by their heads alone, as a guard's resolved spelling is, to the comparison, range check or conjunction they state ([`opened`]), and their sides are the operands as stated; only the view reads their reducts, each with the metavariables elaboration has solved substituted and its atoms classed by the elaborator's conversion among those of every operand read before it ([`Reader::reading`]), so a call is one atom however each statement spells it. A reduct is no spelling to write: a call reduced to the body it unfolds to carries that body's own arms, which elaborate only where they were written, and a local definition reduced to its value names locals the arm's key does not. What the facts invent over atoms is written through the spellings the atoms were read from ([`Facts::respell`]). Where opening reaches none of the three, the statement is read reduced.
//!
//! **A fact is `left <= right` at its carrier**, as the proof's lemmas take it: a strict fact is the loose one of its successor, which conversion reads as one proposition at `Nat` as at `Int`, and an equation is two facts. Its form is `left - right` aligned to `<=`, read by the one reader every fact and the goal share, so their atoms are one table's.
//!
//! **What is not read is recorded.** A proposition stating a decision the view does not read — a disjunction, an opaque `Bool` function, an equality's false arm — is kept for the report.

use {
    super::{decision_of, global, in_scope, is_empty},
    crate::{
        Context, Error, classing_among, open_layer, reduce_with, same_uncommitted,
        zonk_solved_term_metas,
    },
    curios_algebra::{Carrier, Lattice, LinearForm, Monomial, Operation},
    curios_analysis::RESOLVED_SPELLING_LAYERS,
    curios_core::{
        Apply, Cases, Classes, Extremum, Free, Global, InductType, Intrinsic, LinearViews, Match,
        Nat, Probe, StructType, Subterm, Term, TupleType, atoms_in, classed_in, int_normalize,
    },
    curios_num::{Integer, Natural},
    curios_utilities::{OrderSyntax, Plicity, SyntaxName},
};

/// Where a fact came from, for the report.
#[derive(Clone, Debug, PartialEq)]
pub enum Origin {
    /// A hypothesis in the hole's scope, as the variable it is — a term, so a report spells it through its reader's names.
    Hypothesis(Term),
    /// A proof field of one, by its label or its position.
    Field(Term, String),
    /// A guard of an arm the hole sits in, as written, and the case it took.
    Guard(Term, bool),
    /// That a monomial over naturals is at least zero.
    Natural,
    /// What an operation defines: a quotient's bound, a case of a truncated subtraction, of a `min`, a `max`, an `abs` or a `sign`, or that a power is at least one.
    Definition,
    /// The product of two facts, from where each came.
    Product(Box<Origin>, Box<Origin>),
    /// The goal, negated.
    Negated,
}

impl Origin {
    /// The term the origin names: the hypothesis, or the guard as written.
    pub fn named(&self) -> Option<&Term> {
        match self {
            Origin::Hypothesis(name) | Origin::Field(name, _) | Origin::Guard(name, _) => {
                Some(name)
            }
            Origin::Natural | Origin::Definition | Origin::Product(..) | Origin::Negated => None,
        }
    }

    /// Whether a report names the fact: one a reader can act on, which a natural's sign, an operation's definition and a product of two facts are not.
    pub(super) fn is_reported(&self) -> bool {
        !matches!(
            self,
            Origin::Natural | Origin::Definition | Origin::Product(..)
        )
    }

    /// Whether the fact is the negated goal or a product with it: proved only where a split on the goal refuted it, so a certificate using one is written in the refuting form.
    pub(super) fn negates(&self) -> bool {
        match self {
            Origin::Negated => true,
            Origin::Product(left, right) => left.negates() || right.negates(),
            _ => false,
        }
    }
}

/// One fact: `left <= right` at `carrier`, proved by `proof`.
#[derive(Clone)]
pub(super) struct Fact {
    pub(super) origin: Origin,
    /// The comparison as it was stated, a strict one still strict, for the report.
    pub(super) stated: Term,
    pub(super) carrier: Carrier,
    pub(super) left: Term,
    pub(super) right: Term,
    /// `left - right`, aligned to `<=`.
    pub(super) form: LinearForm,
    /// A term whose type conversion aligns with `left <= right`.
    pub(super) proof: Term,
}

impl Fact {
    /// Its sides at `carrier`: widened where the fact is at `Nat` and `carrier` is `Int`, the proposition conversion aligns it with.
    pub(super) fn sides_at(&self, carrier: Carrier) -> (Term, Term) {
        let widen = |side: &Term| match (self.carrier, carrier) {
            (Carrier::Natural, Carrier::Integer) => {
                Term::intrinsic(Intrinsic::NatToInt(side.clone()))
            }
            _ => side.clone(),
        };
        (widen(&self.left), widen(&self.right))
    }
}

/// The goal a decision states: `left < right` or `left <= right` at `carrier`, as its reduct spells it.
pub(super) struct Target {
    pub(super) carrier: Carrier,
    pub(super) relation: Operation,
    pub(super) left: Term,
    pub(super) right: Term,
    /// `left - right`, aligned to `<=`.
    pub(super) form: LinearForm,
    /// The decision as the bound's reduct spells it: what the refuting form splits on, so the refinement its arms install is keyed where the bound reads it.
    pub(super) decision: Term,
}

/// What the scope states: the facts read, the case splits their operations define, the remainders lifted out of them, and the propositions stating a decision the view does not read.
pub(super) struct Facts {
    pub(super) facts: Vec<Fact>,
    pub(super) splits: Vec<Split>,
    pub(super) lifts: Vec<Lift>,
    pub(super) unread: Vec<(Origin, Term)>,
    /// Each reduct an operand was read through, beside the operand as stated.
    spelled: Vec<(Term, Term)>,
}

impl Facts {
    /// `term` with every reduct an operand was read through written as the operand was stated: what the facts invent over atoms — a natural's sign, a quotient's bounds, a subtraction's cases — is built from the atoms' reducts, and is written over the spellings in scope. An outer reduct is replaced before one inside it.
    pub(super) fn respell(&self, term: &Term) -> Term {
        let mut pending = self.spelled.iter().collect::<Vec<_>>();
        let mut term = term.clone();
        while let Some(at) = (0..pending.len()).find(|&index| {
            let (reduct, _) = pending[index];
            !pending
                .iter()
                .enumerate()
                .any(|(other, (outer, _))| other != index && outer.mentions_term(reduct))
        }) {
            let (reduct, stated) = pending.swap_remove(at);
            term = term.replace_term(reduct, stated);
        }
        term
    }

    /// Whether `form` holds a remainder the facts were lifted over: a goal that does is not met by their sum, which is over the dividend, and is proved by refuting its negation, lifted as they were.
    pub(super) fn lifted_over(&self, form: &LinearForm) -> bool {
        self.lifts
            .iter()
            .any(|lift| lift.coefficient(form).is_some())
    }
}

/// A case split an operation defines on an order of its operands, with the facts each case adds: a truncated subtraction `a - b` on `b <= a`, a `min` or a `max` over `a` and `b` on `a <= b`, an `abs` on `+0 <= a`, and a `sign` on each of `+0 < a` and `a < +0`. Operations whose guards are one comparison to conversion share one split ([`Reader::open`]).
pub(super) struct Split {
    /// The comparison the proof splits on.
    pub(super) guard: Term,
    /// The guard as the view reads it where it holds and where it fails, by which a second guard is told to be this one or its negation; `None` where the view does not read it.
    read: Option<(LinearForm, LinearForm)>,
    /// Where it holds: the guard, and for a subtraction `b + (a - b) = a` by `le/add_sub_cancel`, for a `min` or a `max` the operand it is there.
    pub(super) holds: Vec<Fact>,
    /// Where it fails: the guard's strict dual, and for a subtraction `a - b <= 0` by `le/sub_zero`, which conversion does not decide there, for a `min` or a `max` the other operand.
    pub(super) fails: Vec<Fact>,
}

/// One reading of a scope: the reader every fact and the goal share, which of the atoms it has read are one, what it has read so far, and the operations whose definitions it has read.
pub(super) struct Reader<'a> {
    views: &'a mut LinearViews,
    classes: &'a mut Classes,
    read: Facts,
    divided: Vec<Division>,
    subtracted: Vec<(Term, Term)>,
    powered: Vec<Term>,
    extrema: Vec<(Extremum, Term, Term)>,
    signed: Vec<Term>,
}

impl<'a> Reader<'a> {
    /// A reader over `views`, classing its atoms among `classes`: every reader of one bound is handed the same two, so the goal, the facts, the negated goal and the facts' products are read over one table of atoms and one partition of them.
    pub(super) fn new(views: &'a mut LinearViews, classes: &'a mut Classes) -> Self {
        Reader {
            views,
            classes,
            read: Facts {
                facts: Vec::new(),
                splits: Vec::new(),
                lifts: Vec::new(),
                unread: Vec::new(),
                spelled: Vec::new(),
            },
            divided: Vec::new(),
            subtracted: Vec::new(),
            powered: Vec::new(),
            extrema: Vec::new(),
            signed: Vec::new(),
        }
    }

    /// The goal `decision` states, or `None` where it is no `Nat` or `Int` `<` or `<=`: an equality's negation is a disjunction, and nothing else is a comparison the view reads. Read first, so its atoms are handed out before any fact's.
    ///
    /// `stated` is the decision as the bound spells it, where opening the bound reaches one: its comparison's operands are the goal's sides and it is what the refuting form splits on, so the refinement the split installs is keyed as the bound reads it. `decision` is its reduct, read where the stated spelling opens to no comparison.
    pub(super) fn target(
        &mut self,
        context: &mut Context,
        stated: Option<&Term>,
        decision: &Term,
    ) -> Result<Option<Target>, Error> {
        let opened = match stated {
            Some(stated) => match opened(context, stated)? {
                Some(Opened::Comparison(comparison)) => Some((comparison, stated.clone())),
                _ => None,
            },
            None => None,
        };
        let (comparison, decision) = match opened {
            Some(opened) => opened,
            None => match &*reduce_with(context, decision)? {
                Subterm::Intrinsic(comparison) => (comparison.clone(), decision.clone()),
                _ => return Ok(None),
            },
        };
        let Some((carrier, relation @ (Operation::Less | Operation::AtMost), left, right)) =
            sides(&comparison)
        else {
            return Ok(None);
        };
        let Some(form) = self.form(context, carrier, relation, &left, &right)? else {
            return Ok(None);
        };
        Ok(Some(Target {
            carrier,
            relation,
            left,
            right,
            form,
            decision,
        }))
    }

    /// Every fact the scope states: the hypotheses, their proof fields and the guards live now, what the operations they are read over define, and every natural monomial's non-negativity, the goal's included.
    pub(super) fn collect(
        mut self,
        context: &mut Context,
        target: Option<&Target>,
    ) -> Result<Facts, Error> {
        curios_profile::profile!("entailment::collect");
        for (name, type_) in binders(context) {
            for (statement, proof, origin) in statements(context, &name, &type_)? {
                self.proposition(context, &statement, proof, origin)?;
            }
        }
        self.guards(context)?;
        self.definitions(context, target)?;
        // Last, so every monomial a fact or the goal was read over has been handed out.
        self.naturals(context, target)?;
        Ok(self.read)
    }

    /// A proposition `proof` inhabits: the facts it states, if it states any — read as a bound is, through the decision its `Holds` is stuck on, or as an equation.
    fn proposition(
        &mut self,
        context: &mut Context,
        type_: &Term,
        proof: Term,
        origin: Origin,
    ) -> Result<(), Error> {
        if let Some(decision) = held(context, type_)? {
            return self.decision(context, &decision, type_, proof, origin);
        }
        let reduced = reduce_with(context, type_)?;
        if let Some(decision) = decision_of(context, &reduced)? {
            return self.decision(context, &decision, type_, proof, origin);
        }
        if is_empty(context, &reduced) {
            return self.refutation(context, proof, origin);
        }
        let equality = Global::Authored(context.syntax().entailment.equality.qualifier());
        if let Subterm::InductType(InductType {
            name,
            params,
            indices,
            ..
        }) = &*reduced
            && *name == equality
            && let ([type_], [left, right]) = (params.as_slice(), indices.as_slice())
            // The type parameter as elaborated names the carrier, `/std/Nat`, which reduces to the intrinsic it re-exports.
            && let Some(carrier) = carrier_of(&reduce_with(context, type_)?)
        {
            self.equation(context, carrier, left, right, proof, origin)?;
        }
        Ok(())
    }

    /// A decision `proof` shows `true`: a comparison, an equality, a range check, or a conjunction of them — at the spelling `decision` states, opened by its heads, and read reduced where that opens to none of them. One that reduces to `false` refutes the scope.
    fn decision(
        &mut self,
        context: &mut Context,
        decision: &Term,
        stated: &Term,
        proof: Term,
        origin: Origin,
    ) -> Result<(), Error> {
        match opened(context, decision)? {
            Some(Opened::Comparison(comparison)) => {
                return self.comparison(context, &comparison, stated, proof, origin);
            }
            Some(Opened::Range(range)) => {
                return self.range(context, range, proof, origin, stated);
            }
            Some(Opened::Conjunction(first, second)) => {
                return self.conjunction(context, &first, &second, stated, proof, origin);
            }
            None => {}
        }
        let reduced = reduce_with(context, decision)?;
        match &*reduced {
            Subterm::Intrinsic(Intrinsic::BoolAnd(first, second)) => {
                self.conjunction(context, first, second, stated, proof, origin)
            }
            Subterm::Intrinsic(Intrinsic::Bool(false)) => self.refutation(context, proof, origin),
            Subterm::Intrinsic(comparison) => {
                self.comparison(context, comparison, stated, proof, origin)
            }
            // A `Bool` function unfolding to a conjunction as `in_range` does: `match c >= lo | true => c <= hi | false => false end`, stuck on the first conjunct.
            Subterm::Match(Match {
                head,
                cases:
                    Cases::Bool {
                        false_case,
                        true_case,
                    },
                ..
            }) if matches!(&**false_case, Subterm::Intrinsic(Intrinsic::Bool(false))) => {
                self.conjunction(context, head, true_case, stated, proof, origin)
            }
            _ => {
                self.read.unread.push((origin, stated.clone()));
                Ok(())
            }
        }
    }

    /// A comparison `proof` shows holds: a bound as it stands, and an equality as the two bounds its equation gives.
    fn comparison(
        &mut self,
        context: &mut Context,
        comparison: &Intrinsic,
        stated: &Term,
        proof: Term,
        origin: Origin,
    ) -> Result<(), Error> {
        match sides(comparison) {
            Some((_, Operation::Less | Operation::AtMost, ..)) => {
                self.admit(context, comparison.clone(), proof, origin)
            }
            Some((carrier, Operation::Equal, left, right))
                if in_scope(context, &[order(context, carrier).eq_of_eql]) =>
            {
                let eq_of_eql = order(context, carrier).eq_of_eql;
                let equation = Term::apply(global(eq_of_eql), [left.clone(), right.clone(), proof]);
                self.equation(context, carrier, &left, &right, equation, origin)
            }
            _ => {
                self.read.unread.push((origin, stated.clone()));
                Ok(())
            }
        }
    }

    /// The conjuncts of a conjunction `proof` holds, each proved by a split on the first: where it is `true` the conjunction is the second, and where it is `false` the conjunction is `false`, so `proof` is eliminated by its match. Each is bound at its own statement, so the split is elaborated against `Holds(conjunct)`.
    ///
    /// Sound only where the split's arms reduce `proof`'s type, which a guard keyed on the conjunction itself stops — so a guard is read by [`Reader::guards`] instead.
    fn conjunction(
        &mut self,
        context: &mut Context,
        first: &Term,
        second: &Term,
        stated: &Term,
        proof: Term,
        origin: Origin,
    ) -> Result<(), Error> {
        let holds = context.syntax().proof.holds;
        let first_statement = Term::apply(global(holds), [first.clone()]);
        let second_statement = Term::apply(global(holds), [second.clone()]);

        let refuted = eliminate(context, proof.clone());
        let first_split = split(context, first, refuted, qed(context));
        let first_proof = bound(context, first_statement.clone(), first_split);
        self.decision(
            context,
            first,
            &first_statement,
            first_proof,
            origin.clone(),
        )?;

        let refuted = eliminate(context, proof.clone());
        let second_split = split(context, first, refuted, proof);
        let second_proof = bound(context, second_statement, second_split);
        self.decision(context, second, stated, second_proof, origin)
    }

    /// An equation `left = right` at `carrier`, proved by `proof`, as the two bounds it gives.
    fn equation(
        &mut self,
        context: &mut Context,
        carrier: Carrier,
        left: &Term,
        right: &Term,
        proof: Term,
        origin: Origin,
    ) -> Result<(), Error> {
        let of_eq = order(context, carrier).of_eq;
        let sym = context.syntax().entailment.sym;
        if !in_scope(context, &[of_eq, sym]) {
            return Ok(());
        }
        let bound = |left: &Term, right: &Term, proof: Term| {
            Term::apply_marked(
                global(of_eq),
                [
                    (Plicity::Implicit, left.clone()),
                    (Plicity::Implicit, right.clone()),
                    (Plicity::Explicit, proof),
                ],
            )
        };
        let reversed = Term::apply_marked(
            global(sym),
            [
                (Plicity::Implicit, carrier_type(carrier)),
                (Plicity::Implicit, left.clone()),
                (Plicity::Implicit, right.clone()),
                (Plicity::Explicit, proof.clone()),
            ],
        );
        let forward = comparison_of(carrier, Operation::AtMost, left, right);
        self.admit(context, forward, bound(left, right, proof), origin.clone())?;
        let backward = comparison_of(carrier, Operation::AtMost, right, left);
        self.admit(context, backward, bound(right, left, reversed), origin)
    }

    /// The guards live now — the arm's at insertion, the birth's at a retry, since the caller installed those.
    ///
    /// **A guard's fact is proved at the spelling reduction gives the guard.** `qed` checks against a guard wherever the arm's equation answers the statement it is given, and an equation answers every term conversion holds equal to its key (`reduce`'s `refined_reduct`), so the fact is written over the guard's reduct, taken with the guard's own refinement withheld so that the arm does not answer `true` for it: `i < m` written through `Cmp` is read as the `Nat/lt` comparison it reduces to. A range check in its true arm is the one guard read from its written spelling, opened by its heads alone ([`opened`]) — `Char/is_upper(c)` to the `in_range` call it makes — because its two bounds are `le/of_in_range`'s to give and no reduct of it. In its false arm it is read by its reduct like any other guard: a comparison where the check is from zero, its lower bound folding away.
    ///
    /// A guard written through a concept is recorded as written and as the dispatch resolves, two spellings of one fact, and is read once.
    fn guards(&mut self, context: &mut Context) -> Result<(), Error> {
        let entries = context
            .visible_scrutinee_entries()
            .map(|(frame, _, entry)| (frame, entry.original.clone(), entry.value.clone()))
            .collect::<Vec<_>>();

        let mut read = Vec::<(usize, Term)>::new();
        for (frame, written, value) in entries {
            let Subterm::Intrinsic(Intrinsic::Bool(case)) = &*value else {
                continue;
            };
            let case = *case;
            let (opened, spelled) = context.with_refinements_withheld_from(frame, |context| {
                Ok::<_, Error>((opened(context, &written)?, reduce_with(context, &written)?))
            })?;
            if read.contains(&(frame, spelled.clone())) {
                continue;
            }
            read.push((frame, spelled.clone()));
            let origin = Origin::Guard(written.clone(), case);
            match (opened, &*spelled) {
                (Some(Opened::Range(range)), _) if case => {
                    let qed = qed(context);
                    self.range(context, range, qed, origin, &written)?;
                }
                (_, Subterm::Intrinsic(comparison)) => {
                    self.guard(context, comparison, case, &written, origin)?;
                }
                _ => self.read.unread.push((origin, written)),
            }
        }
        Ok(())
    }

    /// One comparison guard, by the case it took.
    fn guard(
        &mut self,
        context: &mut Context,
        comparison: &Intrinsic,
        case: bool,
        written: &Term,
        origin: Origin,
    ) -> Result<(), Error> {
        let Some((carrier, relation, left, right)) = sides(comparison) else {
            self.read.unread.push((origin, written.clone()));
            return Ok(());
        };
        let order = order(context, carrier);
        let qed = qed(context);
        match (relation, case) {
            (Operation::Less | Operation::AtMost, true) => {
                self.admit(context, comparison.clone(), qed, origin)
            }
            // A comparison that came back `false` is the other one.
            (Operation::Less, false) if in_scope(context, &[order.of_not_lt]) => {
                let dual = comparison_of(carrier, Operation::AtMost, &right, &left);
                let proof = Term::apply(global(order.of_not_lt), [left, right, qed]);
                self.admit(context, dual, proof, origin)
            }
            (Operation::AtMost, false) if in_scope(context, &[order.of_not_le]) => {
                let dual = comparison_of(carrier, Operation::Less, &right, &left);
                let proof = Term::apply(global(order.of_not_le), [left, right, qed]);
                self.admit(context, dual, proof, origin)
            }
            (Operation::Equal, true) if in_scope(context, &[order.eq_of_eql]) => {
                let equation =
                    Term::apply(global(order.eq_of_eql), [left.clone(), right.clone(), qed]);
                self.equation(context, carrier, &left, &right, equation, origin)
            }
            // An equality's false arm, and an inequality's true one, is a disjunction.
            _ => {
                self.read.unread.push((origin, written.clone()));
                Ok(())
            }
        }
    }

    /// A range check `proof` shows holds: `lo <= c` and `c <= hi`, through `le/of_in_range` over the arguments the check is passed as stated — so a guard's `qed` checks against the check the arm recorded, and a hypothesis is handed to the lemma as it is. The lemma's body was elaborated where no guard stood between the check and its unfolding.
    fn range(
        &mut self,
        context: &mut Context,
        Range { c, lo, hi }: Range,
        proof: Term,
        origin: Origin,
        written: &Term,
    ) -> Result<(), Error> {
        let range = context.syntax().entailment.range;
        if !in_scope(context, &[range]) {
            self.read.unread.push((origin, written.clone()));
            return Ok(());
        }
        let both = Term::apply(global(range), [c.clone(), lo.clone(), hi.clone(), proof]);
        let low = Term::proj(both.clone(), 0);
        self.admit(
            context,
            Intrinsic::NatLe(lo, c.clone()),
            low,
            origin.clone(),
        )?;
        self.admit(
            context,
            Intrinsic::NatLe(c, hi),
            Term::proj(both, 1),
            origin,
        )
    }

    /// What the operations the facts and the goal are read over define, until a reading meets no new one: a quotient's bounds hold its dividend, which may be a truncated subtraction, a subtraction's cases hold its operands, which may be quotients, and any of them may hold a power, a `min` or a `max`, whose own operands may be any of them.
    fn definitions(&mut self, context: &mut Context, target: Option<&Target>) -> Result<(), Error> {
        loop {
            let divided = self.quotients(context, target)?;
            let subtracted = self.subtractions(context, target)?;
            let powered = self.powers(context, target)?;
            let split = self.extrema(context, target)?;
            let signed = self.signs(context, target)?;
            if !divided && !subtracted && !powered && !split && !signed {
                return Ok(());
            }
        }
    }

    /// Each `min` and `max` among the atoms read so far, at `Nat` and at `Int`, through the cases its operands' order makes: `a <= b`, where `min(a, b) = a` and `max(a, b) = b`, and `b < a`, where each is the other operand. A case the scope already decides is read as facts; otherwise the cases are kept as a split the search opens where it needs it, as a truncated subtraction's are. Whether it read one.
    ///
    /// Conversion decides a `min` and a `max` from their operands as terms and reads no fact, so this is where a bound over one meets what the scope says of its operands.
    fn extrema(&mut self, context: &mut Context, target: Option<&Target>) -> Result<bool, Error> {
        let mut pending: Vec<(Extremum, Term, Term)> = Vec::new();
        for atom in self.atoms(target) {
            let Subterm::Intrinsic(intrinsic) = &**self.views.term(atom) else {
                continue;
            };
            let Some((extremum, a, b)) = Extremum::of(intrinsic) else {
                continue;
            };
            let node = (extremum, a.clone(), b.clone());
            if !self.extrema.contains(&node) && !pending.contains(&node) {
                pending.push(node);
            }
        }

        let mut read = false;
        for (extremum, a, b) in pending {
            let order = order(context, extremum.carrier);
            let names = [
                order.min_of_le,
                order.min_of_ge,
                order.max_of_le,
                order.max_of_ge,
                order.of_lt,
                order.of_not_le,
                order.of_eq,
                context.syntax().entailment.sym,
            ];
            if !in_scope(context, &names) {
                continue;
            }
            self.extrema.push((extremum, a.clone(), b.clone()));
            read = true;

            let guard = Term::intrinsic(comparison_of(extremum.carrier, Operation::AtMost, &a, &b));
            match &*reduce_with(context, &guard)? {
                Subterm::Intrinsic(Intrinsic::Bool(true)) => {
                    self.kept(context, extremum, &a, &b, true)?
                }
                Subterm::Intrinsic(Intrinsic::Bool(false)) => {
                    self.kept(context, extremum, &a, &b, false)?
                }
                _ => {
                    let holds = self.apart(context, |reader, context| {
                        reader.kept(context, extremum, &a, &b, true)
                    })?;
                    let fails = self.apart(context, |reader, context| {
                        reader.kept(context, extremum, &a, &b, false)
                    })?;
                    self.open(context, guard, holds, fails)?;
                }
            }
        }
        Ok(read)
    }

    /// One case of a `min` or a `max` over `a` and `b`. Where `a <= b` holds: that, and the operation as the operand it is there, by the carrier's `min/of_le` or `max/of_le`. Where it fails: `b < a`, and the operation as the other operand, by `min/of_ge` or `max/of_ge` over the bound `le/of_lt` loosens.
    fn kept(
        &mut self,
        context: &mut Context,
        extremum: Extremum,
        a: &Term,
        b: &Term,
        holds: bool,
    ) -> Result<(), Error> {
        let order = order(context, extremum.carrier);
        let node = Term::intrinsic(extremum.node(a.clone(), b.clone()));
        match holds {
            true => {
                let qed = qed(context);
                let guard = comparison_of(extremum.carrier, Operation::AtMost, a, b);
                self.admit(context, guard, qed.clone(), Origin::Definition)?;
                // `a <= b`: a `min` is `a` and a `max` is `b`.
                let (lemma, kept) = match extremum.lattice {
                    Lattice::Meet => (order.min_of_le, a),
                    Lattice::Join => (order.max_of_le, b),
                };
                let equation = Term::apply(global(lemma), [a.clone(), b.clone(), qed]);
                let origin = Origin::Definition;
                self.equation(context, extremum.carrier, &node, kept, equation, origin)
            }
            false => {
                let failed = [a.clone(), b.clone(), qed(context)];
                let below = Term::apply(global(order.of_not_le), failed);
                let guard = comparison_of(extremum.carrier, Operation::Less, b, a);
                self.admit(context, guard, below.clone(), Origin::Definition)?;
                let loosened = Term::apply(global(order.of_lt), [b.clone(), a.clone(), below]);
                // `b <= a`: a `min` is `b` and a `max` is `a`.
                let (lemma, kept) = match extremum.lattice {
                    Lattice::Meet => (order.min_of_ge, b),
                    Lattice::Join => (order.max_of_ge, a),
                };
                let equation = Term::apply(global(lemma), [a.clone(), b.clone(), loosened]);
                let origin = Origin::Definition;
                self.equation(context, extremum.carrier, &node, kept, equation, origin)
            }
        }
    }

    /// Each `abs` and `sign` of an `Int` among the atoms read so far, through the cases its operand's sign makes. An `abs` splits on `+0 <= a`: where it holds the magnitude widened is `a`, and where it fails it is `a` negated. A `sign` splits twice, on `+0 < a` and on `a < +0`: where either holds the sign is `+1` or `-1`, and where one fails the sign is at most or at least zero, so where both fail it is zero. Whether it read one.
    ///
    /// Conversion decides each from its operand as a term and reads no fact, so this is where a bound over one meets what the scope says of its operand.
    fn signs(&mut self, context: &mut Context, target: Option<&Target>) -> Result<bool, Error> {
        let integer = context.syntax().entailment.integer;
        let order = order(context, Carrier::Integer);
        let names = [
            integer.abs_of_non_neg,
            integer.abs_of_neg,
            integer.sign_of_pos,
            integer.sign_of_not_pos,
            integer.sign_of_neg,
            integer.sign_of_not_neg,
            order.of_not_le,
            order.of_not_lt,
            order.of_eq,
            context.syntax().entailment.sym,
        ];
        if !in_scope(context, &names) {
            return Ok(false);
        }

        let mut pending: Vec<Term> = Vec::new();
        for atom in self.atoms(target) {
            let node = self.views.term(atom).clone();
            let signed = matches!(
                &*node,
                Subterm::Intrinsic(Intrinsic::IntAbs(_) | Intrinsic::IntSign(_))
            );
            if signed && !self.signed.contains(&node) && !pending.contains(&node) {
                pending.push(node);
            }
        }

        let read = !pending.is_empty();
        let zero = literal(Carrier::Integer, 0u32);
        for node in pending {
            self.signed.push(node.clone());
            match &*node {
                Subterm::Intrinsic(Intrinsic::IntAbs(a)) => {
                    let guard = Intrinsic::IntLe(zero.clone(), a.clone());
                    self.cases(context, guard, |reader, context, holds| {
                        reader.magnitude(context, a, holds)
                    })?;
                }
                Subterm::Intrinsic(Intrinsic::IntSign(a)) => {
                    let above = Intrinsic::IntLt(zero.clone(), a.clone());
                    self.cases(context, above, |reader, context, holds| {
                        reader.sign(context, a, true, holds)
                    })?;
                    let below = Intrinsic::IntLt(a.clone(), zero.clone());
                    self.cases(context, below, |reader, context, holds| {
                        reader.sign(context, a, false, holds)
                    })?;
                }
                _ => {}
            }
        }
        Ok(read)
    }

    /// The two cases of `guard`, each read by `case` under whether the guard holds: as facts where the scope decides the guard, and otherwise as a split the search opens where it needs it.
    fn cases(
        &mut self,
        context: &mut Context,
        guard: Intrinsic,
        mut case: impl FnMut(&mut Self, &mut Context, bool) -> Result<(), Error>,
    ) -> Result<(), Error> {
        let guard = Term::intrinsic(guard);
        match &*reduce_with(context, &guard)? {
            Subterm::Intrinsic(Intrinsic::Bool(holds)) => case(self, context, *holds),
            _ => {
                let holds = self.apart(context, |reader, context| case(reader, context, true))?;
                let fails = self.apart(context, |reader, context| case(reader, context, false))?;
                self.open(context, guard, holds, fails)
            }
        }
    }

    /// One case of `abs(a)` on `+0 <= a`. Where it holds: that, and `Nat/to_int(abs(a)) = a` by `Int/abs/of_non_neg`. Where it fails: `a < +0`, and `Nat/to_int(abs(a)) = +0 - a` by `Int/abs/of_neg`.
    fn magnitude(&mut self, context: &mut Context, a: &Term, holds: bool) -> Result<(), Error> {
        let integer = context.syntax().entailment.integer;
        let zero = literal(Carrier::Integer, 0u32);
        let widened = Term::intrinsic(Intrinsic::NatToInt(Term::intrinsic(Intrinsic::IntAbs(
            a.clone(),
        ))));
        let origin = Origin::Definition;
        match holds {
            true => {
                let qed = qed(context);
                let guard = Intrinsic::IntLe(zero, a.clone());
                self.admit(context, guard, qed.clone(), Origin::Definition)?;
                let equation = Term::apply(global(integer.abs_of_non_neg), [a.clone(), qed]);
                self.equation(context, Carrier::Integer, &widened, a, equation, origin)
            }
            false => {
                let of_not_le = order(context, Carrier::Integer).of_not_le;
                let below = Term::apply(global(of_not_le), [zero.clone(), a.clone(), qed(context)]);
                let guard = Intrinsic::IntLt(a.clone(), zero.clone());
                self.admit(context, guard, below.clone(), Origin::Definition)?;
                let equation = Term::apply(global(integer.abs_of_neg), [a.clone(), below]);
                let negated = Term::intrinsic(Intrinsic::IntSub(zero, a.clone()));
                self.equation(
                    context,
                    Carrier::Integer,
                    &widened,
                    &negated,
                    equation,
                    origin,
                )
            }
        }
    }

    /// One case of `sign(a)` on its operand standing `above` zero, `+0 < a`, or below it, `a < +0`. Where the guard holds: that, and `sign(a)` as `+1` or `-1`. Where it fails: the loose bound the other way, and `sign(a)` at most or at least zero by it.
    fn sign(
        &mut self,
        context: &mut Context,
        a: &Term,
        above: bool,
        holds: bool,
    ) -> Result<(), Error> {
        let integer = context.syntax().entailment.integer;
        let zero = literal(Carrier::Integer, 0u32);
        let node = Term::intrinsic(Intrinsic::IntSign(a.clone()));
        // The guard's sides in order, and the lemmas of its two cases.
        let (lower, upper, decided, left) = match above {
            true => (&zero, a, integer.sign_of_pos, integer.sign_of_not_pos),
            false => (a, &zero, integer.sign_of_neg, integer.sign_of_not_neg),
        };
        match holds {
            true => {
                let qed = qed(context);
                let guard = Intrinsic::IntLt(lower.clone(), upper.clone());
                self.admit(context, guard, qed.clone(), Origin::Definition)?;
                let equation = Term::apply(global(decided), [a.clone(), qed]);
                let value = match above {
                    true => literal(Carrier::Integer, 1u32),
                    false => Term::intrinsic(Intrinsic::Int(Integer::from(-1i32))),
                };
                let origin = Origin::Definition;
                self.equation(context, Carrier::Integer, &node, &value, equation, origin)
            }
            false => {
                let of_not_lt = order(context, Carrier::Integer).of_not_lt;
                let failed = [lower.clone(), upper.clone(), qed(context)];
                let loose = Term::apply(global(of_not_lt), failed);
                let guard = Intrinsic::IntLe(upper.clone(), lower.clone());
                self.admit(context, guard, loose.clone(), Origin::Definition)?;
                let bound = Term::apply(global(left), [a.clone(), loose]);
                let stated = match above {
                    true => Intrinsic::IntLe(node, zero.clone()),
                    false => Intrinsic::IntLe(zero.clone(), node),
                };
                self.admit(context, stated, bound, Origin::Definition)
            }
        }
    }

    /// That each power among the atoms read so far is at least one, where conversion decides it: `1 <= pow(b, m)` at a base it holds at least one, as every `pow(2, m)` is, so `qed` proves it. Whether it read one.
    ///
    /// A shift by a symbolic count reads as its value times a power of two, so this is the fact a bound over one needs beside what the scope says of the value: `0 < d` gives `0 < shl(d, k)` as the product of two positives.
    fn powers(&mut self, context: &mut Context, target: Option<&Target>) -> Result<bool, Error> {
        let one = literal(Carrier::Natural, 1u32);
        let mut read = false;
        for atom in self.atoms(target) {
            let power = self.views.term(atom).clone();
            let powered = matches!(&*power, Subterm::Intrinsic(Intrinsic::NatPow(..)));
            if !powered || self.powered.contains(&power) {
                continue;
            }
            self.powered.push(power.clone());
            let at_least = Intrinsic::NatLe(one.clone(), power);
            let decided = reduce_with(context, &Term::intrinsic(at_least.clone()))?;
            if !matches!(&*decided, Subterm::Intrinsic(Intrinsic::Bool(true))) {
                continue;
            }
            read = true;
            self.admit(context, at_least, qed(context), Origin::Definition)?;
        }
        Ok(read)
    }

    /// The bounds of each quotient and remainder among the atoms read so far — `d * (x / d) <= x` and `x < d * (x / d) + d` — with the remainder lifted out of every fact ([`Lift`]). Whether it read one.
    ///
    /// The bounds are `le/add_r(d * (x / d), x % d)` and `lt/add_mul_lt(x / d, x % d, x / d + 1, d, qed, qed)`, whose types hold the remainder beside its quotient's multiple, which conversion reads as the dividend; their stated sides hold neither. The second asks for `x % d < d`, which conversion decides at any divisor, where it decides the aligned `x % d + 1 <= d` only at a literal. A division is read only once no other still to be read divides its remainder, so the bounds of `(x % 4096) / 64`, which hold `x % 4096`, are read before that remainder is lifted out of them. Where the divisor is no literal its multiple is a product, which linear arithmetic reads as an unknown of its own and [`products`] relate to its factors.
    ///
    /// `Nat` alone: at `Int`, conversion decides Euclid's identity and neither of a remainder's bounds.
    fn quotients(&mut self, context: &mut Context, target: Option<&Target>) -> Result<bool, Error> {
        let natural = context.syntax().entailment.natural;
        if !in_scope(context, &[natural.below, natural.above, natural.shift]) {
            return Ok(false);
        }
        let mut read = false;
        loop {
            let mut pending: Vec<Division> = Vec::new();
            for atom in self.atoms(target) {
                if let Some(division) = Division::of(self.views.term(atom))
                    && !self.divided.contains(&division)
                    && !pending.contains(&division)
                {
                    pending.push(division);
                }
            }
            let next = pending.iter().find(|division| {
                let remainder = division.remainder();
                !pending
                    .iter()
                    .any(|other| other.dividend.mentions_term(&remainder))
            });
            let Some(division) = next.cloned() else {
                return Ok(read);
            };
            self.divided.push(division.clone());
            read = true;
            // A remainder that does not read as an atom of its own is one conversion computes, and bounds held beside it would be summed as it reads them.
            let Some(lift) = self.lift(context, &division)? else {
                continue;
            };

            let (multiple, remainder) = (division.multiple(), division.remainder());
            let below = Term::apply(global(natural.below), [multiple.clone(), remainder.clone()]);
            let floor = Intrinsic::NatLe(multiple.clone(), division.dividend.clone());
            self.admit(context, floor, below, Origin::Definition)?;
            let quotient = division.quotient();
            let above = Term::apply(
                global(natural.above),
                [
                    quotient.clone(),
                    remainder,
                    successor(Carrier::Natural, quotient),
                    division.divisor.clone(),
                    qed(context),
                    qed(context),
                ],
            );
            let ceiling = Term::intrinsic(Intrinsic::NatAdd(multiple, division.divisor.clone()));
            let ceiling = Intrinsic::NatLt(division.dividend.clone(), ceiling);
            self.admit(context, ceiling, above, Origin::Definition)?;

            let cases = self
                .read
                .splits
                .iter_mut()
                .flat_map(|split| split.holds.iter_mut().chain(&mut split.fails));
            for fact in self.read.facts.iter_mut().chain(cases) {
                lift.apply(fact);
            }
            self.read.lifts.push(lift);
        }
    }

    /// What lifting `division`'s remainder out of a fact takes: the remainder's monomial, and what it denotes — `x - d * (x / d)`, the form of `x <= d * (x / d)`. `None` where the remainder does not read as one atom.
    fn lift(&mut self, context: &mut Context, division: &Division) -> Result<Option<Lift>, Error> {
        let zero = literal(Carrier::Natural, 0u32);
        let Some(remainder) = self.form_of(context, &division.remainder(), &zero)? else {
            return Ok(None);
        };
        let [(coefficient, monomial)] = remainder.terms.as_slice() else {
            return Ok(None);
        };
        if *coefficient != Integer::from(1) || !remainder.constant.is_zero() {
            return Ok(None);
        }
        let monomial = monomial.clone();
        let Some(denotes) = self.form_of(context, &division.dividend, &division.multiple())? else {
            return Ok(None);
        };
        Ok(Some(Lift {
            remainder: monomial,
            denotes,
            multiple: division.multiple(),
            shift: context.syntax().entailment.natural.shift,
        }))
    }

    /// Each truncated subtraction `a - b` among the atoms read so far, through the cases its definition makes: `b <= a`, where `b + (a - b) = a`, and `a < b`, where `a - b <= 0`. A case the scope already decides — under a guard, or where conversion decides `b <= a` — is read as facts; otherwise the cases are kept as a split the search opens where it needs one. Whether it read one.
    fn subtractions(
        &mut self,
        context: &mut Context,
        target: Option<&Target>,
    ) -> Result<bool, Error> {
        let natural = context.syntax().entailment.natural;
        let order = order(context, Carrier::Natural);
        let names = [
            natural.difference,
            natural.truncated,
            order.of_lt,
            order.of_not_le,
            order.of_eq,
            context.syntax().entailment.sym,
        ];
        if !in_scope(context, &names) {
            return Ok(false);
        }
        let mut pending: Vec<(Term, Term)> = Vec::new();
        for atom in self.atoms(target) {
            if let Subterm::Intrinsic(Intrinsic::NatSub(a, b)) = &**self.views.term(atom) {
                let operands = (a.clone(), b.clone());
                if !self.subtracted.contains(&operands) && !pending.contains(&operands) {
                    pending.push(operands);
                }
            }
        }

        let read = !pending.is_empty();
        for (a, b) in pending {
            self.subtracted.push((a.clone(), b.clone()));
            let guard = Term::intrinsic(Intrinsic::NatLe(b.clone(), a.clone()));
            match &*reduce_with(context, &guard)? {
                Subterm::Intrinsic(Intrinsic::Bool(true)) => self.exact(context, &a, &b)?,
                Subterm::Intrinsic(Intrinsic::Bool(false)) => self.truncated(context, &a, &b)?,
                _ => {
                    let holds =
                        self.apart(context, |reader, context| reader.exact(context, &a, &b))?;
                    let fails =
                        self.apart(context, |reader, context| reader.truncated(context, &a, &b))?;
                    self.open(context, guard, holds, fails)?;
                }
            }
        }
        Ok(read)
    }

    /// The case of `a - b` that does not truncate: `b <= a`, and `b + (a - b) = a` by `le/add_sub_cancel`, each proved from `b <= a` by `qed` where the scope decides it.
    fn exact(&mut self, context: &mut Context, a: &Term, b: &Term) -> Result<(), Error> {
        let difference = context.syntax().entailment.natural.difference;
        let qed = qed(context);
        let guard = Intrinsic::NatLe(b.clone(), a.clone());
        self.admit(context, guard, qed.clone(), Origin::Definition)?;
        let cancelled = Term::apply(global(difference), [b.clone(), a.clone(), qed]);
        let subtracted = Term::intrinsic(Intrinsic::NatSub(a.clone(), b.clone()));
        let summed = Term::intrinsic(Intrinsic::NatAdd(b.clone(), subtracted));
        let origin = Origin::Definition;
        self.equation(context, Carrier::Natural, &summed, a, cancelled, origin)
    }

    /// The case that truncates: `a < b`, by `lt/of_not_le` where the scope decides `b <= a` fails, and `a - b <= 0` by `le/sub_zero`.
    fn truncated(&mut self, context: &mut Context, a: &Term, b: &Term) -> Result<(), Error> {
        let natural = context.syntax().entailment.natural;
        let of_not_le = order(context, Carrier::Natural).of_not_le;
        let below = Term::apply(global(of_not_le), [b.clone(), a.clone(), qed(context)]);
        let guard = Intrinsic::NatLt(a.clone(), b.clone());
        self.admit(context, guard, below.clone(), Origin::Definition)?;
        let of_lt = order(context, Carrier::Natural).of_lt;
        let loosened = Term::apply(global(of_lt), [a.clone(), b.clone(), below]);
        let zero = Term::apply(global(natural.truncated), [a.clone(), b.clone(), loosened]);
        let subtracted = Term::intrinsic(Intrinsic::NatSub(a.clone(), b.clone()));
        let truncation = Intrinsic::NatLe(subtracted, literal(Carrier::Natural, 0u32));
        self.admit(context, truncation, zero, Origin::Definition)
    }

    /// The split on `guard`, with the facts each of its cases adds: opened, or joined to the split already open on the comparison the view reads `guard` as, its cases exchanged where the view reads `guard` as that comparison's negation.
    ///
    /// **Two guards that are one comparison to conversion are one split.** An arm of a split on `a + c <= b + c` decides `a <= b`: the equation the arm records answers every term conversion holds equal to its scrutinee, or to its negation. A second split opened inside it would have a case no value reaches, and there its guard's fact is not `qed`'s to prove, the outer equation answering the inner guard. So a `min` and a `max` over one pair, two over pairs one summand apart, and a truncated subtraction over the same operands share one split, and what the search opens for one it has for all.
    fn open(
        &mut self,
        context: &mut Context,
        guard: Term,
        holds: Vec<Fact>,
        fails: Vec<Fact>,
    ) -> Result<(), Error> {
        let read = self.decided(context, &guard)?;
        let joined = read.as_ref().and_then(|(held, failed)| {
            self.read.splits.iter_mut().find_map(|split| {
                let (known_held, known_failed) = split.read.as_ref()?;
                if known_held == held && known_failed == failed {
                    Some((split, false))
                } else if known_held == failed && known_failed == held {
                    Some((split, true))
                } else {
                    None
                }
            })
        });
        match joined {
            Some((split, false)) => {
                split.holds.extend(holds);
                split.fails.extend(fails);
            }
            Some((split, true)) => {
                split.holds.extend(fails);
                split.fails.extend(holds);
            }
            None => self.read.splits.push(Split {
                guard,
                read,
                holds,
                fails,
            }),
        }
        Ok(())
    }

    /// `guard` as the view reads it where it holds and where it fails — a `<` or a `<=` and its dual with the operands exchanged — or `None` where it is no comparison the view reads.
    fn decided(
        &mut self,
        context: &mut Context,
        guard: &Term,
    ) -> Result<Option<(LinearForm, LinearForm)>, Error> {
        let Subterm::Intrinsic(comparison) = &**guard else {
            return Ok(None);
        };
        let Some((carrier, relation, left, right)) = sides(comparison) else {
            return Ok(None);
        };
        let dual = match relation {
            Operation::Less => Operation::AtMost,
            Operation::AtMost => Operation::Less,
            _ => return Ok(None),
        };
        let held = self.form(context, carrier, relation, &left, &right)?;
        let failed = self.form(context, carrier, dual, &right, &left)?;
        Ok(held.zip(failed))
    }

    /// The facts `read` admits, kept apart from the scope's: one case of a split, lifted as every fact is.
    fn apart(
        &mut self,
        context: &mut Context,
        read: impl FnOnce(&mut Self, &mut Context) -> Result<(), Error>,
    ) -> Result<Vec<Fact>, Error> {
        let scope = std::mem::take(&mut self.read.facts);
        let result = read(self, context);
        let case = std::mem::replace(&mut self.read.facts, scope);
        result.map(|()| case)
    }

    /// Every atom the facts, their cases and `target` were read over, in the order they were handed out.
    fn atoms(&self, target: Option<&Target>) -> Vec<curios_algebra::Atom> {
        let mut atoms: Vec<curios_algebra::Atom> = Vec::new();
        for form in self.forms(target) {
            for (_, monomial) in &form.terms {
                for atom in monomial.atoms() {
                    if !atoms.contains(atom) {
                        atoms.push(*atom);
                    }
                }
            }
        }
        atoms.sort_by_key(|atom| atom.index());
        atoms
    }

    /// The forms of the facts, of every case of every split, and of `target`.
    fn forms<'b>(&'b self, target: Option<&'b Target>) -> impl Iterator<Item = &'b LinearForm> {
        let cases = self
            .read
            .splits
            .iter()
            .flat_map(|split| split.holds.iter().chain(&split.fails));
        self.read
            .facts
            .iter()
            .chain(cases)
            .map(|fact| &fact.form)
            .chain(target.map(|target| &target.form))
    }

    /// The form of `left <= right` at `Nat`, read as [`Reader::admit`] reads a fact's: its operands reduced and the comparison not.
    fn form_of(
        &mut self,
        context: &mut Context,
        left: &Term,
        right: &Term,
    ) -> Result<Option<LinearForm>, Error> {
        let read = Intrinsic::NatLe(
            self.side(context, Carrier::Natural, left)?,
            self.side(context, Carrier::Natural, right)?,
        );
        Ok(self
            .views
            .read(&read)
            .map(|(_, view)| view.form().clone().aligned(view.relation()).1))
    }

    /// That every monomial over naturals is at least zero — clause 2 of what conversion decides, so `True/qed()` proves it — for every such monomial a fact, a case or the goal was read over: a goal over an atom no fact names still needs that atom's sign.
    fn naturals(&mut self, context: &mut Context, target: Option<&Target>) -> Result<(), Error> {
        let mut monomials: Vec<Monomial> = Vec::new();
        for form in self.forms(target) {
            for (_, monomial) in &form.terms {
                let natural = monomial
                    .atoms()
                    .iter()
                    .all(|atom| form.nonnegative.contains(atom));
                if natural && !monomials.contains(monomial) {
                    monomials.push(monomial.clone());
                }
            }
        }
        for monomial in monomials {
            let term = monomial
                .atoms()
                .iter()
                .map(|atom| self.views.term(*atom).clone())
                .reduce(|left, right| Term::intrinsic(Intrinsic::NatMul(left, right)))
                .expect("a monomial stands on an atom");
            let statement = Intrinsic::NatLe(literal(Carrier::Natural, 0u32), term);
            self.admit(context, statement, qed(context), Origin::Natural)?;
        }
        Ok(())
    }

    /// A proof of an empty proposition, which refutes the scope by itself: read as the fact `1 <= 0`, proved by eliminating it with a zero-arm match elaborated against that statement.
    fn refutation(
        &mut self,
        context: &mut Context,
        proof: Term,
        origin: Origin,
    ) -> Result<(), Error> {
        let statement = Intrinsic::NatLe(
            literal(Carrier::Natural, 1u32),
            literal(Carrier::Natural, 0u32),
        );
        let holds = context.syntax().proof.holds;
        let stated = Term::apply(global(holds), [Term::intrinsic(statement.clone())]);
        let refuted = eliminate(context, proof);
        let proof = bound(context, stated, refuted);
        self.admit(context, statement, proof, origin)
    }

    /// Admit the fact `proof` proves of `statement`, its operands read reduced — in the fold's normal form, a local definition unfolded, a refined variable read as what its arm refined it to — so its view is the one conversion computes; nothing where `statement` is no `Nat` or `Int` `<` or `<=`.
    ///
    /// The operands are reduced and the comparison is not: a guard's own comparison, and a natural's non-negativity, would reduce to `true` under the arm's refinement and conversion's decision, and a fact decided away is a fact lost. Every remainder lifted so far is lifted out of it, as out of every fact before it.
    fn admit(
        &mut self,
        context: &mut Context,
        statement: Intrinsic,
        proof: Term,
        origin: Origin,
    ) -> Result<(), Error> {
        let stated = Term::intrinsic(statement.clone());
        let Some((carrier, relation @ (Operation::Less | Operation::AtMost), left, right)) =
            sides(&statement)
        else {
            return Ok(());
        };
        let Some(form) = self.form(context, carrier, relation, &left, &right)? else {
            return Ok(());
        };
        // A strict fact is the loose one of its successor, which is the spelling the lemmas take; the stated sides are the proof's, not the reduct's.
        let left = match relation {
            Operation::Less => successor(carrier, left),
            _ => left,
        };
        let mut fact = Fact {
            origin,
            stated,
            carrier,
            left,
            right,
            form,
            proof,
        };
        for lift in &self.read.lifts {
            lift.apply(&mut fact);
        }
        self.read.facts.push(fact);
        Ok(())
    }

    /// The form of `left ⋈ right` at `carrier`, aligned to `<=`: its operands read reduced — in the fold's normal form, a local definition unfolded, a refined variable read as what its arm refined it to, an atom as the one conversion holds it one with, a product of two sums distributed ([`Reader::side`]) — so its view is the one conversion computes, and each reduct recorded beside the spelling it was read from ([`Reader::spell`]). `None` where the view does not read it.
    fn form(
        &mut self,
        context: &mut Context,
        carrier: Carrier,
        relation: Operation,
        left: &Term,
        right: &Term,
    ) -> Result<Option<LinearForm>, Error> {
        self.spell(context, left)?;
        self.spell(context, right)?;
        let read = comparison_of(
            carrier,
            relation,
            &self.side(context, carrier, left)?,
            &self.side(context, carrier, right)?,
        );
        let Some((_, view)) = self.views.read(&read) else {
            return Ok(None);
        };
        let (aligned, form) = view.form().clone().aligned(view.relation());
        Ok((aligned == Operation::AtMost).then_some(form))
    }

    /// Each leaf of `stated` — what is no intrinsic, walked to through the intrinsics around it — recorded beside its reduct where the two differ, for [`Facts::respell`] to write what is built from the reduct over the leaf.
    fn spell(&mut self, context: &mut Context, stated: &Term) -> Result<(), Error> {
        if let Subterm::Intrinsic(intrinsic) = &**stated {
            for operand in intrinsic.operands() {
                self.spell(context, operand)?;
            }
            return Ok(());
        }
        let reduct = solved(context, stated)?;
        if reduct != *stated && !self.read.spelled.iter().any(|(known, _)| *known == reduct) {
            self.read.spelled.push((reduct, stated.clone()));
        }
        Ok(())
    }

    /// One side of a comparison at `carrier` as the view reads it: its reading ([`Reader::reading`]) with every product of two sums distributed. The fold leaves such a product standing until a comparison asks, and the view distributes none and reads a standing product's sums as two atoms, so the reader asks by name, as a comparison does and as conversion's own linear solving does.
    fn side(
        &mut self,
        context: &mut Context,
        carrier: Carrier,
        term: &Term,
    ) -> Result<Term, Error> {
        let read = self.reading(context, term)?;
        let distributed = match carrier {
            Carrier::Natural => Nat::normalize(context, read.clone()),
            _ => int_normalize(context, read.clone()),
        };
        match distributed.probed() {
            Ok(distributed) => Ok(distributed.unwrap_or(read)),
            Err(error) => Err(Error::from_reduce(error, |refusal| {
                Error::reduce_exhausted(term.clone(), refusal)
            })),
        }
    }

    /// `term` as the view reads it: reduced, the reduct with the metavariables elaboration has solved substituted, and each of its atoms spelled as the first atom read that the elaborator's conversion holds it one with, nothing committed (`same_uncommitted`).
    ///
    /// The view pairs atoms by identity, and which atoms are one is conversion's to say (`documentation/design/arithmetic/a-term-is-one-where-conversion-says-so.md`): a call whose argument one statement writes as a sum and another reads as the successor an arm refined a variable to is one number, and reduction opens neither inside the stuck call. So the atoms of every operand are classed among those read before it, and the operand is handed to the view one spelling to a class, as the conversion chain hands a pair's to its readers. The substitution comes first, as the chain prepares a pair: an operator's witness stands in a signature as the metavariable its resolution solved and in a bound as the witness itself, and a recursive member's slot stands where its name does, which is a spelling and no question. A comparison with no value at the type level is that the two are not known to be one.
    fn reading(&mut self, context: &mut Context, term: &Term) -> Result<Term, Error> {
        curios_profile::profile!("entailment::reading");
        let solved = solved(context, term)?;
        let classes = &mut *self.classes;
        let classed = atoms_in(context, &solved)
            .and_then(|atoms| {
                classing_among(context, classes, &atoms, |context, this, that| {
                    Ok(same_uncommitted(context, this, that)
                        .probed()?
                        .unwrap_or(false))
                })
            })
            .and_then(|()| classed_in(context, &solved, classes));
        classed.map_err(|error| {
            Error::from_reduce(error, |refusal| {
                Error::reduce_exhausted(term.clone(), refusal)
            })
        })
    }
}

/// `term` reduced, and the reduct with the metavariables elaboration has solved substituted: an operand's own reduct, before its atoms are classed, which is what [`Facts::respell`] writes the operand in place of.
fn solved(context: &mut Context, term: &Term) -> Result<Term, Error> {
    let reduced = reduce_with(context, term)?;
    Ok(zonk_solved_term_metas(context, &reduced))
}

/// The goal's negation, a fact of the refuting form's false arm, where a split on the goal's decision refined it to `false`: `a < b` failing is `b <= a`, `a <= b` failing is `b < a`. Read by the same reader, after the facts, proved by `qed` there, and lifted by `lifts` as they were. `None` where the vocabulary is not in scope.
/// The hole's own binders, in scope order: a global of every mounted unit is an assumption in the base frame too, and none is a fact about this scope.
pub(super) fn binders(context: &Context) -> Vec<(Free, Term)> {
    context
        .locals()
        .iter()
        .filter(|(name, _)| name.as_global().is_none())
        .cloned()
        .collect()
}

/// What a hypothesis states, each statement beside the proof that inhabits it and where it came from: the hypothesis itself, or the fields of a tuple or a structure, one level down and in their order.
pub(super) fn statements(
    context: &mut Context,
    name: &Free,
    type_: &Term,
) -> Result<Vec<(Term, Term, Origin)>, Error> {
    let reduced = reduce_with(context, type_)?;
    let telescope = match &*reduced {
        Subterm::TupleType(TupleType { telescope }) => telescope.clone(),
        Subterm::StructType(StructType {
            name: family,
            universes,
            params,
        }) => {
            let Some(decl) = context.struct_decl(family).cloned() else {
                return Ok(Vec::new());
            };
            // `elaborate_proj`'s own rule: a proof the procedure writes reaches no field its author could not.
            if !decl.rep_public
                && context
                    .island()
                    .is_some_and(|island| !island.is_within(&decl.module))
            {
                return Ok(Vec::new());
            }
            let arity = context.instantiate_universe_bound_at(
                &decl.universe_context,
                &decl.arity,
                universes,
            )?;
            arity.open(&params.iter().collect::<Vec<_>>())
        }
        _ => {
            let hypothesis = Term::free_var(name);
            return Ok(vec![(
                type_.clone(),
                hypothesis.clone(),
                Origin::Hypothesis(hypothesis),
            )]);
        }
    };
    let mut stated = Vec::with_capacity(telescope.len());
    let mut cursor = telescope.cursor();
    let mut index = 0;
    while let Some((label, domain)) = cursor.entry() {
        let field = Term::proj(Term::free_var(name), index);
        let label = label.map_or_else(|| index.to_string(), str::to_string);
        stated.push((
            domain,
            field.clone(),
            Origin::Field(Term::free_var(name), label),
        ));
        cursor.advance(field);
        index += 1;
    }
    Ok(stated)
}

pub(super) fn negated(
    context: &mut Context,
    views: &mut LinearViews,
    classes: &mut Classes,
    target: &Target,
    lifts: &[Lift],
) -> Result<Option<Fact>, Error> {
    let order = order(context, target.carrier);
    let (name, statement) = match target.relation {
        Operation::Less => (
            order.of_not_lt,
            comparison_of(
                target.carrier,
                Operation::AtMost,
                &target.right,
                &target.left,
            ),
        ),
        _ => (
            order.of_not_le,
            comparison_of(target.carrier, Operation::Less, &target.right, &target.left),
        ),
    };
    if !in_scope(context, &[name]) {
        return Ok(None);
    }
    let arguments = [target.left.clone(), target.right.clone(), qed(context)];
    let proof = Term::apply(global(name), arguments);
    let mut reader = Reader::new(views, classes);
    reader.admit(context, statement, proof, Origin::Negated)?;
    let mut negation = reader.read.facts.pop();
    for lift in lifts {
        negation.iter_mut().for_each(|fact| lift.apply(fact));
    }
    Ok(negation)
}

/// The product of each pair of `facts` and the negated goal, through `le/mul`: `a <= b` and `c <= d` give `a * d + b * c <= a * c + b * d`, read as every fact is — the products Mathlib's `nlinarith` adds before its linear search, each a fact whose monomials that search reads as unknowns.
///
/// A natural's sign is a factor only over one atom, and never beside another natural's, whose product conversion decides; a pair at two carriers is taken at `Int`, the `Nat` factor's sides widened as a sum's are. A pair whose `le/mul` is not in scope is left out.
pub(super) fn products(
    context: &mut Context,
    views: &mut LinearViews,
    classes: &mut Classes,
    facts: &Facts,
    negated: Option<&Fact>,
) -> Result<Vec<Fact>, Error> {
    let factors = facts
        .facts
        .iter()
        .chain(negated)
        .filter(|fact| {
            fact.origin != Origin::Natural
                || fact
                    .form
                    .terms
                    .iter()
                    .all(|(_, monomial)| monomial.atoms().len() == 1)
        })
        .collect::<Vec<_>>();
    let mut reader = Reader::new(views, classes);
    for (index, first) in factors.iter().enumerate() {
        for second in &factors[index + 1..] {
            if first.origin == Origin::Natural && second.origin == Origin::Natural {
                continue;
            }
            let carrier = match (first.carrier, second.carrier) {
                (Carrier::Natural, Carrier::Natural) => Carrier::Natural,
                _ => Carrier::Integer,
            };
            let mul = order(context, carrier).mul;
            if !in_scope(context, &[mul]) {
                continue;
            }
            let ((a, b), (c, d)) = (first.sides_at(carrier), second.sides_at(carrier));
            let proof = Term::apply_marked(
                global(mul),
                [
                    (Plicity::Implicit, a.clone()),
                    (Plicity::Implicit, b.clone()),
                    (Plicity::Implicit, c.clone()),
                    (Plicity::Implicit, d.clone()),
                    (Plicity::Explicit, first.proof.clone()),
                    (Plicity::Explicit, second.proof.clone()),
                ],
            );
            let times = |left: &Term, right: &Term| multiply(carrier, left.clone(), right.clone());
            let statement = comparison_of(
                carrier,
                Operation::AtMost,
                &add(carrier, times(&a, &d), times(&b, &c)),
                &add(carrier, times(&a, &c), times(&b, &d)),
            );
            let origin = Origin::Product(
                Box::new(first.origin.clone()),
                Box::new(second.origin.clone()),
            );
            reader.admit(context, statement, proof, origin)?;
        }
    }
    Ok(reader.read.facts)
}

/// A quotient or a remainder: its dividend, its divisor, and the program's proof that the divisor is not zero — so the quotient and the remainder rebuilt from it are the atoms the program wrote.
#[derive(Clone, PartialEq)]
struct Division {
    dividend: Term,
    divisor: Term,
    non_zero: Term,
}

impl Division {
    /// The division `term` is, where it is one by anything but the literal zero, whose proof of being nonzero only a contradiction in scope could hold.
    fn of(term: &Term) -> Option<Self> {
        let Subterm::Intrinsic(
            Intrinsic::NatDiv {
                dividend,
                divisor,
                non_zero,
            }
            | Intrinsic::NatRem {
                dividend,
                divisor,
                non_zero,
            },
        ) = &**term
        else {
            return None;
        };
        let zero = matches!(&**divisor, Subterm::Intrinsic(Intrinsic::Nat(literal))
            if literal.to_natural().is_some_and(|value| value.is_zero()));
        (!zero).then(|| Division {
            dividend: dividend.clone(),
            divisor: divisor.clone(),
            non_zero: non_zero.clone(),
        })
    }

    fn quotient(&self) -> Term {
        Term::intrinsic(Intrinsic::NatDiv {
            dividend: self.dividend.clone(),
            divisor: self.divisor.clone(),
            non_zero: self.non_zero.clone(),
        })
    }

    fn remainder(&self) -> Term {
        Term::intrinsic(Intrinsic::NatRem {
            dividend: self.dividend.clone(),
            divisor: self.divisor.clone(),
            non_zero: self.non_zero.clone(),
        })
    }

    /// `d * (x / d)`, which conversion reads beside `x % d` as `x`.
    fn multiple(&self) -> Term {
        Term::intrinsic(Intrinsic::NatMul(self.divisor.clone(), self.quotient()))
    }
}

/// A remainder lifted out of the facts: a fact over it is raised by a multiple of its quotient's multiple on both sides, which conversion reads as a fact over the dividend, and its form has the remainder replaced by what it denotes.
pub(super) struct Lift {
    remainder: Monomial,
    /// `x - d * (x / d)`.
    denotes: LinearForm,
    /// `d * (x / d)`.
    multiple: Term,
    /// `le/add_mono_l`.
    shift: SyntaxName,
}

impl Lift {
    /// The remainder's coefficient in `form`, where it has one.
    fn coefficient(&self, form: &LinearForm) -> Option<Integer> {
        form.terms
            .iter()
            .find(|(_, monomial)| *monomial == self.remainder)
            .map(|(coefficient, _)| coefficient.clone())
    }

    /// `fact` raised by `c * d * (x / d)` on both sides, `c` the remainder's coefficient in it, through `le/add_mono_l`: the remainder's `c` copies then stand beside as many multiples, which conversion reads as `c * x`. An `Int` fact reads a remainder only through its widening, which is left as it is: a certificate that needs it does not check, and is reported as the procedure's refusal.
    fn apply(&self, fact: &mut Fact) {
        let Some(coefficient) = self.coefficient(&fact.form) else {
            return;
        };
        if fact.carrier != Carrier::Natural {
            return;
        }
        let copies = coefficient.magnitude();
        let raise = match copies == Natural::from(1u32) {
            true => self.multiple.clone(),
            false => Term::intrinsic(Intrinsic::NatMul(
                literal(Carrier::Natural, copies),
                self.multiple.clone(),
            )),
        };
        fact.proof = Term::apply_marked(
            global(self.shift),
            [
                (Plicity::Explicit, raise.clone()),
                (Plicity::Implicit, fact.left.clone()),
                (Plicity::Implicit, fact.right.clone()),
                (Plicity::Explicit, fact.proof.clone()),
            ],
        );
        fact.left = Term::intrinsic(Intrinsic::NatAdd(raise.clone(), fact.left.clone()));
        fact.right = Term::intrinsic(Intrinsic::NatAdd(raise, fact.right.clone()));
        fact.form = self.substituted(&fact.form, &coefficient);
    }

    /// `form` with its `coefficient · remainder` replaced by `coefficient · (x - d * (x / d))`.
    fn substituted(&self, form: &LinearForm, coefficient: &Integer) -> LinearForm {
        let mut terms = form
            .terms
            .iter()
            .filter(|(_, monomial)| *monomial != self.remainder)
            .cloned()
            .collect::<Vec<_>>();
        for (scale, monomial) in &self.denotes.terms {
            let scaled = scale.clone() * coefficient.clone();
            match terms.iter_mut().find(|(_, known)| known == monomial) {
                Some((sum, _)) => *sum = sum.clone() + scaled,
                None => terms.push((scaled, monomial.clone())),
            }
        }
        terms.retain(|(coefficient, _)| !coefficient.is_zero());
        let mut nonnegative = form.nonnegative.clone();
        for atom in &self.denotes.nonnegative {
            if !nonnegative.contains(atom) {
                nonnegative.push(*atom);
            }
        }
        LinearForm {
            constant: form.constant.clone() + self.denotes.constant.clone() * coefficient.clone(),
            terms,
            nonnegative,
        }
    }
}

/// A range check as stated: `lo <= c <= hi`.
struct Range {
    c: Term,
    lo: Term,
    hi: Term,
}

/// What a statement names once its heads are opened.
enum Opened {
    /// A call of the range check `le/of_in_range` reads, its arguments as stated.
    Range(Range),
    /// A comparison the fragment reads, over its operands as stated.
    Comparison(Intrinsic),
    /// A conjunction, its conjuncts as stated.
    Conjunction(Term, Term),
}

/// `stated` opened a layer at a time by its heads alone, as a guard's resolved spelling is, until it is a call of the range check, a comparison or a conjunction: `None` where it becomes none of them. No operand is reduced, so what it names is spelled as the statement spells it — as an arm's key spells a guard.
fn opened(context: &mut Context, stated: &Term) -> Result<Option<Opened>, Error> {
    let check = global(context.syntax().entailment.in_range);
    open_until(context, stated, |term| match &**term {
        Subterm::Apply(Apply { head, arguments }) if *head == check => match arguments.as_slice() {
            [c, lo, hi] => Some(Some(Opened::Range(Range {
                c: c.term.clone(),
                lo: lo.term.clone(),
                hi: hi.term.clone(),
            }))),
            _ => Some(None),
        },
        Subterm::Intrinsic(Intrinsic::BoolAnd(first, second)) => {
            Some(Some(Opened::Conjunction(first.clone(), second.clone())))
        }
        Subterm::Intrinsic(comparison) if sides(comparison).is_some() => {
            Some(Some(Opened::Comparison(comparison.clone())))
        }
        _ => None,
    })
    .map(Option::flatten)
}

/// The decision `stated` holds, opened by its heads to `Holds(decision)` — a named proposition standing for one, to the `i < n` it was written as — or `None` where opening reaches no `Holds`.
pub(super) fn held(context: &mut Context, stated: &Term) -> Result<Option<Term>, Error> {
    let holds = global(context.syntax().proof.holds);
    open_until(context, stated, |term| match &**term {
        Subterm::Apply(Apply { head, arguments }) if *head == holds => match arguments.as_slice() {
            [decision] => Some(decision.term.clone()),
            _ => None,
        },
        _ => None,
    })
}

/// What `found` answers of `term` or of the first layer opening its heads reaches that it answers of, or `None` where opening stops first. Bounded as the resolved spelling is: each step opens one application layer.
fn open_until<T>(
    context: &mut Context,
    term: &Term,
    found: impl Fn(&Term) -> Option<T>,
) -> Result<Option<T>, Error> {
    let mut current = term.clone();
    for _ in 0..RESOLVED_SPELLING_LAYERS {
        if let Some(answer) = found(&current) {
            return Ok(Some(answer));
        }
        match open_layer(context, &current)? {
            Some(layer) => current = layer,
            None => return Ok(None),
        }
    }
    Ok(None)
}

/// `match head | true => true_case | false => false_case end`, its motive elided as a written one is, so elaboration checks each arm against the expected type with `head` refined.
pub(super) fn split(context: &mut Context, head: &Term, false_case: Term, true_case: Term) -> Term {
    let motive = Term::match_motive_written(Term::hole(context.mint_metavar()));
    Term::bool_match_scoped(head.clone(), motive, false_case, true_case)
}

/// `match refuted end`: a proof whose type reduces to an empty proposition, eliminated in the place of whatever is expected.
pub(super) fn eliminate(context: &mut Context, refuted: Term) -> Term {
    let motive = Term::match_motive_written(Term::hole(context.mint_metavar()));
    let cases = Vec::<(curios_core::Atom, Vec<(Plicity, Free)>, Term)>::new();
    Term::induct_match_scoped_marked(refuted, motive, cases, None)
}

/// `let x: statement = body; x`: `body` elaborated against `statement` rather than against whatever position it is put in.
fn bound(context: &mut Context, statement: Term, body: Term) -> Term {
    let binder = context.fresh(None);
    Term::let_(&binder, statement, body, Term::free_var(&binder))
}

/// A comparison's carrier, relation and operands, for the six the fragment reads.
fn sides(comparison: &Intrinsic) -> Option<(Carrier, Operation, Term, Term)> {
    let (carrier, relation, left, right) = match comparison {
        Intrinsic::NatLt(left, right) => (Carrier::Natural, Operation::Less, left, right),
        Intrinsic::NatLe(left, right) => (Carrier::Natural, Operation::AtMost, left, right),
        Intrinsic::NatEql(left, right) => (Carrier::Natural, Operation::Equal, left, right),
        Intrinsic::IntLt(left, right) => (Carrier::Integer, Operation::Less, left, right),
        Intrinsic::IntLe(left, right) => (Carrier::Integer, Operation::AtMost, left, right),
        Intrinsic::IntEql(left, right) => (Carrier::Integer, Operation::Equal, left, right),
        _ => return None,
    };
    Some((carrier, relation, left.clone(), right.clone()))
}

/// `left ⋈ right` at `carrier`, `⋈` being `<` or `<=`.
fn comparison_of(carrier: Carrier, relation: Operation, left: &Term, right: &Term) -> Intrinsic {
    let (left, right) = (left.clone(), right.clone());
    match (carrier, relation) {
        (Carrier::Natural, Operation::Less) => Intrinsic::NatLt(left, right),
        (Carrier::Natural, _) => Intrinsic::NatLe(left, right),
        (_, Operation::Less) => Intrinsic::IntLt(left, right),
        _ => Intrinsic::IntLe(left, right),
    }
}

/// `term + 1` at `carrier`.
fn successor(carrier: Carrier, term: Term) -> Term {
    add(carrier, term, literal(carrier, 1u32))
}

/// `left + right` at `carrier`.
pub(super) fn add(carrier: Carrier, left: Term, right: Term) -> Term {
    Term::intrinsic(match carrier {
        Carrier::Natural => Intrinsic::NatAdd(left, right),
        _ => Intrinsic::IntAdd(left, right),
    })
}

/// `left * right` at `carrier`.
pub(super) fn multiply(carrier: Carrier, left: Term, right: Term) -> Term {
    Term::intrinsic(match carrier {
        Carrier::Natural => Intrinsic::NatMul(left, right),
        _ => Intrinsic::IntMul(left, right),
    })
}

/// The literal `value` at `carrier`.
pub(super) fn literal(carrier: Carrier, value: impl Into<Natural>) -> Term {
    let value = value.into();
    Term::intrinsic(match carrier {
        Carrier::Natural => Intrinsic::Nat(Nat::new(value)),
        _ => Intrinsic::Int(Integer::from(value)),
    })
}

fn carrier_type(carrier: Carrier) -> Term {
    Term::intrinsic(match carrier {
        Carrier::Natural => Intrinsic::NatType,
        _ => Intrinsic::IntType,
    })
}

fn carrier_of(type_: &Term) -> Option<Carrier> {
    match &**type_ {
        Subterm::Intrinsic(Intrinsic::NatType) => Some(Carrier::Natural),
        Subterm::Intrinsic(Intrinsic::IntType) => Some(Carrier::Integer),
        _ => None,
    }
}

/// The order vocabulary at `carrier`.
pub(super) fn order(context: &Context, carrier: Carrier) -> OrderSyntax {
    let entailment = context.syntax().entailment;
    match carrier {
        Carrier::Natural => entailment.nat,
        _ => entailment.int,
    }
}

/// `True/qed()`.
pub(super) fn qed(context: &Context) -> Term {
    Term::apply(global(context.syntax().proof.true_qed), Vec::<Term>::new())
}
