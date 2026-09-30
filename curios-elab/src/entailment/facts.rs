//! The facts a bound may be proved from, and the goal it states: what the hole's scope says about `Nat` and `Int` comparisons, each read through the canonical linear view `curios-core` publishes and paired with a term that proves it.
//!
//! **Where a fact comes from.**
//!
//! - A hypothesis in scope whose type reduces to a decided comparison, a conjunction of comparisons — `&&`, and any `Bool` function unfolding to one as `in_range` and `Char/is_upper` do — or an equation at `Nat` or `Int`.
//! - A proof field of a tuple- or struct-typed hypothesis, one level down, where the item may open the representation: `v.counted`, `hi.ok`.
//! - A guard of an arm the hole sits in — the arm's at insertion, and at a retry the ones the hole was born under: a comparison scrutinee with the case it took, an `==` scrutinee's true arm, and a range check's true arm, through `Le/of_in_range`, since a split in the guard's own arm meets the guard's key, which answers `true` for the whole check and never unfolds it.
//! - What an operation a fact or the goal is read over defines: a quotient's bounds, and a truncated subtraction's cases ([`Reader::definitions`]).
//! - That every monomial over naturals is at least zero, which conversion decides.
//!
//! A local definition and a variable a match refined need no reading of their own: every statement is read reduced, so a fact stated over a `let` meets a goal stated over its value, and one over `k` meets one over the successor an arm refined `k` to.
//!
//! **A remainder by a literal is lifted away.** Conversion reads `k * (x / k) + x % k` as `x` wherever one side of a comparison holds both, so a sum of facts that holds a remainder beside its quotient's multiple is not the sum of their views. Every fact over `x % k` is therefore raised by the multiple on both sides, which conversion reads as a fact over `x` ([`Lift`]), and the remainder stays out of every form the search reads.
//!
//! **A fact is `left <= right` at its carrier**, as the proof's lemmas take it: a strict fact is the loose one of its successor, which conversion reads as one proposition at `Nat` as at `Int`, and an equation is two facts. Its form is `left - right` aligned to `<=`, read by the one reader every fact and the goal share, so their atoms are one table's.
//!
//! **What is not read is recorded.** A proposition stating a decision the view does not read — a disjunction, an opaque `Bool` function, an equality's false arm — is kept for the report.

use {
    super::{decision_of, global, in_scope},
    crate::{Context, Error, reduce_with},
    curios_algebra::{Carrier, LinearForm, Monomial, Operation},
    curios_core::{
        Cases, Free, Global, InductType, Intrinsic, LinearViews, Match, Nat, StructType, Subterm,
        Term, TupleType,
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
    /// What an operation defines: a quotient's bound, or a case of a truncated subtraction.
    Definition,
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
            Origin::Natural | Origin::Definition | Origin::Negated => None,
        }
    }

    /// Whether a report names the fact: one a reader can act on, which a natural's sign and an operation's definition are not.
    pub(super) fn is_reported(&self) -> bool {
        !matches!(self, Origin::Natural | Origin::Definition)
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
}

impl Facts {
    /// Whether `form` holds a remainder the facts were lifted over: a goal that does is not met by their sum, which is over the dividend, and is proved by refuting its negation, lifted as they were.
    pub(super) fn lifted_over(&self, form: &LinearForm) -> bool {
        self.lifts
            .iter()
            .any(|lift| lift.coefficient(form).is_some())
    }
}

/// A case split a truncated subtraction `a - b` defines, on `b <= a`, with the facts each case adds.
pub(super) struct Split {
    /// `b <= a`, which the proof splits on.
    pub(super) guard: Term,
    /// Where it holds: `b <= a`, and `b + (a - b) = a` by `Le/add_sub_cancel`.
    pub(super) holds: Vec<Fact>,
    /// Where it fails: `a < b`, and `a - b <= 0` by `Le/sub_zero`, which conversion does not decide there.
    pub(super) fails: Vec<Fact>,
}

/// One reading of a scope: the reader every fact and the goal share, what it has read so far, and the operations whose definitions it has read.
pub(super) struct Reader<'a> {
    views: &'a mut LinearViews,
    read: Facts,
    divided: Vec<Division>,
    subtracted: Vec<(Term, Term)>,
}

impl<'a> Reader<'a> {
    pub(super) fn new(views: &'a mut LinearViews) -> Self {
        Reader {
            views,
            read: Facts {
                facts: Vec::new(),
                splits: Vec::new(),
                lifts: Vec::new(),
                unread: Vec::new(),
            },
            divided: Vec::new(),
            subtracted: Vec::new(),
        }
    }

    /// The goal `decision` states, or `None` where it is no `Nat` or `Int` `<` or `<=`: an equality's negation is a disjunction, and nothing else is a comparison the view reads. Read first, so its atoms are handed out before any fact's.
    pub(super) fn target(
        &mut self,
        context: &mut Context,
        decision: &Term,
    ) -> Result<Option<Target>, Error> {
        let reduced = reduce_with(context, decision)?;
        let Subterm::Intrinsic(comparison) = &*reduced else {
            return Ok(None);
        };
        let Some((carrier, relation @ (Operation::Less | Operation::AtMost), left, right)) =
            sides(comparison)
        else {
            return Ok(None);
        };
        let Some((_, view)) = self.views.read(comparison) else {
            return Ok(None);
        };
        let (_, form) = view.form().clone().aligned(view.relation());
        Ok(Some(Target {
            carrier,
            relation,
            left,
            right,
            form,
            decision: decision.clone(),
        }))
    }

    /// Every fact the scope states: the hypotheses, their proof fields and the guards live now, what the operations they are read over define, and every natural monomial's non-negativity, the goal's included.
    pub(super) fn collect(
        mut self,
        context: &mut Context,
        target: Option<&Target>,
    ) -> Result<Facts, Error> {
        curios_profile::profile!("entailment::collect");
        for (name, type_) in context.locals().to_vec() {
            self.hypothesis(context, &name, &type_)?;
        }
        self.guards(context)?;
        self.definitions(context, target)?;
        // Last, so every monomial a fact or the goal was read over has been handed out.
        self.naturals(context, target)?;
        Ok(self.read)
    }

    /// A hypothesis: a fact itself, or a tuple or structure whose proof fields are, one level down.
    fn hypothesis(
        &mut self,
        context: &mut Context,
        name: &Free,
        type_: &Term,
    ) -> Result<(), Error> {
        let reduced = reduce_with(context, type_)?;
        let telescope = match &*reduced {
            Subterm::TupleType(TupleType { telescope }) => telescope.clone(),
            Subterm::StructType(StructType {
                name: family,
                universes,
                params,
            }) => {
                let Some(decl) = context.struct_decl(family).cloned() else {
                    return Ok(());
                };
                // `elaborate_proj`'s own rule: a proof the procedure writes reaches no field its author could not.
                if !decl.rep_public
                    && context
                        .island()
                        .is_some_and(|island| !island.is_within(&decl.module))
                {
                    return Ok(());
                }
                let arity = context.instantiate_universe_bound_at(
                    &decl.universe_context,
                    &decl.arity,
                    universes,
                )?;
                arity.open(&params.iter().collect::<Vec<_>>())
            }
            _ => {
                let origin = Origin::Hypothesis(Term::free_var(name));
                return self.proposition(context, type_, Term::free_var(name), origin);
            }
        };
        let mut cursor = telescope.cursor();
        let mut index = 0;
        while let Some((label, domain)) = cursor.entry() {
            let field = Term::proj(Term::free_var(name), index);
            let label = label.map_or_else(|| index.to_string(), str::to_string);
            self.proposition(
                context,
                &domain,
                field.clone(),
                Origin::Field(Term::free_var(name), label),
            )?;
            cursor.advance(field);
            index += 1;
        }
        Ok(())
    }

    /// A proposition `proof` inhabits: the facts it states, if it states any — read as a bound is, through the decision its `Holds` is stuck on, or as an equation.
    fn proposition(
        &mut self,
        context: &mut Context,
        type_: &Term,
        proof: Term,
        origin: Origin,
    ) -> Result<(), Error> {
        let reduced = reduce_with(context, type_)?;
        if let Some(decision) = decision_of(context, &reduced)? {
            return self.decision(context, &decision, type_, proof, origin);
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

    /// A decision `proof` shows `true`: a comparison, an equality, or a conjunction of them.
    fn decision(
        &mut self,
        context: &mut Context,
        decision: &Term,
        stated: &Term,
        proof: Term,
        origin: Origin,
    ) -> Result<(), Error> {
        let reduced = reduce_with(context, decision)?;
        match &*reduced {
            Subterm::Intrinsic(Intrinsic::BoolAnd(first, second)) => {
                self.conjunction(context, first, second, stated, proof, origin)
            }
            Subterm::Intrinsic(comparison) => match sides(comparison) {
                Some((_, Operation::Less | Operation::AtMost, ..)) => {
                    self.admit(context, comparison.clone(), proof, origin)
                }
                Some((carrier, Operation::Equal, left, right))
                    if in_scope(context, &[order(context, carrier).eq_of_eql]) =>
                {
                    let eq_of_eql = order(context, carrier).eq_of_eql;
                    let equation =
                        Term::apply(global(eq_of_eql), [left.clone(), right.clone(), proof]);
                    self.equation(context, carrier, &left, &right, equation, origin)
                }
                _ => {
                    self.read.unread.push((origin, stated.clone()));
                    Ok(())
                }
            },
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
    /// A guard's written spelling is reduced with its own refinement withheld, which turns `i < m` written through `Cmp` into the `Nat/lt` intrinsic, and `Char/is_upper(c)` into `in_range`'s body, without the arm answering `true` for either.
    fn guards(&mut self, context: &mut Context) -> Result<(), Error> {
        let entries = context
            .visible_scrutinee_entries()
            .map(|(frame, _, entry)| (frame, entry.original.clone(), entry.value.clone()))
            .collect::<Vec<_>>();

        for (frame, written, value) in entries {
            let Subterm::Intrinsic(Intrinsic::Bool(case)) = &*value else {
                continue;
            };
            let case = *case;
            let origin = Origin::Guard(written.clone(), case);
            let spelled = context
                .with_refinements_withheld_from(frame, |context| reduce_with(context, &written))?;
            match &*spelled {
                Subterm::Intrinsic(comparison) => {
                    self.guard(context, comparison, case, &written, origin)?;
                }
                // A range check's true arm, through `Le/of_in_range` at `qed`: the lemma's body was elaborated where no guard stood between the check and its unfolding, and `qed` checks against the check in the arm.
                Subterm::Match(Match {
                    head,
                    cases:
                        Cases::Bool {
                            false_case,
                            true_case,
                        },
                    ..
                }) if case
                    && matches!(&**false_case, Subterm::Intrinsic(Intrinsic::Bool(false))) =>
                {
                    self.range(context, head, true_case, origin, &written)?;
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

    /// A range check's true arm: `lo <= c` and `c <= hi`, read off `in_range`'s unfolded body. A weak head normal form leaves a match's arms as elaborated — `c <= hi` still a call through `Cmp` — so both conjuncts are reduced before they are read.
    fn range(
        &mut self,
        context: &mut Context,
        first: &Term,
        second: &Term,
        origin: Origin,
        written: &Term,
    ) -> Result<(), Error> {
        let range = context.syntax().entailment.range;
        let (first, second) = (reduce_with(context, first)?, reduce_with(context, second)?);
        let bounds = match (&*first, &*second) {
            (
                Subterm::Intrinsic(Intrinsic::NatLe(lo, c)),
                Subterm::Intrinsic(Intrinsic::NatLe(bounded, hi)),
            ) if c == bounded => Some((c.clone(), lo.clone(), hi.clone())),
            _ => None,
        };
        let Some((c, lo, hi)) = bounds.filter(|_| in_scope(context, &[range])) else {
            self.read.unread.push((origin, written.clone()));
            return Ok(());
        };
        let both = Term::apply(
            global(range),
            [c.clone(), lo.clone(), hi.clone(), qed(context)],
        );
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

    /// What the operations the facts and the goal are read over define, until a reading meets no new one: a quotient's bounds hold its dividend, which may be a truncated subtraction, and a subtraction's cases hold its operands, which may be quotients.
    fn definitions(&mut self, context: &mut Context, target: Option<&Target>) -> Result<(), Error> {
        loop {
            let divided = self.quotients(context, target)?;
            let subtracted = self.subtractions(context, target)?;
            if !divided && !subtracted {
                return Ok(());
            }
        }
    }

    /// The bounds of each quotient and remainder by a literal among the atoms read so far — `k * (x / k) <= x` and `x < k * (x / k) + k` — with the remainder lifted out of every fact ([`Lift`]). Whether it read one.
    ///
    /// The bounds are `Le/add_r(k * (x / k), x % k)` and `Le/add_mono_l(k * (x / k), @x % k + 1, @k, qed)`, whose types hold the remainder beside its quotient's multiple, which conversion reads as the dividend; their stated sides hold neither. A division is read only once no other still to be read divides its remainder, so the bounds of `(x % 4096) / 64`, which hold `x % 4096`, are read before that remainder is lifted out of them.
    ///
    /// `Nat` alone: at `Int`, conversion decides Euclid's identity and neither of a remainder's bounds.
    fn quotients(&mut self, context: &mut Context, target: Option<&Target>) -> Result<bool, Error> {
        let natural = context.syntax().entailment.natural;
        if !in_scope(context, &[natural.below, natural.shift]) {
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
            let above = Term::apply_marked(
                global(natural.shift),
                [
                    (Plicity::Explicit, multiple.clone()),
                    (Plicity::Implicit, successor(Carrier::Natural, remainder)),
                    (Plicity::Implicit, division.divisor.clone()),
                    (Plicity::Explicit, qed(context)),
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

    /// What lifting `division`'s remainder out of a fact takes: the remainder's monomial, and what it denotes — `x - k * (x / k)`, the form of `x <= k * (x / k)`. `None` where the remainder does not read as one atom.
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
            natural.loosened,
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
                    self.read.splits.push(Split {
                        guard,
                        holds,
                        fails,
                    });
                }
            }
        }
        Ok(read)
    }

    /// The case of `a - b` that does not truncate: `b <= a`, and `b + (a - b) = a` by `Le/add_sub_cancel`, each proved from `b <= a` by `qed` where the scope decides it.
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

    /// The case that truncates: `a < b`, by `Lt/of_not_le` where the scope decides `b <= a` fails, and `a - b <= 0` by `Le/sub_zero`.
    fn truncated(&mut self, context: &mut Context, a: &Term, b: &Term) -> Result<(), Error> {
        let natural = context.syntax().entailment.natural;
        let of_not_le = order(context, Carrier::Natural).of_not_le;
        let below = Term::apply(global(of_not_le), [b.clone(), a.clone(), qed(context)]);
        let guard = Intrinsic::NatLt(a.clone(), b.clone());
        self.admit(context, guard, below.clone(), Origin::Definition)?;
        let loosened = Term::apply(global(natural.loosened), [a.clone(), b.clone(), below]);
        let zero = Term::apply(global(natural.truncated), [a.clone(), b.clone(), loosened]);
        let subtracted = Term::intrinsic(Intrinsic::NatSub(a.clone(), b.clone()));
        let truncation = Intrinsic::NatLe(subtracted, literal(Carrier::Natural, 0u32));
        self.admit(context, truncation, zero, Origin::Definition)
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
        let read = Intrinsic::NatLe(reduce_with(context, left)?, reduce_with(context, right)?);
        Ok(self
            .views
            .read(&read)
            .map(|(_, view)| view.form().clone().aligned(view.relation()).1))
    }

    /// That every monomial over naturals is at least zero — clause 2 of what conversion decides, so `Bool/True/qed()` proves it — for every such monomial a fact, a case or the goal was read over: a goal over an atom no fact names still needs that atom's sign.
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
        let read = comparison_of(
            carrier,
            relation,
            &reduce_with(context, &left)?,
            &reduce_with(context, &right)?,
        );
        let Some((_, view)) = self.views.read(&read) else {
            return Ok(());
        };
        let (aligned, form) = view.form().clone().aligned(view.relation());
        if aligned != Operation::AtMost {
            return Ok(());
        }
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
}

/// The goal's negation, a fact of the refuting form's false arm, where a split on the goal's decision refined it to `false`: `a < b` failing is `b <= a`, `a <= b` failing is `b < a`. Read by the same reader, after the facts, proved by `qed` there, and lifted by `lifts` as they were. `None` where the vocabulary is not in scope.
pub(super) fn negated(
    context: &mut Context,
    views: &mut LinearViews,
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
    let mut reader = Reader::new(views);
    reader.admit(context, statement, proof, Origin::Negated)?;
    let mut negation = reader.read.facts.pop();
    for lift in lifts {
        negation.iter_mut().for_each(|fact| lift.apply(fact));
    }
    Ok(negation)
}

/// A quotient or a remainder by a nonzero literal: its dividend, its divisor, and the program's proof that the divisor is not zero — so the quotient and the remainder rebuilt from it are the atoms the program wrote.
#[derive(Clone, PartialEq)]
struct Division {
    dividend: Term,
    divisor: Term,
    non_zero: Term,
}

impl Division {
    /// The division `term` is, where it is one by a nonzero literal.
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
        let Subterm::Intrinsic(Intrinsic::Nat(literal)) = &**divisor else {
            return None;
        };
        literal
            .to_natural()
            .is_some_and(|value| !value.is_zero())
            .then(|| Division {
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

    /// `k * (x / k)`, which conversion reads beside `x % k` as `x`.
    fn multiple(&self) -> Term {
        Term::intrinsic(Intrinsic::NatMul(self.divisor.clone(), self.quotient()))
    }
}

/// A remainder lifted out of the facts: a fact over it is raised by a multiple of its quotient's multiple on both sides, which conversion reads as a fact over the dividend, and its form has the remainder replaced by what it denotes.
pub(super) struct Lift {
    remainder: Monomial,
    /// `x - k * (x / k)`.
    denotes: LinearForm,
    /// `k * (x / k)`.
    multiple: Term,
    /// `Le/add_mono_l`.
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

    /// `fact` raised by `c * k * (x / k)` on both sides, `c` the remainder's coefficient in it, through `Le/add_mono_l`: the remainder's `c` copies then stand beside as many multiples, which conversion reads as `c * x`. An `Int` fact reads a remainder only through its widening, which is left as it is: a certificate that needs it does not check, and is reported as the procedure's refusal.
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

    /// `form` with its `coefficient · remainder` replaced by `coefficient · (x - k * (x / k))`.
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

/// `let conjunct: statement = body; conjunct`: `body` elaborated against `statement` rather than against whatever position it is put in.
fn bound(context: &mut Context, statement: Term, body: Term) -> Term {
    let binder = context.fresh(Some("conjunct"));
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
    let one = literal(carrier, 1u32);
    Term::intrinsic(match carrier {
        Carrier::Natural => Intrinsic::NatAdd(term, one),
        _ => Intrinsic::IntAdd(term, one),
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

/// `Bool/True/qed()`.
pub(super) fn qed(context: &Context) -> Term {
    Term::apply(global(context.syntax().proof.true_qed), Vec::<Term>::new())
}
