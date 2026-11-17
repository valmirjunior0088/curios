//! The certifier as its readers hold it: reduction, conversion and classification each read their terms by value, every `let`-bound local unfolded, behind doors that unfold what they are handed.
//!
//! **A reading is made only here, at a door.** [`ByValue`] holds the certifier mutably borrowed, so nothing binds while it lives, and it offers no typing, no binding and no recording: the certifier's lookups by shared reference, and by name each capability a reading mutates — the budget, the memo tables, a case equation's settlement, a fresh binder opened, a bracket, plain reduction, the closed machine's runs. Its fields are this module's, and every door enters by one function, which unfolds the terms the door is handed before it makes one ([`Certifier::read_by_value`]), but the size order's, which unfolds them as a graded call reads them, a variable an arm's index inversion solved standing as written ([`Certifier::read_graded`]); a bracket hands its walk the same reading. So a walk behind a door is handed only what its door unfolded, its own parts and reducts, binders it minted, and what the scope and the definitions hold, all by value but for such a variable, which reduction holds as it holds any other, and no binding can leave any of it naming a `let`-bound local while it reads.
//!
//! **A door keeps the name and the signature it had**, so typing, erasure's [`Reader`](super::Reader), the suites and the crates that call the certifier read the same functions. Where a door and its walk share a name, the walk's entry is the reading's method of that name.
//!
//! **A shared trait is implemented on both sides of the doors.** Shared reduction and the shared analyses call back through `Reducer`, `ClosedHost`, `Driver` and `Env`: for a reading they trust what they are handed, which the walk that called them built from what it was handed; for the certifier they are doors. Which applies is the receiver the shared code was handed.
//!
//! Why the rule is held this way, and what the peers do, is the crate README's decision, *A `let` is bound for typing and read by value*.

#[cfg(any(test, feature = "test-support"))]
mod test_support;

use {
    super::{Certifier, Converted, Error, Sort, canonical_operands, resolved_spelling},
    curios_analysis::{Classification, Env, Erased},
    curios_core::{
        Advance, Applications, ClosedMemo, Cost, Free, Global, InductDecl, Intrinsic, Polarity,
        Probe, ReduceError, Reducer, StructDecl, Subterm, Term, Variance,
    },
    std::{mem, ops::Deref},
};

/// The certifier while it reads by value: what reduction, conversion and classification may ask of it, made only by the doors of this module.
pub struct ByValue<'b, 's> {
    certifier: &'b mut Certifier<'s>,
    /// Whether the reading's reduction is plain: it asks the certifier's conversion nothing ([`ByValue::plainly`]). A mode of the reading, and of nothing else, so typing, which runs over no reading, has none to be in.
    plain: bool,
}

impl<'s> Deref for ByValue<'_, 's> {
    type Target = Certifier<'s>;

    fn deref(&self) -> &Certifier<'s> {
        self.certifier
    }
}

impl<'s> Certifier<'s> {
    /// Read `terms` in a reading of this certifier, each unfolded first: the door every reading enters by but the size order's.
    fn read_by_value<const N: usize, T>(
        &mut self,
        terms: [&Term; N],
        read: impl FnOnce(&mut ByValue<'_, 's>, [Term; N]) -> T,
    ) -> T {
        let terms = terms.map(|term| self.scope.by_value(term));

        self.read_unfolded(terms, read)
    }

    /// Read `terms` as a graded call reads them ([`Certifier::graded`]): unfolded as [`Certifier::read_by_value`] unfolds them, but for a variable an arm's index inversion solved, which stands as written, so the size order reads nothing an inversion concluded. The size order's door ([`Env::force_graded`]).
    fn read_graded<const N: usize, T>(
        &mut self,
        terms: [&Term; N],
        read: impl FnOnce(&mut ByValue<'_, 's>, [Term; N]) -> T,
    ) -> T {
        let terms = terms.map(|term| self.scope.graded(term));

        self.read_unfolded(terms, read)
    }

    /// Read `terms`, which a door has unfolded, in a reading of this certifier: the one place a reading is made, which [`Certifier::read_by_value`] and [`Certifier::read_graded`] alone enter.
    fn read_unfolded<const N: usize, T>(
        &mut self,
        terms: [Term; N],
        read: impl FnOnce(&mut ByValue<'_, 's>, [Term; N]) -> T,
    ) -> T {
        read(
            &mut ByValue {
                certifier: self,
                plain: false,
            },
            terms,
        )
    }

    /// Assume an arm's case equation: within the arm, `scrutinee` — as written, and as its dispatch resolves — is `value`, definitionally. Assumed inside the arm's [`Certifier::scoped`] bracket, which is what scopes it.
    ///
    /// **A door.** Both are unfolded first, so the equation is keyed by the spelling the readings inside the arm are handed, made where it is recorded.
    ///
    /// The resolved spelling is computed before the equation is pushed, so it rests only on the equations outside it, which retract no earlier than it does — the view [`ByValue::settle_refinement`] has to reconstruct by withholding, had for free here. Only a local-bearing scrutinee is resolved, since `Scope::refine` records nothing else, and only one with no head a probe presents already. The resolution is a [`Probe`], as a settlement is: a scrutinee with no value at the type level records the written spelling alone.
    pub(crate) fn refine(&mut self, scrutinee: Term, value: Term) -> Result<(), ReduceError> {
        let (scrutinee, value, resolved) =
            self.read_by_value([&scrutinee, &value], |certifier, [scrutinee, value]| {
                let resolved = match scrutinee.has_local_free() && scrutinee.head_key().is_none() {
                    true => resolved_spelling(certifier, &scrutinee).probed()?.flatten(),
                    false => None,
                };

                Ok::<_, ReduceError>((scrutinee, value, resolved))
            })?;
        // What was read off the scope goes where the equations in force change, and an arm that records none — its scrutinee names no local, a literal an outer arm substituted there among them — leaves them as they were.
        if self.scope.refine(scrutinee, resolved, value) {
            self.memos.begin_equations();
        }

        Ok(())
    }
}

/// Whether `this` and `that` are definitionally equal at `type_`, each read by value (`ByValue::convert`).
pub fn convert(
    certifier: &mut Certifier,
    type_: &Term,
    this: &Term,
    that: &Term,
) -> Result<bool, Error> {
    curios_profile::profile!("convert");
    certifier.read_by_value([type_, this, that], |certifier, [type_, this, that]| {
        certifier.convert(&type_, &this, &that)
    })
}

impl Sort {
    /// Classify `type_`, which the caller must already have checked to be a type, read by value ([`ByValue::sort_of`]).
    pub fn of(certifier: &mut Certifier, type_: &Term) -> Result<Sort, Error> {
        certifier.read_by_value([type_], |certifier, [type_]| certifier.sort_of(&type_))
    }
}

/// The sort `universe` reduces to, read by value ([`ByValue::as_sort`]).
pub fn as_sort(certifier: &mut Certifier, universe: &Term) -> Result<Sort, Error> {
    certifier.read_by_value([universe], |certifier, [universe]| {
        certifier.as_sort(&universe)
    })
}

/// The type of a neutral spine, read by value ([`ByValue::synth_neutral`]).
pub fn synth_neutral(certifier: &mut Certifier, term: &Term) -> Result<Option<Term>, Error> {
    certifier.read_by_value([term], |certifier, [term]| certifier.synth_neutral(&term))
}

/// The sort of an intrinsic type former, its parts read by value ([`ByValue::sort_of_intrinsic`]).
pub fn sort_of_intrinsic(certifier: &mut Certifier, intrinsic: &Intrinsic) -> Result<Sort, Error> {
    let former = Term::intrinsic(intrinsic.clone());
    certifier.read_by_value([&former], |certifier, [former]| {
        let Subterm::Intrinsic(intrinsic) = &*former else {
            unreachable!("unfolding a former's parts leaves the former an intrinsic")
        };
        certifier.sort_of_intrinsic(intrinsic)
    })
}

/// Whether a value of this type can be distinguished from another of the same type — the question the large-elimination guard and the `Prop`-field rule both turn on — `type_` read by value (`ByValue::sort_of`).
///
/// Exactly one thing cannot be: a proof, because irrelevance makes any two inhabitants of a proposition interchangeable. Everything else can, **a type included**.
///
/// That a type is erased is not the criterion, and reading it as one would certify a closed inhabitant of `False`. Erasure governs what the *runtime* can observe; irrelevance is a claim about *definitional equality*, and conversion reads a type-valued position back in full. A proposition carrying `A : Type` is identified with one carrying `B`, so eliminating it — or projecting it, which meets no guard at all — makes `A` and `B` convertible, and transport does the rest. `crate::recheck::proposition_tests::a_derivation_through_a_type_carrying_proposition_is_refused` holds that derivation shut; `erased_half` asks the runtime question and is where the structural `Type(_) | Prop` test legitimately belongs.
///
/// Public for one reason: `curios-elab` writes this rule a second time as `is_prop`, and a compile cannot observe the two disagreeing — the elaborator refuses such a declaration before the certifier is asked — so the two are compared directly by `curios-elab`'s `typing::tests::both_checkers_decide_non_informativeness_alike`, which needs to name this one. It answers a question about a type and admits nothing on its own, so exporting it widens what the trusted base can be *asked* without widening what it can be told.
pub fn carries_information(certifier: &mut Certifier, type_: &Term) -> Result<bool, Error> {
    Ok(!Sort::of(certifier, type_)?.is_prop())
}

/// The erased half a term judged at `type_` belongs to, `type_` read by value ([`ByValue::erased_half`]), remembered beside the certifier's sorts: the one door a recorded position (`Certifier::record_checked`) and erasure's [`Reader::erased`](super::Reader::erased) ask.
///
/// **Filed under the type by value, as its sort is**, and under the lives a sort has (`Memos`). A classification that failed is asked again where the type is met next: remembered, its `None` would read as a type no erased half holds, and the failure would go with whichever walk met it first — a refused one takes it back.
pub fn erased_half(certifier: &mut Certifier, type_: &Term) -> Result<Option<Erased>, Error> {
    certifier.read_by_value([type_], |certifier, [type_]| {
        let in_arm = certifier.has_refinements();
        if let Some(erased) = certifier.standing(certifier.memos.half(&type_, in_arm)) {
            return Ok(erased);
        }
        let erased = certifier.erased_half(&type_)?;
        let prefix = certifier.scope.prefix();
        certifier
            .certifier
            .memos
            .store_half(type_, in_arm, prefix, erased);

        Ok(erased)
    })
}

/// Whether a value of `type_` is a proof, `type_` read by value ([`ByValue::bears_proof`]).
pub fn bears_proof(certifier: &mut Certifier, type_: &Term) -> Result<bool, Error> {
    certifier.read_by_value([type_], |certifier, [type_]| certifier.bears_proof(&type_))
}

/// The doors to reduction, for typing and for a shared analysis typing started: each term read by value, then reduced in a reading, by [`ByValue`]'s `Reducer`.
impl Reducer for Certifier<'_> {
    fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
        self.read_by_value([&term], |certifier, [term]| certifier.whnf(term))
    }

    fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
        self.read_by_value([&term], |certifier, [term]| certifier.reduce_forced(term))
    }

    fn spend(&mut self, cost: Cost) -> Result<(), ReduceError> {
        Certifier::spend(self, cost)
    }

    fn fresh_binder(&mut self, hint: Option<&str>) -> Free {
        self.fresh(hint)
    }
}

impl ByValue<'_, '_> {
    /// Run `walk` with every binder it opened and every case equation it assumed closed again afterwards, over this reading, as [`Certifier::scoped`] runs a judgment.
    pub(crate) fn scoped<T>(&mut self, walk: impl FnOnce(&mut Self) -> T) -> T {
        let bracket = self.certifier.open_bracket();
        let outcome = walk(self);
        self.certifier.close_bracket(bracket);

        outcome
    }

    /// Run `read` with reduction plain: it asks the certifier's conversion nothing, at a stuck fold, at a missed equation, at a stuck elimination's atoms or at a one-arm match of a proposition.
    ///
    /// **Two readers need it.** A question reduction puts to conversion is answered by plain reduction: conversion reduces, and left to ask again, a term that reaches itself through an unfolding would be asked about without end, so a question is one conversion the budget bounds, over the reduction that stood before reduction asked anything. And a shared analysis reads a term by it (`Env::force`): conversion's proof irrelevance is sound because acceptance waits on totality, which must therefore read terms on no verdict of conversion's.
    ///
    /// **Each of the two reductions keeps its own answers.** A local-bearing reduct is remembered by the reduction that took it, and so are a sort and a comparison's verdict, which are read through reducts (`Memos`); an equation's reduced spelling is settled once for each. So what plain reduction made of a term never answers for a judgment's, nor the other way, and no answer depends on which of the two asked first.
    ///
    /// **Nothing is typed under it.** The mode is the reading's, and a reading offers no typing, so the types and the checks the certifier remembers, and the erased halves it classifies positions by, are a judgment's alone, and none is filed twice.
    pub(crate) fn plainly<T>(&mut self, read: impl FnOnce(&mut Self) -> T) -> T {
        let outer = mem::replace(&mut self.plain, true);
        let answer = read(self);
        self.plain = outer;

        answer
    }

    /// Whether this reading's reduction is plain ([`ByValue::plainly`]).
    pub(crate) fn plain(&self) -> bool {
        self.plain
    }

    /// Run `read` with the equation at `index` and every equation inside it withheld: the view an equation's reduced spelling is settled in ([`ByValue::settle_refinement`]) and its scrutinee asked about (`ByValue::asked_scrutinee`).
    ///
    /// Withholding equations is a change to the set in force, and so is restoring them: what was read off the scope under the equations in force is set aside while `read` runs and handed back where they are restored, and what `read` read under the narrowed ones goes (`Memos::set_aside`). Withholding is `Scope::hide_refinements_from`'s to justify.
    pub(crate) fn withholding<T>(&mut self, index: usize, read: impl FnOnce(&mut Self) -> T) -> T {
        let outer = self.certifier.scope.hide_refinements_from(index);
        let aside = self.certifier.memos.set_aside(self.plain);
        let outcome = read(self);
        self.certifier.scope.show_refinements(outer);
        self.certifier.memos.hand_back(aside);

        outcome
    }

    /// Settle the reduced spelling of the equation at `index`, reducing `key` with that equation — and every equation inside it — withheld ([`ByValue::withholding`]).
    ///
    /// **The whole of what the two-tier key defers.** Recording an equation costs nothing; this is where the reduction happens — at most once per equation, and only because a probe presented a term the written spelling did not answer.
    ///
    /// An error settles the equation as having no reduced spelling, so the attempt is paid once rather than repeated at every later probe; the settlement is a [`Probe`], so exhaustion settles it the same way and propagates besides.
    pub(crate) fn settle_refinement(&mut self, index: usize, key: Term) -> Result<(), ReduceError> {
        // The reduced spelling is held operand-canonical, the form `refined_reduct` brings a probed value to before comparing — see `canonical_operands`.
        let reduct = self.withholding(index, |certifier| {
            certifier
                .whnf(key)
                .and_then(|reduct| canonical_operands(certifier, &reduct))
        });

        match reduct.probed() {
            Ok(settled) => {
                self.certifier
                    .scope
                    .settle_refinement(index, self.plain, settled);
                Ok(())
            }
            Err(spent) => {
                self.certifier
                    .scope
                    .settle_refinement(index, self.plain, None);
                Err(spent)
            }
        }
    }

    /// The case value `term` is refined to under a reduced spelling this reading's reduction already settled, innermost arm first (`Scope::refinement_of_reduct`).
    pub(crate) fn refinement_of_reduct(&self, term: &Term) -> Option<Term> {
        self.certifier.scope.refinement_of_reduct(term, self.plain)
    }

    /// The equations in force whose reduced spelling this reading's reduction settled and that `candidate` could be a reduct of, innermost first, each as its position, that spelling and its value.
    pub(crate) fn settled_refinements(
        &self,
        candidate: &Term,
        proofs: &[Free],
    ) -> Vec<(usize, Term, Term)> {
        self.certifier
            .scope
            .settled_refinements(candidate, proofs, self.plain)
    }

    /// The innermost equation in force whose reduced spelling this reading's reduction has not asked for and could be `candidate`, as its position and the term to reduce.
    pub(crate) fn unasked_refinement(
        &self,
        candidate: &Term,
        proofs: &[Free],
    ) -> Option<(usize, Term)> {
        self.certifier
            .scope
            .unasked_refinement(candidate, proofs, self.plain)
    }

    /// The remembered weak-head reduct of `term` this reading's reduction took, per entry point, handed back for nothing.
    ///
    /// The one hit that cannot fail, because it spends no steps and mints no identity: the certifier did not perform this computation, and charging it what a memo-free evaluator would have spent, in units or in identities, would run a judgment out on work nobody did. `Spend`'s module documentation and `Memos` state the two halves of why that is safe.
    pub(crate) fn whnf_hit(&self, term: &Term, forced: bool) -> Option<Term> {
        self.certifier.memos.whnf(term, forced, self.plain)
    }

    /// The remembered sort of `type_`, read by this reading's reduction, with nothing spent and nothing minted, as every hit is. A local-free type's, read with no case equation in force, is the declaration's; one read off the scope, for a type naming a local or under an equation, is taken only while the binders it was read under stand.
    pub(crate) fn sort_hit(&self, type_: &Term) -> Option<Sort> {
        self.certifier.standing(self.certifier.memos.sort(
            type_,
            self.certifier.has_refinements(),
            self.plain,
        ))
    }

    /// The remembered verdict of comparing `this` with `that` at `type_` by this reading's reduction, under the lives a typing has, with the pairs of heads its derivation unfolded. Only a verdict reached with no pair of heads left apart is ever filed, so it is the verdict wherever none of those pairs is being unfolded.
    pub(crate) fn convert_hit(&self, type_: &Term, this: &Term, that: &Term) -> Option<Converted> {
        self.certifier.standing(self.certifier.memos.converted(
            type_,
            this,
            that,
            self.certifier.has_refinements(),
            self.plain,
        ))
    }

    /// The binders `term` names that are themselves proofs: opened at a proposition, so conversion reads them nowhere, and a stuck form naming one the key of an equation does not may still be that key's term (`curios_analysis::could_reduce_to`).
    ///
    /// **Each binder is asked once**, where a stuck form naming it first reaches the equations in force, and remembered with the binder. It is asked by plain reduction, which no question interrupts, and counted not a proof while it is being asked, so a type whose classification comes back to its own binder ends there, toward refusal. A failure that is no exhaustion of the budget is that the binder is not known to be a proof.
    pub(crate) fn proofs_named(&mut self, term: &Term) -> Result<Vec<Free>, ReduceError> {
        let mut proofs = Vec::new();
        for name in term.mentions().locals() {
            let proof = match self.certifier.scope.classified(name) {
                Some(proof) => proof,
                None => {
                    let Some(type_) = self.certifier.scope.local_type(name).cloned() else {
                        continue;
                    };
                    self.certifier.scope.classify(name, false);
                    let sort = self.plainly(|certifier| certifier.sort_of(&type_));
                    let proof = sort.probed_refusal()?.is_some_and(|sort| sort.is_prop());
                    self.certifier.scope.classify(name, proof);
                    proof
                }
            };
            if proof {
                proofs.push(*name);
            }
        }
        Ok(proofs)
    }

    /// Charge `cost` to the budget ([`Certifier::spend`]).
    pub(crate) fn spend(&mut self, cost: Cost) -> Result<(), ReduceError> {
        self.certifier.spend(cost)
    }

    /// Enter one more level of a walk's depth ([`Certifier::enter_level`]).
    pub(crate) fn enter_level(&mut self) -> Result<(), ReduceError> {
        self.certifier.enter_level()
    }

    /// Leave the level [`ByValue::enter_level`] entered.
    pub(crate) fn leave_level(&mut self) {
        self.certifier.leave_level();
    }

    /// Open a binder that stands for nothing at `type_` ([`Certifier::assume`]): a reading opens only binders it minted.
    pub(crate) fn assume(&mut self, name: &Free, type_: &Term) {
        self.certifier.assume(name, type_);
    }

    /// Step `walk` past its next binder, minted and opened at `domain` ([`Certifier::advance_assumed`]).
    pub(crate) fn advance_assumed(&mut self, walk: &mut impl Advance, domain: &Term) -> Free {
        self.certifier.advance_assumed(walk, domain)
    }

    /// Remember a `term`'s weak-head reduct, which [`ByValue::whnf_hit`] hands back.
    ///
    /// **Stored for nothing.** `Memos::begin_declaration` clears the table exactly where `Spend::restore_budget` fires, and every node it holds was built under that budget, which charges a construction what it builds — so the budget that built an entry is its bound. Charging it besides, against a compilation-wide allowance at the tree footprint of key and reduct, would bill entries that die with the declaration, and bill them by their trees where a reduct is a graph whose tree has `2^n` nodes.
    pub(crate) fn whnf_store(&mut self, term: Term, forced: bool, reduct: Term) {
        self.certifier
            .memos
            .store_whnf(term, forced, self.plain, reduct);
    }

    /// Remember `type_`'s sort, beside the binders in scope now.
    pub(crate) fn sort_store(&mut self, type_: Term, sort: Sort) {
        let plain = self.plain;
        let certifier = &mut *self.certifier;
        certifier.memos.store_sort(
            type_,
            certifier.scope.has_refinements(),
            plain,
            certifier.scope.prefix(),
            sort,
        );
    }

    /// Remember the verdict of comparing `this` with `that` at `type_`, with the pairs of heads its derivation unfolded.
    pub(crate) fn convert_store(
        &mut self,
        type_: &Term,
        this: &Term,
        that: &Term,
        converted: Converted,
    ) {
        let plain = self.plain;
        let certifier = &mut *self.certifier;
        certifier.memos.store_converted(
            (type_.clone(), this.clone(), that.clone()),
            certifier.scope.has_refinements(),
            plain,
            certifier.scope.prefix(),
            converted,
        );
    }

    /// Remember what classifying the stuck operation `term` found, stored for nothing as a reduct is ([`ByValue::whnf_store`]).
    pub(crate) fn keep_classification(&mut self, term: Term, classification: Classification) {
        self.certifier
            .memos
            .store_classification(term, classification);
    }

    /// The closed machine's memo, which lives the declaration as the local-free reducts do.
    pub(crate) fn closed_memo_mut(&mut self) -> &mut ClosedMemo {
        self.certifier.memos.closed()
    }

    /// Begin a run of the closed machine (`Memos::begin_closed_run`).
    pub(crate) fn begin_closed_run(&mut self) -> Option<ClosedMemo> {
        self.certifier.memos.begin_closed_run()
    }

    /// End the run [`ByValue::begin_closed_run`] began (`Memos::end_closed_run`).
    pub(crate) fn end_closed_run(&mut self, outer: Option<ClosedMemo>) {
        self.certifier.memos.end_closed_run(outer);
    }
}

/// What a shared analysis a reading started may ask of it: the certifier's answers, and reduction that trusts what it is handed.
impl Env for ByValue<'_, '_> {
    type Error = Error;

    /// Plain reduction, as the seam asks, over this reading.
    fn force(&mut self, term: &Term) -> Result<Term, Self::Error> {
        Ok(self.plainly(|certifier| certifier.reduce_forced(term.clone()))?)
    }

    fn assumption(&self, name: &Free) -> Option<&Term> {
        Env::assumption(&**self, name)
    }

    fn is_local(&self, name: &Free) -> bool {
        Env::is_local(&**self, name)
    }

    fn fresh(&mut self, hint: Option<&str>) -> Free {
        Certifier::fresh(self, hint)
    }

    fn unfold(&self, name: &Free) -> Option<&Term> {
        Env::unfold(&**self, name)
    }

    fn body(&self, name: &Global) -> Option<&Term> {
        Env::body(&**self, name)
    }

    fn induct_decl(&self, name: &Global) -> Option<&InductDecl> {
        Env::induct_decl(&**self, name)
    }

    fn struct_decl(&self, name: &Global) -> Option<&StructDecl> {
        Env::struct_decl(&**self, name)
    }

    fn polarity(&self, name: &Global, index: usize) -> Polarity {
        Env::polarity(&**self, name, index)
    }

    fn judged(&self, name: &Global) -> bool {
        Env::judged(&**self, name)
    }

    fn variance(&self, name: &Global, index: usize) -> Variance {
        Env::variance(&**self, name, index)
    }

    fn applications(&self, name: &Global) -> Option<Applications> {
        Env::applications(&**self, name)
    }

    fn refined(&self) -> Vec<(Term, Term)> {
        Env::refined(&**self)
    }
}

/// The certifier's side of the shared-analysis seam, for an analysis typing started: its reduction a door ([`ByValue`]).
impl Env for Certifier<'_> {
    type Error = Error;

    /// Plain reduction, as the seam asks: a shared analysis borrows no opinion of the certifier's conversion through the term it reads. A door, so the term is read by value, in a reading whose reduction is plain.
    fn force(&mut self, term: &Term) -> Result<Term, Self::Error> {
        Ok(self.read_by_value([term], |certifier, [term]| {
            certifier.plainly(|certifier| certifier.reduce_forced(term))
        })?)
    }

    /// Plain reduction of the term as a graded call reads it: a door, the term read by value but for a variable an arm's index inversion solved, so a guard's operand and a call's argument read through a witness are read as the call names them.
    fn force_graded(&mut self, term: &Term) -> Result<Term, Self::Error> {
        Ok(self.read_graded([term], |certifier, [term]| {
            certifier.plainly(|certifier| certifier.reduce_forced(term))
        })?)
    }

    fn assumption(&self, name: &Free) -> Option<&Term> {
        self.local_type(name)
    }

    fn is_local(&self, name: &Free) -> bool {
        self.local_type(name).is_some()
    }

    fn fresh(&mut self, hint: Option<&str>) -> Free {
        Certifier::fresh(self, hint)
    }

    /// None: what the certifier hands a shared analysis it reads by value already — a door unfolded it, or the scope holds it so — but for a graded call's arguments, whose variables an arm's index inversion solved the size order reads as written (`Certifier::graded`).
    fn unfold(&self, _: &Free) -> Option<&Term> {
        None
    }

    fn body(&self, name: &Global) -> Option<&Term> {
        Certifier::body(self, name)
    }

    fn induct_decl(&self, name: &Global) -> Option<&InductDecl> {
        Certifier::induct_decl(self, name)
    }

    fn struct_decl(&self, name: &Global) -> Option<&StructDecl> {
        Certifier::struct_decl(self, name)
    }

    fn polarity(&self, name: &Global, index: usize) -> Polarity {
        Certifier::polarity(self, name, index)
    }

    /// Whether a walk filed the family's vector: the certifier registers every family of a module before it judges any, so one stored after the family judged is held and not judged.
    fn judged(&self, name: &Global) -> bool {
        Certifier::judged(self, name)
    }

    fn variance(&self, name: &Global, index: usize) -> Variance {
        Certifier::variance(self, name, index)
    }

    fn applications(&self, name: &Global) -> Option<Applications> {
        Certifier::applications(self, name)
    }

    /// The case equations of the arms in force, each keyed by value where it was recorded (`Certifier::refine`) and restated under an arm's solution (`Scope::restate`), so none names a `let`-bound local.
    fn refined(&self) -> Vec<(Term, Term)> {
        self.scope.refined()
    }
}
