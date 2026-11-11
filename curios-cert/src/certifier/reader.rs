//! A reader of certified terms: the certifier's own judgments, for a walk that reads a term this certifier accepted — erasure — and decides nothing of it.
//!
//! **Every entry is a judgment the certifier already makes.** Reduction to a head, as a judgment that takes a value apart reads it ([`Reader::head`]), a term's type ([`infer`]), which half a value of a type is in (the classification the erasure obligations are seeded by, [`erased_half`]), a case put in force as an arm is checked under it, the binders, the frame bracket, the lookups, the budget and what was read. No entry adds a rule, so a reader reads a type by the reduction that accepted the term, in the environment the walk that certified it built.
//!
//! **A reader records nothing of the walk that certified.** Inside [`Certifier::read`] no erased position and no call is recorded, so what the obligations read stays the certification's own. What its lookups and reductions read is noted as any judgment's is, for the reader to take ([`Reader::take_reads`]).

#[cfg(test)]
mod tests;

#[cfg(any(test, feature = "test-support"))]
mod test_support;

use {
    super::{Certifier, enter_arm, enter_case},
    crate::{Error, erased_half, infer},
    curios_analysis::Erased,
    curios_common::SyntaxRegistry,
    curios_core::{
        Atom, Cost, Free, Global, InductDecl, InductType, Reads, ReduceError, Reducer, StructDecl,
        Term, UniverseContext,
    },
};

/// The certifier, read through: what a reader of its certified terms may ask of it.
pub struct Reader<'k, 's> {
    certifier: &'k mut Certifier<'s>,
}

/// An arm a reader entered ([`Reader::enter_arm`]): the index targets its case aims at, the value its scrutinee stands for, and whether the indices rule the case out, in which case the certifier typed nothing of it.
pub struct EnteredArm {
    pub targets: Vec<Term>,
    pub value: Term,
    pub ruled_out: bool,
}

impl<'s> Certifier<'s> {
    /// Run `read` over a reader of this certifier's terms, recording no erased position and no call while it reads.
    ///
    /// **What it typed is remembered while it reads, and for no walk after it.** A typing made while reading recorded no position and graded no call, and a hit stands for every one its typing records, so a walk answered from one would leave the term's positions unconstrained and its groups' local gate unasked. The typings go where the outermost read ends (`Memos::discard_typings`), as a refused walk's do; inside it the reader keeps the memo that types a shared term once.
    pub fn read<T>(&mut self, read: impl FnOnce(&mut Reader<'_, 's>) -> T) -> T {
        let outer = self.positions.hold_for_reading(true);
        let outcome = read(&mut Reader { certifier: self });
        self.positions.hold_for_reading(outer);
        if !outer {
            self.memos.discard_typings();
        }

        outcome
    }
}

impl<'s> Reader<'_, 's> {
    /// The registry the certifier was made with.
    pub fn syntax(&self) -> SyntaxRegistry {
        self.certifier.syntax()
    }

    /// A binder no term in hand names.
    pub fn fresh(&self, hint: Option<&str>) -> Free {
        self.certifier.fresh(hint)
    }

    /// Charge `cost` to the certifier's budget.
    pub fn spend(&mut self, cost: Cost) -> Result<(), ReduceError> {
        self.certifier.spend(cost)
    }

    /// The whole budget again, as a declaration's judgment starts on ([`Certifier::restore_budget`]).
    pub fn restore_budget(&mut self) {
        self.certifier.restore_budget();
    }

    /// Assume the constraints `universes` states, as the certifier assumes a declaration's before it judges the declaration at each assignment of its sorts ([`Certifier::assume_universes`]).
    pub fn assume_universes(&mut self, universes: &UniverseContext) {
        self.certifier.assume_universes(universes);
    }

    /// Run `read` in a frame of its own: every binder it opens and every case it puts in force is retracted when it returns, as the certifier's every judgment brackets its own (`Certifier::scoped`).
    pub fn scoped<T>(&mut self, read: impl FnOnce(&mut Reader<'_, 's>) -> T) -> T {
        self.certifier
            .scoped(|certifier| read(&mut Reader { certifier }))
    }

    /// Open a binder that stands for nothing at `type_` ([`Certifier::assume`]).
    pub fn assume(&mut self, name: &Free, type_: &Term) {
        self.certifier.assume(name, type_);
    }

    /// Open a binder that stands for `value`, which has `type_`, as a `let` binds it (`Certifier::bind`).
    pub fn bind(&mut self, name: &Free, type_: &Term, value: &Term) {
        self.certifier.bind(name, type_, value, false);
    }

    /// `term` reduced to its head as a judgment that takes it apart reads it ([`Reducer::reduce_forced`]): a recursive call unfolded where it is productive, as typing reads a scrutinee's type or a function's.
    pub fn head(&mut self, term: &Term) -> Result<Term, ReduceError> {
        self.certifier.reduce_forced(term.clone())
    }

    /// The type of `term` ([`infer`]), a type former's being its sort.
    pub fn infer(&mut self, term: &Term) -> Result<Term, Error> {
        infer(self.certifier, term)
    }

    /// Which half a value of `type_` is in — a proof, a type, or neither, `None` — as the erasure obligations classify a position, remembered beside the certifier's sorts.
    pub fn erased(&mut self, type_: &Term) -> Result<Option<Erased>, Error> {
        let certifier = &mut *self.certifier;
        let type_ = certifier.by_value(type_);
        let in_arm = certifier.has_refinements();
        if let Some(erased) = certifier.standing(certifier.memos.half(&type_, in_arm)) {
            return Ok(erased);
        }

        let erased = erased_half(certifier, &type_)?;
        let prefix = certifier.scope.prefix();
        certifier.memos.store_half(type_, in_arm, prefix, erased);

        Ok(erased)
    }

    /// Put the case `value` of `scrutinee` in force, as an arm of a `Bool` or `Switch` elimination is checked under it: the scrutinee read by value, as typing reads a match's ([`infer`]), so a case equation names no `let`-bound local.
    pub fn case(&mut self, scrutinee: &Term, value: &Term) -> Result<(), Error> {
        let (scrutinee, value) = (
            self.certifier.by_value(scrutinee),
            self.certifier.by_value(value),
        );
        enter_case(self.certifier, &scrutinee, &value)
    }

    /// Enter the arm of `tag` in an elimination of `scrutinee`, a value of `family`, with its payload under `binders`, as the arm is checked: the scrutinee read by value, as [`Reader::case`] reads it.
    pub fn enter_arm(
        &mut self,
        family: &InductType,
        scrutinee: &Term,
        tag: &Atom,
        binders: &[Free],
    ) -> Result<EnteredArm, Error> {
        let scrutinee = self.certifier.by_value(scrutinee);
        enter_arm(self.certifier, family, &scrutinee, tag, binders)
    }

    /// The `induct` declaration `name`, noted as read.
    pub fn induct_decl(&self, name: &Global) -> Option<&InductDecl> {
        self.certifier.induct_decl(name)
    }

    /// The `struct` declaration `name`, noted as read.
    pub fn struct_decl(&self, name: &Global) -> Option<&StructDecl> {
        self.certifier.struct_decl(name)
    }

    /// What the reader's lookups and reductions have read since the last take.
    pub fn take_reads(&mut self) -> Reads {
        self.certifier.take_reads()
    }
}
