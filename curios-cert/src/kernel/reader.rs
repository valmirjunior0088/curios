//! A reader of certified terms: the kernel's own judgments, for a walk that reads a term this kernel accepted — erasure — and decides nothing of it.
//!
//! **Every entry is a judgment the kernel already makes.** Reduction to a head ([`whnf()`]), a term's type ([`infer`]), which half a value of a type is in (the classification the erasure obligations are seeded by, [`erased_half`]), a case put in force as an arm is checked under it, the binders, the frame bracket, the lookups, the budget and what was read. No entry adds a rule, so a reader reads a type by the reduction that accepted the term, and what certification reduced, sorted and classified is remembered for it.
//!
//! **A reader records nothing of the walk that certified.** Inside [`Kernel::read`] no erased position and no call is recorded, so what the obligations read stays the certification's own. What its lookups and reductions read is noted as any judgment's is, for the reader to take ([`Reader::take_reads`]).

#[cfg(test)]
mod tests;

use {
    super::{Kernel, enter_arm, enter_case, whnf},
    crate::{Error, erased_half, infer},
    curios_analysis::Erased,
    curios_core::{
        Atom, Cost, Free, Global, InductDecl, InductType, Reads, ReduceError, StructDecl, Term,
    },
    curios_utilities::SyntaxRegistry,
};

/// The kernel, read through: what a reader of its certified terms may ask of it.
pub struct Reader<'k, 's> {
    kernel: &'k mut Kernel<'s>,
}

/// An arm a reader entered ([`Reader::enter_arm`]): the index targets its case aims at, the value its scrutinee stands for, and whether the indices rule the case out, in which case the kernel typed nothing of it.
pub struct EnteredArm {
    pub targets: Vec<Term>,
    pub value: Term,
    pub ruled_out: bool,
}

impl<'s> Kernel<'s> {
    /// Run `read` over a reader of this kernel's terms, recording no erased position and no call while it reads.
    pub fn read<T>(&mut self, read: impl FnOnce(&mut Reader<'_, 's>) -> T) -> T {
        let outer = self.positions.hold_for_reading(true);
        let outcome = read(&mut Reader { kernel: self });
        self.positions.hold_for_reading(outer);

        outcome
    }
}

impl<'s> Reader<'_, 's> {
    /// The registry the kernel was made with.
    pub fn syntax(&self) -> SyntaxRegistry {
        self.kernel.syntax()
    }

    /// A binder no term in hand names.
    pub fn fresh(&self, hint: Option<&str>) -> Free {
        self.kernel.fresh(hint)
    }

    /// Charge `cost` to the kernel's budget.
    pub fn spend(&mut self, cost: Cost) -> Result<(), ReduceError> {
        self.kernel.spend(cost)
    }

    /// The whole budget again, as a declaration's judgment starts on ([`Kernel::restore_budget`]).
    pub fn restore_budget(&mut self) {
        self.kernel.restore_budget();
    }

    /// Run `read` in a frame of its own: every binder it opens and every case it puts in force is retracted when it returns, as the kernel's every judgment brackets its own (`Kernel::scoped`).
    pub fn scoped<T>(&mut self, read: impl FnOnce(&mut Reader<'_, 's>) -> T) -> T {
        self.kernel.scoped(|kernel| read(&mut Reader { kernel }))
    }

    /// Open a binder that stands for nothing at `type_` ([`Kernel::assume`]).
    pub fn assume(&mut self, name: &Free, type_: &Term) {
        self.kernel.assume(name, type_);
    }

    /// Open a binder that stands for `value`, which has `type_`, as a `let` binds it (`Kernel::bind`).
    pub fn bind(&mut self, name: &Free, type_: &Term, value: &Term) {
        self.kernel.bind(name, type_, value, false);
    }

    /// `term` reduced to its head, by the reduction a judgment of the kernel's runs.
    pub fn whnf(&mut self, term: &Term) -> Result<Term, ReduceError> {
        whnf(self.kernel, term.clone())
    }

    /// The type of `term` ([`infer`]), a type former's being its sort.
    pub fn infer(&mut self, term: &Term) -> Result<Term, Error> {
        infer(self.kernel, term)
    }

    /// Which half a value of `type_` is in — a proof, a type, or neither, `None` — as the erasure obligations classify a position, remembered beside the kernel's sorts.
    pub fn erased(&mut self, type_: &Term) -> Result<Option<Erased>, Error> {
        let kernel = &mut *self.kernel;
        let type_ = kernel.by_value(type_);
        let in_arm = kernel.has_refinements();
        if let Some(erased) = kernel.standing(kernel.memos.half(&type_, in_arm)) {
            return Ok(erased);
        }

        let erased = erased_half(kernel, &type_)?;
        let prefix = kernel.scope.prefix();
        kernel.memos.store_half(type_, in_arm, prefix, erased);

        Ok(erased)
    }

    /// Put the case `value` of `scrutinee` in force, as an arm of a `Bool` or `Switch` elimination is checked under it.
    pub fn case(&mut self, scrutinee: &Term, value: &Term) -> Result<(), Error> {
        enter_case(self.kernel, scrutinee, value)
    }

    /// Enter the arm of `tag` in an elimination of `scrutinee`, a value of `family`, with its payload under `binders`, as the arm is checked.
    pub fn enter_arm(
        &mut self,
        family: &InductType,
        scrutinee: &Term,
        tag: &Atom,
        binders: &[Free],
    ) -> Result<EnteredArm, Error> {
        enter_arm(self.kernel, family, scrutinee, tag, binders)
    }

    /// The `induct` declaration `name`, noted as read.
    pub fn induct_decl(&self, name: &Global) -> Option<&InductDecl> {
        self.kernel.induct_decl(name)
    }

    /// The `struct` declaration `name`, noted as read.
    pub fn struct_decl(&self, name: &Global) -> Option<&StructDecl> {
        self.kernel.struct_decl(name)
    }

    /// The body of the definition `name`, noted as read.
    pub fn value(&self, name: &Free) -> Option<&Term> {
        self.kernel.value_at(name)
    }

    /// What the reader's lookups and reductions have read since the last take.
    pub fn take_reads(&mut self) -> Reads {
        self.kernel.take_reads()
    }
}
